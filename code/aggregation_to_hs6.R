# ===========================
# HTS8 --> HS6 (weighted + simple) --> HS6-2012 (weighted + simple)
# ===========================

library(tidyverse)
library(arrow)
library(lubridate)
library(stringr)

# ---------- Paths ----------
hts8_dir   <- "data/processed/tariffs_hts8_quarterly"          # input parquet files
weights_fp <- "data/temp/weights.csv"                          # base weights: hs6, hts8, s_h8_in_h6
cor2017_fp <- "data/temp/correl_hs6_2017_to_2012.csv"          # hs6_from, hs6_to, relationship
cor2022_fp <- "data/temp/correl_hs6_2022_to_2012.csv"          # hs6_from, hs6_to, relationship
out_dir    <- "data/processed/tariffs_hs6_2012_quarterly"

# ---------- Load base HTS8->HS6 weights (may be sparse for later years) ----------
# expected columns: hs6, hts8, s_h8_in_h6 (shares within HS6; will renormalise per group/quarter)
weights_base <- read_csv(
  weights_fp,
  col_types = cols(
    hs6 = col_character(),
    hts8 = col_character(),
    s_h8_in_h6 = col_double(),
    .default = col_guess()
  )
) %>%
  mutate(
    hs6  = str_pad(hs6,  6, pad = "0"),
    hts8 = str_pad(hts8, 8, pad = "0")
  ) %>%
  select(hs6, hts8, s_h8_in_h6)

# ---------- Helper: turn a correl table with 'relationship' into shares ----------
# Rules:
# - 1:1  -> share = 1
# - n:1  -> share = 1 (each source maps fully to a single target)
# - 1:n  -> equal split across the multiple hs6_to for that hs6_from
# - n:n  -> equal split across the multiple hs6_to for that hs6_from
make_correl_with_shares <- function(path, hs_version_label) {
  read_csv(path,
           col_types = cols(
             hs6_from     = col_character(),
             hs6_to       = col_character(),
             relationship = col_character()
           )) %>%
    mutate(
      hs6_from     = str_pad(str_replace_all(hs6_from, "\\s+", ""), 6, pad = "0"),
      hs6_to       = str_pad(str_replace_all(hs6_to,   "\\s+", ""), 6, pad = "0"),
      relationship = str_trim(relationship),
      hs_version   = hs_version_label
    ) %>%
    group_by(hs6_from) %>%
    mutate(
      n_targets = n(),
      share = case_when(
        relationship == "1:1" ~ 1,
        relationship == "n:1" ~ 1,
        relationship %in% c("1:n", "n:n") ~ 1 / n_targets,
        TRUE ~ 1 / n_targets  # safe fallback
      )
    ) %>%
    ungroup() %>%
    select(hs_version, hs6_from, hs6_to, share)
}

# ---------- Build correlation tables with shares ----------
cor2017 <- make_correl_with_shares(cor2017_fp, "HS2017")
cor2022 <- make_correl_with_shares(cor2022_fp, "HS2022")
correls <- bind_rows(cor2017, cor2022)

# Check correls shares sum to 1
correls %>%
  group_by(hs_version, hs6_from) %>%
  summarise(s = sum(share), .groups = "drop") %>%
  filter(abs(s - 1) > 1e-9) %>%
  { if (nrow(.) > 0) warning("Some hs6_from groups do not sum to 1 in correls") }

# ---------- Helper: determine HS version from date ----------
hs_version_for_date <- function(d) {
  d <- as_date(d)
  case_when(
    d <  ymd("2017-01-01") ~ "HS2012",
    d <  ymd("2022-01-01") ~ "HS2017",
    TRUE                   ~ "HS2022"
  )
}

# ---------- Helper: aggregate one parquet file ----------
# Expects parquet with columns at least:
# i_iso3, quarter (date-like), hts8, applied_ave, schedule_ave
aggregate_hts8_file <- function(file_path) {
  df <- read_parquet(file_path) %>%
    mutate(
      quarter       = as_date(quarter),
      year          = year(quarter),
      qtr           = quarter(quarter),
      hts8          = str_pad(hts8, 8, pad = "0"),
      hs6           = substr(hts8, 1, 6),
      hs_version    = hs_version_for_date(quarter)
    )
  
  # bring in base HTS8->HS6 shares; may be missing for some HTS8
  dfw <- df %>%
    left_join(weights_base, by = c("hs6", "hts8"))
  
  # --- HTS8 -> HS6 per (i_iso3, hs6, quarter)
  hs6_agg <- dfw %>%
    group_by(i_iso3, hs6, hs_version, quarter, year, qtr) %>%
    summarise(
      n_hts8 = sum(!is.na(applied_ave)),
      
      # weighted (renormalize to observed lines)
      wsum_base = sum(s_h8_in_h6[!is.na(applied_ave) & !is.na(s_h8_in_h6)], na.rm = TRUE),
      tariff_weighted_hs6 = if_else(
        wsum_base > 0,
        sum(applied_ave * replace_na(s_h8_in_h6, 0), na.rm = TRUE) / wsum_base,
        NA_real_
      ),
      schedule_weighted_hs6 = if_else(
        wsum_base > 0,
        sum(schedule_ave * replace_na(s_h8_in_h6, 0), na.rm = TRUE) / wsum_base,
        NA_real_
      ),
      
      # simple averages (independent)
      tariff_simple_hs6   = if_else(any(!is.na(applied_ave)),  mean(applied_ave,  na.rm = TRUE), NA_real_),
      schedule_simple_hs6 = if_else(any(!is.na(schedule_ave)), mean(schedule_ave, na.rm = TRUE), NA_real_),
      
      .groups = "drop"
    )
  
  # --- HS6 (contemporaneous) -> HS6-2012 using correlation tables
  # For HS2012 rows: identity mapping
  hs6_2012_from_2012 <- hs6_agg %>%
    filter(hs_version == "HS2012") %>%
    mutate(hs6_2012 = hs6) %>%
    transmute(
      i_iso3, quarter, year, qtr, hs6 = hs6_2012,
      tariff_weighted = tariff_weighted_hs6,
      schedule_weighted = schedule_weighted_hs6,
      tariff_simple = tariff_simple_hs6,
      schedule_simple = schedule_simple_hs6,
      n_hts8
    )
  
  # HS2017 / HS2022 rows: map via correl tables (shares computed from 'relationship')
  hs6_non2012 <- hs6_agg %>% filter(hs_version != "HS2012")
  
  if (nrow(hs6_non2012) > 0) {
    hs6_mapped <- hs6_non2012 %>%
      left_join(
        correls %>% select(hs_version, hs6_from, hs6_to, share),
        by = c("hs_version", "hs6" = "hs6_from")
      ) %>%
      # if you'd prefer identity fallback for unmapped, replace the filter with:
      # mutate(hs6_to = coalesce(hs6_to, hs6), share = coalesce(share, 1))
      filter(!is.na(hs6_to), !is.na(share)) %>%
      group_by(i_iso3, quarter, year, qtr, hs6_to) %>%
      summarise(
        tariff_weighted = sum(tariff_weighted_hs6 * share, na.rm = TRUE) / sum(share, na.rm = TRUE),
        schedule_weighted = sum(schedule_weighted_hs6 * share, na.rm = TRUE) / sum(share, na.rm = TRUE),
        tariff_simple   = sum(tariff_simple_hs6   * share, na.rm = TRUE) / sum(share, na.rm = TRUE),
        schedule_simple = sum(schedule_simple_hs6 * share, na.rm = TRUE) / sum(share, na.rm = TRUE),
        n_hts8 = sum(n_hts8, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(hs6 = hs6_to)
  } else {
    hs6_mapped <- tibble(
      i_iso3 = character(), quarter = as_date(character()),
      year = integer(), qtr = integer(), hs6 = character(),
      tariff_weighted = double(), schedule_weighted = double(),
      tariff_simple = double(), schedule_simple = double(),
      n_hts8 = integer()
    )
  }
  
  bind_rows(hs6_2012_from_2012, hs6_mapped) %>%
    arrange(i_iso3, quarter, hs6)
}

# ---------- Gather and process all parquet files ----------
files <- list.files(hts8_dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
message("Found ", length(files), " parquet files to process.")

all_hs6_2012 <- purrr::map_dfr(files, ~{
  message("Processing: ", .x)
  aggregate_hts8_file(.x)
})

# ---------- Write output ----------
if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)

write_dataset(
  all_hs6_2012,
  path = out_dir,
  format = "parquet",
  partitioning = c("year", "qtr"),
  existing_data_behavior = "overwrite"
)

# Examine how many weighted observations vs total
coverage <- all_hs6_2012 %>%
  summarise(
    n_weighted = sum(!is.na(tariff_weighted)),
    n_simple   = sum(!is.na(tariff_simple)),
    n_total    = n(),
    pct_weighted = n_weighted / n_total
  )

message("✅ Done! HS6-2012 quarterly tariffs (weighted + simple) written to: ", out_dir)