# =======================
#  Tariff processing (HTS8, quarterly)
#  - Build quarterly HTS8 schedules from USITC
#  - Merge GTA shifters & overrides
#  - Write partitioned parquet (year/qtr)
# =======================

# --- Packages ---
library(arrow)
library(countrycode)
library(readr)
library(tidyverse)
library(lubridate)
library(matrixStats)

# --- Small helpers ---
pad8    <- function(x) stringr::str_pad(x, 8, pad = "0")
qstarts <- function(y) ymd(paste0(rep(y, each = 4), c("-01-01","-04-01","-07-01","-10-01")))
warn_once <- local({
  seen <- FALSE
  function(msg) { if (!seen) { warning(msg, call. = FALSE); seen <<- TRUE } }
})

# --- External helpers & reference data ---
source("code/tariff_functions.R")  # mfn_rates(), pref_rates(), col2_rates(), fix_commodity(), hts8_to_hs07()
countries   <- read_csv("data/temp/countries.csv",             show_col_types = FALSE)  # i_iso3
unit_values <- read_csv("data/temp/unit_values_2010_2012.csv", show_col_types = FALSE)  # AVE conversion

# --- USITC reader (as-of quarter start) ---
read_usitc_asof <- function(y, asof) {
  f <- sprintf("data/raw/tariffs_usitc/tariff_database_%d.txt", y)
  delim <- if_else(y > 2016, ",", "|")
  
  # infer types once per file (cheap) — character for flags/codes, numeric for rates
  cols0 <- colnames(read_delim(f, delim = delim, show_col_types = FALSE))
  types <- tibble(name = cols0) |>
    mutate(txt = str_detect(name, "_ind$|indicator$|rate_type_code$|additional_duty$"),
           num = str_detect(name, "^ad_val_rate$|^specific_rate$|^other_rate$"),
           t   = case_when(txt ~ "c", num ~ "n", TRUE ~ "?")) |>
    pull(t) |> paste(collapse = "")
  dt <- read_delim(f, delim = delim, col_types = types, show_col_types = FALSE)
  
  # Normalize dates (two USITC formats observed)
  if (y %in% c(2019:2022, 2024:2025)) {
    dt <- mutate(dt,
                 begin_effect_date  = mdy(begin_effect_date),
                 end_effective_date = mdy(end_effective_date)
    )
  } else {
    strip_time <- function(x) mdy(substr(x, 1, nchar(x) - 8))
    dt <- mutate(dt,
                 begin_effect_date  = strip_time(begin_effect_date),
                 end_effective_date = strip_time(end_effective_date)
    )
  }
  
  dt |>
    filter(begin_effect_date <= asof, end_effective_date > asof) |>
    mutate(hts8 = if_else(nchar(as.character(hts8)) == 7, paste0("0", hts8), as.character(hts8)))
}

# --- Preference AVEs (min across available indicators) ---
pref_aves <- function(usitc, uvs, country_iso) {
  ind_cols <- usitc |> select(matches("(_ind$|indicator$)"))
  indicators <- ind_cols |> pivot_longer(everything(), values_to = "indicator") |>
    pull(indicator) |> unique() |> na.omit() |> sort()
  
  shell <- expand_grid(hts8 = unique(usitc$hts8), i_iso3 = country_iso)
  if (!length(indicators))
    return(transmute(shell, hts8, i_iso3, pref_ave = NA_real_))
  
  prefs <- map(indicators, ~ pref_rates(usitc, uvs, indicator = .x)) |>
    reduce(left_join, by = c("hts8","i_iso3"), .init = shell)
  
  pref_names <- setdiff(names(prefs), c("hts8","i_iso3"))
  if (!length(pref_names))
    return(transmute(prefs, hts8, i_iso3, pref_ave = NA_real_))
  
  pref_mat <- prefs |> select(all_of(pref_names)) |> mutate(across(everything(), as.numeric)) |> as.matrix()
  
  prefs |>
    mutate(pref_ave = matrixStats::rowMins(pref_mat, na.rm = TRUE),
           pref_ave = if_else(is.infinite(pref_ave), NA_real_, pref_ave)) |>
    select(hts8, i_iso3, pref_ave)
}

# --- Compile one quarter (HTS8 x partner) baseline schedule ---
compile_quarter_hts8 <- function(y, q_start, countries, unit_values) {
  usitc <- read_usitc_asof(y, q_start)
  
  # Pull unit values (both quantity codes) for AVE conversions
  uvs <- usitc |>
    select(hts8, quantity_1_code, quantity_2_code) |>
    left_join(unit_values |> rename(quantity_1_code = quantity_code, unit_value_1 = unit_value),
              by = c("hts8","quantity_1_code")) |>
    left_join(unit_values |> rename(quantity_2_code = quantity_code, unit_value_2 = unit_value),
              by = c("hts8","quantity_2_code")) |>
    select(hts8, unit_value_1, unit_value_2)
  
  mfns      <- mfn_rates(usitc, uvs)                              # -> hts8, i_iso3, mfn_ave
  pref_ave  <- pref_aves(usitc, uvs, countries$i_iso3)            # -> hts8, i_iso3, pref_ave
  col2_full <- col2_rates(usitc, uvs, y) |> select(hts8, i_iso3, col2_ave)
  col2_hts8 <- col2_full |> select(hts8, col2_ave) |> distinct()
  col2_iso  <- if (nrow(col2_full)) unique(col2_full$i_iso3) else character(0)
  
  # Merge + choose baseline applied rate (min of MFN & prefs; COL2 overrides by country)
  out <- mfns |>
    left_join(pref_ave,  by = c("hts8","i_iso3")) |>
    left_join(col2_hts8, by = "hts8") |>
    mutate(
      base_min    = pmin(mfn_ave, pref_ave, na.rm = TRUE),
      base_min    = if_else(is.infinite(base_min), NA_real_, base_min),
      applied_ave = if_else(i_iso3 %in% col2_iso, col2_ave, base_min)
    ) |>
    transmute(
      year    = year(q_start),
      qtr     = quarter(q_start),
      yearq   = paste0(year, "Q", qtr),
      quarter = floor_date(q_start, "quarter"),
      i_iso3, hts8, applied_ave, mfn_ave, pref_ave, col2_ave
    )
  
  # Sanity: zero-pad + non-missing keys
  out <- mutate(out, hts8 = pad8(hts8))
  stopifnot(!any(is.na(out$quarter)), !any(is.na(out$hts8)))
  out
}

# ---------- Quarters to build ----------
years <- 2013:2025
qs <- qstarts(years)
qs <- qs[ year(qs) < 2025 | quarter(qs) <= 2 ]  # through 2025Q2

# ---------- GTA shifters & overrides (pre-agg to quarter × iso × HTS8) ----------
shifters2 <- read_parquet("data/temp/shifters.parquet") |>
  transmute(
    i_iso3  = countrycode(partner, "country.name", "iso3c"),
    hts8    = pad8(product_code),
    quarter = as.Date(quarter),
    shifter_adval
  ) |>
  group_by(i_iso3, hts8, quarter) |>
  summarise(shifter_adval = sum(shifter_adval, na.rm = TRUE), .groups = "drop")

overrides2 <- read_parquet("data/temp/overrides.parquet") |>
  transmute(
    i_iso3  = countrycode(partner, "country.name", "iso3c"),
    hts8    = pad8(hts8),
    quarter = as.Date(quarter),
    ov_share = pmin(1, pmax(0, ov_share)),
    ov_rate
  )

# ---------- Output ----------
out_quarter_dir <- "data/processed/tariffs_hts8_quarterly"
if (dir.exists(out_quarter_dir)) unlink(out_quarter_dir, recursive = TRUE)

walk(qs, function(qdt) {
  y <- year(qdt)
  message("[quarter] ", qdt)
  
  # Baseline schedule
  base_q <- compile_quarter_hts8(y, qdt, countries, unit_values)
  
  # Merge GTA adjustments for this quarter only
  s_q <- filter(shifters2,  quarter == qdt)
  o_q <- filter(overrides2, quarter == qdt)
  
  out_q <- base_q |>
    rename(applied_ave_baseline = applied_ave) |>
    left_join(s_q, by = c("i_iso3","hts8","quarter")) |>
    mutate(
      shifter_adval       = coalesce(shifter_adval, 0),
      applied_plus_shift  = coalesce(applied_ave_baseline, 0) + shifter_adval
    ) |>
    left_join(o_q, by = c("i_iso3","hts8","quarter")) |>
    mutate(
      applied_final = if_else(!is.na(ov_rate) & !is.na(ov_share),
                              ov_share * ov_rate + (1 - ov_share) * applied_plus_shift,
                              applied_plus_shift)
    ) |>
    transmute(
      year, qtr, yearq, quarter, i_iso3, hts8,
      applied_ave = applied_final,          # final after GTA adjustments
      schedule_ave = applied_ave_baseline,  # baseline (pre-GTA)
      mfn_ave, pref_ave, col2_ave
    )
  
  write_dataset(
    out_q,
    path = out_quarter_dir,
    format = "parquet",
    partitioning = c("year","qtr"),
    existing_data_behavior = "overwrite"
  )
  
  rm(base_q, s_q, o_q, out_q); gc()
})
