# ==================== Setup ====================
library(arrow)
library(tidyverse)
library(lubridate)

# --- Small helpers ---
norm6 <- function(x) str_pad(str_sub(str_replace_all(as.character(x), "[^0-9]", ""), 1, 6), 6, pad = "0")
norm8 <- function(x) str_pad(str_sub(str_replace_all(as.character(x), "[^0-9]", ""), 1, 8), 8, pad = "0")
only_digits <- function(x) str_replace_all(as.character(x), "[^0-9]", "")
DATE_2017 <- as.Date("2017-01-01")
DATE_2022 <- as.Date("2022-01-01")

# ==================== Inputs on disk ====================
#   - h2012: hs6_2012 -> hts8_2012
#   - h2017: hs6_2017 -> hts8_2017
#   - h2022: hs6_2022 -> hts8_2022
#   - c17_12: hs6_2017 -> hs6_2012
#   - cc67  : hs6_2022 -> hs6_2017

h2012 <- read_csv("data/temp/hs_tree_2012.csv",
                  col_types = cols(hs6_2012 = col_character(), hts8_2012 = col_character())) |>
  transmute(hs6_2012 = norm6(hs6_2012), hts8_2012 = norm8(hts8_2012)) |>
  distinct()

h2017 <- read_csv("data/temp/hs_tree_2017.csv",
                  col_types = cols(hs6_2017 = col_character(), hts8_2017 = col_character())) |>
  transmute(hs6_2017 = norm6(hs6_2017), hts8_2017 = norm8(hts8_2017)) |>
  distinct()

h2022 <- read_csv("data/temp/hs_tree_2022.csv",
                  col_types = cols(hs6_2022 = col_character(), hts8_2022 = col_character())) |>
  transmute(hs6_2022 = norm6(hs6_2022), hts8_2022 = norm8(hts8_2022)) |>
  distinct()

c17_12 <- read_csv("data/temp/concord_hs6_2017_to_2012.csv") |>
  transmute(hs6_2017 = norm6(hs6_2017), hs6_2012 = norm6(hs6_2012)) |>
  distinct()

cc67 <- read_csv("data/temp/concord_hs6_2022_to_2017.csv") |>
  transmute(hs6_2022 = norm6(hs6_2022), hs6_2017 = norm6(hs6_2017)) |>
  distinct()

# ==================== Measures ====================
# Expected columns (others ignored):
# id, partners, start_date, end_date, program, effect, rate_type, rate_value, hs_revision, hs_scope_1/hs_scope_2
measures_raw <- read_csv("data/temp/measures.csv", show_col_types = FALSE)

# --- Parse HS scope strings into rows of (level, code) ---
expand_scope_string <- function(scope) {
  if (is.na(scope) || scope == "") return(tibble(level = character(), code = character()))
  
  scope  <- str_replace_all(scope, "[\u2012\u2013\u2014\u2015]", "-")  # normalize dashes
  header <- case_when(
    str_detect(scope, "^\\s*hs6\\s*:") ~ "hs6",
    str_detect(scope, "^\\s*hs8\\s*:") ~ "hts8",
    TRUE                               ~ "hts8"
  )
  body   <- str_replace(scope, "^\\s*hs(6|8)\\s*:\\s*", "")
  parts  <- str_split(body, "\\s*[,;]\\s*")[[1]]
  
  map_dfr(parts, function(s) {
    s <- str_trim(s)
    if (str_detect(s, "-")) {
      lo <- only_digits(str_extract(s, "^[0-9]+"))
      hi <- only_digits(str_extract(s, "[0-9]+$"))
      stopifnot(nchar(lo) == nchar(hi))
      w <- nchar(lo)
      tibble(level = header,
             code  = sprintf(paste0("%0", w, "d"), seq.int(as.integer(lo), as.integer(hi))))
    } else {
      tibble(level = header, code = only_digits(s))
    }
  }) |>
    distinct()
}

# --- Tidy measures ---
measures <- measures_raw |>
  rename_with(~ make.unique(tolower(trimws(gsub("\\s+", "_", .x))))) |>
  mutate(
    id          = as.character(id),
    partners    = as.character(partners),
    program     = tolower(as.character(program)),
    effect      = tolower(as.character(effect)),
    rate_type   = tolower(as.character(rate_type)),
    start_date  = ymd(as.character(start_date), quiet = TRUE),
    end_date    = ymd(as.character(end_date),   quiet = TRUE),
    hs_revision = toupper(as.character(hs_revision)),
    rate_value  = suppressWarnings(as.numeric(rate_value))
  )

# --- Expand a single measure row into regime-scoped HTS/HS columns ---
expand_one_row <- function(row) {
  # tiny getter (keeps scalar strings)
  gv <- function(nm) {
    v <- row[[nm]]
    if (is.null(v)) return(NA)
    if (length(v)) v[[1]] else NA
  }
  
  scopes <- bind_rows(expand_scope_string(gv("hs_scope_1")),
                      expand_scope_string(gv("hs_scope_2"))) |>
    distinct()
  if (nrow(scopes) == 0) return(tibble())
  
  start_d <- as.Date(gv("start_date"))
  # Default hs_revision from start_date if missing
  rev <- if (is.na(gv("hs_revision")) || gv("hs_revision") == "") {
    if (!is.na(start_d) && start_d >= DATE_2022) "HS2022" else "HS2017"
  } else {
    gv("hs_revision")
  }
  
  src_hs6  <- scopes |> filter(level == "hs6")  |> transmute(hs6  = norm6(code))
  src_hts8 <- scopes |> filter(level == "hts8") |> transmute(hts8 = norm8(code))
  
  base <- tibble(
    id         = as.character(gv("id")),
    partners   = as.character(gv("partners")),
    start_date = start_d,
    end_date   = as.Date(gv("end_date")),
    effect     = as.character(gv("effect")),
    rate_type  = as.character(gv("rate_type")),
    rate_value = suppressWarnings(as.numeric(gv("rate_value"))),
    program    = as.character(gv("program"))
  )
  
  if (start_d < DATE_2017) {
    # ---- HS2012 regime (pre-2017) ----
    d1 <- if (nrow(src_hts8)) tidyr::crossing(base, src_hts8) |>
      transmute(!!!base, hts8_2012 = hts8, hs6_2012 = substr(hts8, 1, 6)) else tibble()
    
    d2 <- if (nrow(src_hs6) && rev == "HS2017") src_hs6 |>
      rename(hs6_2017 = hs6) |>
      inner_join(c17_12, by = "hs6_2017") |>
      inner_join(h2012,  by = "hs6_2012") |>
      tidyr::crossing(base) else tibble()
    
    d3 <- if (nrow(src_hs6) && rev == "HS2022") src_hs6 |>
      rename(hs6_2022 = hs6) |>
      inner_join(cc67,   by = "hs6_2022") |>
      inner_join(c17_12, by = "hs6_2017") |>
      inner_join(h2012,  by = "hs6_2012") |>
      tidyr::crossing(base) else tibble()
    
    bind_rows(d1, d2, d3) |>
      mutate(hts8_2017 = NA_character_, hs6_2017 = NA_character_,
             hts8_2022 = NA_character_, hs6_2022 = NA_character_)
  } else if (start_d < DATE_2022) {
    # ---- HS2017 regime (2017–2021) ----
    d1 <- if (nrow(src_hts8)) tidyr::crossing(base, src_hts8) |>
      transmute(!!!base, hts8_2017 = hts8, hs6_2017 = substr(hts8, 1, 6)) else tibble()
    
    d2 <- if (nrow(src_hs6) && rev == "HS2017") src_hs6 |>
      rename(hs6_2017 = hs6) |>
      inner_join(h2017, by = "hs6_2017") |>
      tidyr::crossing(base) else tibble()
    
    d3 <- if (nrow(src_hs6) && rev == "HS2022") src_hs6 |>
      rename(hs6_2022 = hs6) |>
      inner_join(cc67,  by = "hs6_2022") |>
      inner_join(h2017, by = "hs6_2017") |>
      tidyr::crossing(base) else tibble()
    
    bind_rows(d1, d2, d3) |>
      mutate(hts8_2012 = NA_character_, hs6_2012 = NA_character_,
             hts8_2022 = NA_character_, hs6_2022 = NA_character_)
  } else {
    # ---- HS2022 regime (2022+) ----
    if (rev != "HS2022")
      stop(sprintf("Row %s: start >= 2022 but hs_revision = %s", gv("id"), rev))
    
    d1 <- if (nrow(src_hts8)) tidyr::crossing(base, src_hts8) |>
      transmute(!!!base, hts8_2022 = hts8, hs6_2022 = substr(hts8, 1, 6)) else tibble()
    
    d2 <- if (nrow(src_hs6)) src_hs6 |>
      rename(hs6_2022 = hs6) |>
      inner_join(h2022, by = "hs6_2022") |>
      tidyr::crossing(base) else tibble()
    
    bind_rows(d1, d2) |>
      mutate(hts8_2012 = NA_character_, hs6_2012 = NA_character_,
             hts8_2017 = NA_character_, hs6_2017 = NA_character_)
  }
}

# --- Expand all measures; then split multi-partner rows ---
expanded <- measures |>
  mutate(row_id = row_number()) |>
  group_split(row_id, .keep = TRUE) |>
  map_dfr(~ expand_one_row(.x |> slice(1))) |>
  mutate(partners = str_squish(partners)) |>
  separate_rows(partners, sep = "\\s*[,;]\\s*") |>
  mutate(partner = na_if(str_squish(partners), "")) |>
  select(-partners) |>
  distinct()

# ==================== Quarter explosion ====================
explode_quarters <- function(df, cutoff_date = as.Date("2025-06-30")) {
  if (nrow(df) == 0) return(tibble())
  df |>
    mutate(
      start_eff = as.Date(start_date),
      end_eff   = pmin(coalesce(as.Date(end_date), cutoff_date + days(1)), cutoff_date + days(1)),
      q_start   = floor_date(start_eff, "quarter"),
      q_end     = floor_date(end_eff - days(1), "quarter")
    ) |>
    filter(start_eff < end_eff) |>
    mutate(quarter = map2(q_start, q_end, ~ if (.x <= .y) seq(.x, .y, by = "quarter") else as.Date(NA))) |>
    unnest(quarter, keep_empty = FALSE) |>
    mutate(
      q_lo = floor_date(quarter, "quarter"),
      q_hi = ceiling_date(quarter, "quarter"),
      overlap_days    = pmax(0, as.numeric(pmin(end_eff, q_hi) - pmax(start_eff, q_lo))),
      quarter_days    = as.numeric(q_hi - q_lo),
      frac_of_quarter = overlap_days / quarter_days
    ) |>
    filter(overlap_days > 0) |>
    select(-q_lo, -q_hi, -q_start, -q_end, -start_eff, -end_eff)
}

# ==================== Materialisation (any regime -> quarter's HTS8) ====================
# Ensures the HS/HTS columns exist before joining.
ensure_scope_cols <- function(df) {
  need <- c("hts8_2012","hts8_2017","hts8_2022","hs6_2012","hs6_2017","hs6_2022")
  for (nm in need) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  mutate(df, across(all_of(need), as.character))
}

# Robust, “always-return-something” materialiser:
materialise_to_hts8 <- function(dfq) {
  dfq <- dfq |>
    ensure_scope_cols() |>
    ungroup() |>
    as_tibble() |>
    mutate(regime = case_when(
      quarter <  DATE_2017 ~ "2012",
      quarter <  DATE_2022 ~ "2017",
      TRUE                  ~ "2022"
    ))
  
  base_cols <- c("id","partner","quarter","program","effect","rate_type","rate_value","frac_of_quarter")
  
  # create a 0-row template to guarantee outXX objects exist
  template  <- dfq |> transmute(across(all_of(base_cols))) |> slice(0)
  empty_out <- function() bind_cols(template, tibble(hts8 = character()))
  
  # ---- 2012 target ----
  ex12 <- dfq |>
    filter(regime == "2012", !is.na(hts8_2012)) |>
    select(all_of(base_cols), hts8_2012) |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2012)
  
  r12a <- dfq |>
    filter(regime == "2012", !is.na(hs6_2012)) |>
    select(all_of(base_cols), hs6_2012) |>
    inner_join(h2012, by = "hs6_2012", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2012)
  
  r12b <- dfq |>
    filter(regime == "2012", !is.na(hs6_2017)) |>
    select(all_of(base_cols), hs6_2017) |>
    inner_join(c17_12, by = "hs6_2017", relationship = "many-to-many") |>
    inner_join(h2012,  by = "hs6_2012", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2012)
  
  r12c <- dfq |>
    filter(regime == "2012", !is.na(hs6_2022)) |>
    select(all_of(base_cols), hs6_2022) |>
    inner_join(cc67,   by = "hs6_2022", relationship = "many-to-many") |>
    inner_join(c17_12, by = "hs6_2017", relationship = "many-to-many") |>
    inner_join(h2012,  by = "hs6_2012", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2012)
  
  out12 <- bind_rows(ex12, r12a, r12b, r12c)
  if (nrow(out12) == 0) out12 <- empty_out()
  
  # ---- 2017 target ----
  ex17 <- dfq |>
    filter(regime == "2017", !is.na(hts8_2017)) |>
    select(all_of(base_cols), hts8_2017) |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2017)
  
  r17a <- dfq |>
    filter(regime == "2017", !is.na(hs6_2017)) |>
    select(all_of(base_cols), hs6_2017) |>
    inner_join(h2017, by = "hs6_2017", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2017)
  
  r17b <- dfq |>
    filter(regime == "2017", !is.na(hs6_2012)) |>
    select(all_of(base_cols), hs6_2012) |>
    inner_join(distinct(c17_12, hs6_2012, hs6_2017),
               by = "hs6_2012", relationship = "many-to-many") |>
    inner_join(h2017, by = "hs6_2017", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2017)
  
  r17c <- dfq |>
    filter(regime == "2017", !is.na(hs6_2022)) |>
    select(all_of(base_cols), hs6_2022) |>
    inner_join(cc67, by = "hs6_2022", relationship = "many-to-many") |>
    inner_join(h2017, by = "hs6_2017", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2017)
  
  out17 <- bind_rows(ex17, r17a, r17b, r17c)
  if (nrow(out17) == 0) out17 <- empty_out()
  
  # ---- 2022 target ----
  ex22 <- dfq |>
    filter(regime == "2022", !is.na(hts8_2022)) |>
    select(all_of(base_cols), hts8_2022) |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2022)
  
  r22a <- dfq |>
    filter(regime == "2022", !is.na(hs6_2022)) |>
    select(all_of(base_cols), hs6_2022) |>
    inner_join(h2022, by = "hs6_2022", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2022)
  
  r22b <- dfq |>
    filter(regime == "2022", !is.na(hs6_2017)) |>
    select(all_of(base_cols), hs6_2017) |>
    inner_join(distinct(cc67, hs6_2017, hs6_2022),
               by = "hs6_2017", relationship = "many-to-many") |>
    inner_join(h2022, by = "hs6_2022", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2022)
  
  r22c <- dfq |>
    filter(regime == "2022", !is.na(hs6_2012)) |>
    select(all_of(base_cols), hs6_2012) |>
    inner_join(distinct(c17_12, hs6_2012, hs6_2017),
               by = "hs6_2012", relationship = "many-to-many") |>
    inner_join(distinct(cc67,   hs6_2017, hs6_2022),
               by = "hs6_2017", relationship = "many-to-many") |>
    inner_join(h2022, by = "hs6_2022", relationship = "many-to-many") |>
    transmute(across(all_of(base_cols)), hts8 = hts8_2022)
  
  out22 <- bind_rows(ex22, r22a, r22b, r22c)
  if (nrow(out22) == 0) out22 <- empty_out()
  
  bind_rows(out12, out17, out22) |>
    distinct()
}

# ==================== Shifters & Overrides ====================
# 1) explode measures to quarter rows
qrows <- expanded |> explode_quarters(cutoff_date = as.Date("2025-06-30"))

# 2) SHIFTERS: effect in {adjust, exemption}; exemption zeroes the program contribution
shifters <- qrows |>
  filter(rate_type == "adval", effect %in% c("adjust", "exemption")) |>
  materialise_to_hts8() |>
  mutate(rate_contrib = rate_value * frac_of_quarter,
         is_exemption = effect == "exemption") |>
  group_by(id, partner, quarter, hts8, program) |>
  summarise(rate_contrib_id = max(rate_contrib, na.rm = TRUE),
            has_exempt_id   = any(is_exemption), .groups = "drop") |>
  group_by(partner, quarter, hts8, program) |>
  summarise(shifter_raw   = sum(rate_contrib_id, na.rm = TRUE),
            has_exemption = any(has_exempt_id),
            shifter_prog  = if_else(has_exemption, 0, shifter_raw),
            n_measures    = n_distinct(id), .groups = "drop") |>
  group_by(partner, product_code = hts8, product_level = "HTS8-target", quarter) |>
  summarise(shifter_adval = sum(shifter_prog, na.rm = TRUE),
            n_measures    = sum(n_measures), .groups = "drop") |>
  arrange(partner, product_code, quarter) |>
  mutate(year = year(quarter), qtr = quarter(quarter), yearq = paste0(year, "Q", qtr))

# 3) OVERRIDES: weighted average of 'override' rates within quarter/product
overrides <- qrows |>
  filter(rate_type == "adval", effect == "override") |>
  materialise_to_hts8() |>
  group_by(id, partner, quarter, hts8) |>
  summarise(rate_contrib_id  = max(rate_value * frac_of_quarter, na.rm = TRUE),
            share_contrib_id = max(frac_of_quarter,              na.rm = TRUE),
            .groups = "drop") |>
  group_by(partner, hts8, quarter) |>
  summarise(ov_share = pmin(1, sum(share_contrib_id, na.rm = TRUE)),
            ov_rate  = if_else(sum(share_contrib_id, na.rm = TRUE) > 0,
                               sum(rate_contrib_id,  na.rm = TRUE) / sum(share_contrib_id, na.rm = TRUE),
                               NA_real_),
            .groups = "drop")

# Save files
write_parquet(shifters, "data/temp/shifters.parquet")
write_parquet(overrides, "data/temp/overrides.parquet")
