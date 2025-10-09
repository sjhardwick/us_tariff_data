library(haven)
library(readr)
library(dplyr)
library(stringr)
library(purrr)

countries <- read_csv("data/temp/countries.csv", show_col_types = FALSE) # must have cty_code, i_iso3

# ---- helper to read annual Schott detail ----
read_imp_year <- function(y) {
  base <- sprintf("data/raw/trade_schott/imp_detl_%d_12n", y)
  f_dta <- paste0(base, ".dta")
  f_csv <- paste0(base, ".csv")
  
  if (file.exists(f_dta)) {
    df <- haven::read_dta(f_dta)
  } else if (file.exists(f_csv)) {
    df <- read_csv(f_csv, show_col_types = FALSE)
  } else stop("Missing file for year ", y)
  
  df
}

# ---- build annual weights ----
years <- 2015:2025

out <- map_dfr(years, function(y) {
  y_use <- if (y == 2025) 2024 else y  # reuse 2024 weights for 2025
  message("[weights] tariffs year ", y, " (using trade ", y_use, ")")
  
  df <- read_imp_year(y_use) %>%
    mutate(
      commodity_chr = str_pad(as.character(commodity), 10, pad = "0"),
      hts8 = substr(commodity_chr, 1, 8),
      hs6  = substr(commodity_chr, 1, 6),
      value = con_val_yr
    ) %>%
    left_join(select(countries, cty_code, i_iso3), by = "cty_code") %>%
    filter(!is.na(i_iso3)) %>%
    group_by(year = y, i_iso3, hs6, hts8) %>%   # note: force 'year = y'
    summarise(value_hts8 = sum(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(year, i_iso3, hs6) %>%
    mutate(total_hs6 = sum(value_hts8, na.rm = TRUE),
           weight = ifelse(total_hs6 > 0, value_hts8 / total_hs6, NA_real_)) %>%
    ungroup() %>%
    select(year, i_iso3, hs6, hts8, weight)
})

arrow::write_parquet(out, "data/processed/weights_hts8_annual.parquet")
