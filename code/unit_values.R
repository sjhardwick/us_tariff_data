# pooled, robust unit values (2012–2014) → formatted like (year, hts8, quantity_code, unit_value)

library(haven)
library(tidyverse)
library(stringr)
library(purrr)

# ----- helper: build unit values for a 3-year window -----
build_unit_values <- function(start_year = 2012, tariff_meta_year = NULL) {
  # choose 3-year span: y, y+1, y+2
  years <- start_year:(start_year + 2)
  # default tariff metadata to the middle year
  y_meta <- tariff_meta_year %||% years[2]
  
  # ---------- 1) read & stack, compute robust uvs ----------
  imports <- map_dfr(years, function(y) {
    read_dta(sprintf("data/raw/trade_schott/imp_detl_%d_12n.dta", y)) %>%
      select(commodity, gen_qy1_yr, gen_qy2_yr, dut_val_yr) %>%
      rename(
        quantity_1    = gen_qy1_yr,
        quantity_2    = gen_qy2_yr,
        customs_value = dut_val_yr
      ) %>%
      mutate(
        year = y,
        # stabilise hts text format
        commodity = as.character(format(commodity, scientific = FALSE, trim = TRUE)),
        commodity = str_replace_all(commodity, "\\s+", ""),
        commodity = str_pad(commodity, width = 10, side = "left", pad = "0"),
        hts8      = substr(commodity, 1, 8)
      )
  })
  
  imports_uv <- imports %>%
    mutate(
      quantity_1    = suppressWarnings(as.numeric(quantity_1)),
      quantity_2    = suppressWarnings(as.numeric(quantity_2)),
      customs_value = suppressWarnings(as.numeric(customs_value)),
      uv_q1 = if_else(!is.na(quantity_1) & quantity_1 > 0, customs_value / quantity_1, NA_real_),
      uv_q2 = if_else(!is.na(quantity_2) & quantity_2 > 0, customs_value / quantity_2, NA_real_)
    )
  
  # gentle filters
  imports_uv_clean <- imports_uv %>%
    filter(!is.na(customs_value), customs_value > 0) %>%
    # remove zeroes
    filter(is.na(uv_q1) | uv_q1 > 0, is.na(uv_q2) | uv_q2 > 0)
  
  # pooled medians across the 3-year window
  uv_medians <- imports_uv_clean %>%
    group_by(hts8) %>%
    summarise(
      median_uv_q1 = if (all(is.na(uv_q1))) NA_real_ else median(uv_q1, na.rm = TRUE),
      median_uv_q2 = if (all(is.na(uv_q2))) NA_real_ else median(uv_q2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------- 2) read tariff uq codes (from chosen metadata year) ----------
  tariffs <- read_delim(
    sprintf("data/raw/tariffs_usitc/tariff_database_%d.txt", y_meta),
    show_col_types = FALSE
  ) %>%
    select(hts8, quantity_1_code, quantity_2_code) %>%
    distinct()
  
  # ---------- 3) reshape to target format ----------
  uv_long_q1 <- uv_medians %>%
    left_join(tariffs, by = "hts8") %>%
    transmute(
      hts8,
      quantity_code = quantity_1_code,
      unit_value    = median_uv_q1
    )
  
  uv_long_q2 <- uv_medians %>%
    left_join(tariffs, by = "hts8") %>%
    transmute(
      hts8,
      quantity_code = quantity_2_code,
      unit_value    = median_uv_q2
    )
  
  unit_values <- bind_rows(uv_long_q1, uv_long_q2) %>%
    # drop rows where we don't have either a code or a value
    filter(!is.na(quantity_code), !is.na(unit_value))
  
  # ---------- 4) preview and save ----------
  message(sprintf("3-year window: %d–%d (tariff meta: %d)", years[1], years[3], y_meta))
  print(head(unit_values, 10))
  
  out_path <- sprintf("data/temp/unit_values_%d_%d.csv", years[1], years[3])
  write_csv(unit_values, out_path)
  message(sprintf("saved: %s", out_path))
  
  invisible(unit_values)
}

# usage
build_unit_values(start_year = 2010)
