# ==========================================
# Build HS2012 6-digit weights from Schott
# Using 2012 annual (Dec YTD) imports
# ==========================================

library(tidyverse)
library(lubridate)
library(haven)   # read_dta

# --- helpers ---
pad10_num <- function(x) sprintf("%010.0f", floor(as.numeric(x)))  # keep leading zeros
h8_from_h10 <- function(h10) substr(h10, 1, 8)
h6_from_h10 <- function(h10) substr(h10, 1, 6)

# choose value column; prefer general value if present, else consumption
pick_value_col <- function(df) {
  cand <- c("gen_val_yr","con_val_yr")
  nm <- cand[cand %in% names(df)][1]
  if (is.na(nm)) stop("No value column found (gen_val_yr/con_val_yr/...).")
  df[[nm]]
}

# --- read 2012 (and other years if applicable) files and stack ---
years <- 2012
files <- glue::glue("data/raw/trade_schott/imp_detl_{years}_12n.dta")
names(files) <- years
stopifnot(all(file.exists(files)))

weights_src <- map_dfr(names(files), function(yy) {
  df <- read_dta(files[[yy]])
  # build clean 10/8/6-digit codes
  h10 <- pad10_num(df$commodity)
  tibble(
    year = as.integer(yy),
    h10  = h10,
    hts8 = h8_from_h10(h10),
    hs6  = h6_from_h10(h10),
    val  = as.numeric(pick_value_col(df))
  )
}) %>%
  filter(!is.na(val), val > 0)

# --- collapse to HTS8 and HS6, average across years chosen ---
w8_year <- weights_src %>%
  group_by(year, hts8) %>%
  summarise(v8 = sum(val, na.rm = TRUE), .groups = "drop")

# average across years
w8_main <- w8_year %>%
  group_by(hts8) %>%
  summarise(w8_main = mean(v8, na.rm = TRUE), .groups = "drop")

# map to HS6 (from the same h10 split)
h8_to_h6 <- weights_src %>%
  distinct(hts8, hs6)

weights <- w8_main %>%
  inner_join(h8_to_h6, by = "hts8") %>%
  group_by(hs6) %>%
  mutate(
    n_h8     = dplyr::n(),
    W6       = sum(w8_main, na.rm = TRUE),
    s_h8_in_h6 = if_else(W6 > 0, w8_main / W6, NA_real_)
  ) %>%
  ungroup() %>%
  select(hs6, hts8, n_h8, s_h8_in_h6, w8_main, W6)

# --- sanity checks ---
# 1) HTS8 must not start with "00"
bad_h8 <- weights %>% filter(substr(hts8,1,2) == "00")
if (nrow(bad_h8)) {
  stop("Found HTS8 codes starting with '00' (invalid). Recheck how HTS8 is constructed.")
}

# 2) Each HS6 should have >=1 HTS8; many have multiple
summary_by_hs6 <- weights %>% dplyr::count(hs6, name="n_h8")
# glance:
summary_by_hs6 %>% summarise(min_h8 = min(n_h8), median_h8 = median(n_h8), max_h8 = max(n_h8))

# 3) Shares within HS6 sum to ~1 (allowing for rounding)
check_shares <- weights %>% group_by(hs6) %>%
  summarise(sum_share = sum(s_h8_in_h6, na.rm = TRUE), .groups="drop")
stopifnot(max(abs(check_shares$sum_share - 1), na.rm=TRUE) < 1e-6)

# save
write_csv(weights, "data/temp/weights.csv")
