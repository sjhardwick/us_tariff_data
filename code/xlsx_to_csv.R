library(readxl)
library(dplyr)
library(lubridate)
library(readr)

measures_xlsx <- read_excel("data/temp/measures.xlsx") %>%
  mutate(
    start_date = as.Date(start_date, origin = "1899-12-30"),  # Excel date fix if numeric
    end_date   = as.Date(end_date,   origin = "1899-12-30")
  )

# Force ISO-8601 before saving
measures_xlsx <- measures_xlsx %>%
  mutate(
    start_date = format(start_date, "%Y-%m-%d"),
    end_date   = format(end_date, "%Y-%m-%d")
  )

write_csv(measures_xlsx, "data/temp/measures.csv")
