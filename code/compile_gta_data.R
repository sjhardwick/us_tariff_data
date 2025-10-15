
library(tidyverse)
library(openxlsx)

# set the folder containing the CSV files
folder_path <- "data/raw/gta"

# list all CSV files in the folder
files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# read and bind them into one data frame
all_data <- files %>%
  map_dfr(read_csv, .id = "source_file")

# preview result
print(all_data)

# filter
clean_data <- all_data %>%
  filter(`Eligible Firm` == "all") %>%
  filter(!str_detect(`State Act Title`, "Reclassification")) %>%
  filter(!str_detect(`State Act Title`, "GSP")) %>%
  filter(!str_detect(`State Act Title`, "Generalized System of Preferences")) %>%
  filter(!str_detect(`State Act Title`, "Import tariff changes in")) %>%
  filter(!str_detect(`State Act Title`, "Termination of trade preferences for India and Turkiye")) %>%
  filter(!str_detect(`State Act Title`, "Government to suspend import duties on baby formula")) %>%
  filter(!str_detect(`State Act Title`, "The U.S. Administration supports revocation of the Most-Favoured-Nation tariff treatment for Russia")) %>%
  mutate(
    implemented = as.Date(`Date Implemented`),
    removed     = as.Date(`Date Removed`),
    duration    = as.numeric(difftime(removed, implemented, units = "days"))
  ) %>%
  filter(is.na(duration) | duration >= 1)

write.xlsx(clean_data, "data/temp/gta.xlsx")
