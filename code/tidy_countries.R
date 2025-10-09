
library(countrycode)
library(readr)
library(tidyverse)

# read in concordance (2021) from USITC

countries_raw <- read_tsv("data/raw/countries_2021_usitc.txt") %>% pull()

countries <- tibble(countries_raw) %>% 
  mutate(cty_code = substr(countries_raw, 3, 6), 
         i_name = substr(countries_raw, 19, nchar(countries_raw))) %>% 
  select(-countries_raw) %>% 
  mutate(i_iso3 = countrycode(i_name, "country.name", "iso3c")) %>%
  na.omit() # remove e.g. international organisations, unidentified countries

write_csv(countries, "data/temp/countries.csv") # save
