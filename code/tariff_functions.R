
library(tidyverse)

# function to get MFN rates

mfn_rates <- function(usitc_table, uvs) {
  
  countries <- read_csv("data/temp/countries.csv")
  
  hts8 <- usitc_table %>% pull(hts8) %>% unique()
  i_iso3 <- countries %>% pull(i_iso3)
  
  shell <- expand_grid(hts8, i_iso3) %>% 
    arrange(hts8, i_iso3)
  
  ave_by_hts8 <- usitc_table %>% 
    left_join(uvs, by = "hts8") %>% 
    filter(!(substr(hts8, 1, 2) == "99")) %>% 
    mutate(mfn_ave = case_when(mfn_rate_type_code == "0" ~ 0, 
                               mfn_rate_type_code == "7" ~ mfn_ad_val_rate, 
                               mfn_rate_type_code == "1" ~  
                                 mfn_specific_rate / unit_value_1, 
                               mfn_rate_type_code == "2" ~  
                                 mfn_specific_rate / unit_value_2, 
                               mfn_rate_type_code == "3" ~ 
                                 mfn_specific_rate / unit_value_1 +  
                                 mfn_other_rate / unit_value_2, 
                               mfn_rate_type_code == "4" ~ 
                                 mfn_specific_rate / unit_value_1 + 
                                 mfn_ad_val_rate, 
                               mfn_rate_type_code == "5" ~ 
                                 mfn_specific_rate / unit_value_2 + 
                                 mfn_ad_val_rate, 
                               mfn_rate_type_code == "6" ~ 
                                 mfn_specific_rate / unit_value_1 + 
                                 mfn_other_rate / unit_value_2 + 
                                 mfn_ad_val_rate, 
                               TRUE ~ NA)) %>% 
    select(hts8, mfn_ave) 
  
  ave_by_pair <- shell %>%
    left_join(ave_by_hts8, by = "hts8") %>% 
    na.omit() 
  
  return(ave_by_pair)
  
}

# function to get column 2 rates

col2_rates <- function(usitc_table, uvs, year) {
  
  countries <- read_csv("data/temp/countries.csv")
  
  hts8 <- usitc_table %>% pull(hts8) %>% unique()
  i_iso3 <- if (year > 2022) {
    c("PRK", "CUB", "BLR", "RUS")
  } else {
    c("PRK", "CUB")
  }
  
  shell <- expand_grid(hts8, i_iso3) %>% 
    arrange(hts8, i_iso3)
  
  ave_by_hts8 <- usitc_table %>% 
    left_join(uvs, by = "hts8") %>% 
    filter(!(substr(hts8, 1, 2) == "99")) %>% 
    mutate(col2_ave = case_when(col2_rate_type_code == "0" ~ 0, 
                                col2_rate_type_code == "7" ~ col2_ad_val_rate, 
                                col2_rate_type_code == "1" ~ 
                                  col2_specific_rate / unit_value_1, 
                                col2_rate_type_code == "2" ~ 
                                  col2_specific_rate / unit_value_2, 
                                col2_rate_type_code == "3" ~ 
                                  col2_specific_rate / unit_value_1 + 
                                  col2_other_rate / unit_value_2, 
                                col2_rate_type_code == "4" ~ 
                                  col2_specific_rate / unit_value_1 + 
                                  col2_ad_val_rate, 
                                col2_rate_type_code == "5" ~ 
                                  col2_specific_rate / unit_value_2 + 
                                  col2_ad_val_rate, 
                                col2_rate_type_code == "6" ~ 
                                  col2_specific_rate / unit_value_1 + 
                                  col2_other_rate / unit_value_2 + 
                                  col2_ad_val_rate, 
                                TRUE ~ NA)) %>% 
    select(hts8, col2_ave) 
  
  ave_by_pair <- shell %>%
    left_join(ave_by_hts8, by = "hts8") %>% 
    na.omit() 
  
  return(ave_by_pair)
  
}

# function to get preferential rates

pref_rates <- function(usitc_table, uvs, indicator) {
  
  indicator_name <- usitc_table %>% 
    select(ends_with(c("_ind", "indicator"))) %>% 
    select_if(~ any(. == indicator)) %>% 
    colnames()
  
  # get unique number for indicator to avoid same name for two schemes
  indicator_id <- paste(utf8ToInt(indicator), collapse = "")
  
  pref_name <- gsub("_indicator$", "", indicator_name)
  pref_name <- gsub("_ind$", "", pref_name)
  pref_name_ave <- paste(pref_name, "_ave_", indicator_id, sep = "")
  
  tariffs_under_pref <- usitc_table %>% 
    left_join(uvs, by = "hts8") %>% 
    filter(!!sym(indicator_name) == indicator) %>% 
    filter(!(substr(hts8, 1, 2) == "99"))
  
  pref_rate_type_code <- paste(pref_name, "_rate_type_code", sep = "")
  pref_ad_val_rate <- paste(pref_name, "_ad_val_rate", sep = "")
  pref_specific_rate <- paste(pref_name, "_specific_rate", sep = "")
  pref_other_rate <- paste(pref_name, "_other_rate", sep = "")
  
  if (pref_rate_type_code %in% colnames(usitc_table)) {
    
    ave_by_hts8 <- usitc_table %>% 
      left_join(uvs, by = "hts8") %>% 
      filter(!(substr(hts8, 1, 2) == "99")) %>% 
      mutate(!!pref_name_ave := case_when(
        !!sym(pref_rate_type_code) == "0" ~ 0, 
        !!sym(pref_rate_type_code) == "7" ~ !!sym(pref_ad_val_rate), 
        TRUE ~ NA)
      )
        
        if (pref_specific_rate %in% colnames(usitc_table)) {
          
          ave_by_hts8 <- ave_by_hts8 %>%
            mutate(!!pref_name_ave := case_when(
              !!sym(pref_rate_type_code) == "1" ~ !!sym(pref_specific_rate) / unit_value_1, 
              !!sym(pref_rate_type_code) == "2" ~ !!sym(pref_specific_rate) / unit_value_2, 
              !!sym(pref_rate_type_code) == "4" ~ !!sym(pref_specific_rate) / unit_value_1 + !!sym(pref_ad_val_rate),
              !!sym(pref_rate_type_code) == "5" ~ !!sym(pref_specific_rate) / unit_value_2 + !!sym(pref_ad_val_rate),
              TRUE ~ !!sym(pref_name_ave)
            ))
          
          if (pref_other_rate %in% colnames(usitc_table)) {
            
            ave_by_hts8 <- ave_by_hts8 %>%
              mutate(!!pref_name_ave := case_when(
                !!sym(pref_rate_type_code) == "3" ~ !!sym(pref_specific_rate) / unit_value_1 + 
                  !!sym(pref_other_rate) / unit_value_2, 
                !!sym(pref_rate_type_code) == "6" ~ !!sym(pref_specific_rate) / unit_value_1 + 
                  !!sym(pref_other_rate) / unit_value_2 + !!sym(pref_ad_val_rate), 
                TRUE ~ !!sym(pref_name_ave)
              ))
            
          }
          
        }
    
  } else {
    
    ave_by_hts8 <- usitc_table %>% 
      left_join(uvs, by = "hts8") %>% 
      filter(!(substr(hts8, 1, 2) == "99")) %>% 
      mutate(!!pref_name_ave := 0)
    
  }
  
  ave_by_hts8 <- ave_by_hts8 %>% 
    select(hts8, !!sym(pref_name_ave))
  
  # create shell table with all PTA partner-product pairs
  
  hts8 <- usitc_table %>% pull(hts8) %>% unique()
  
  i_iso3 <- read_csv("data/raw/preferences.csv") %>%
    rename("ind" = indicator) %>% 
    filter(ind == indicator) %>% 
    pull(i_iso3)
  
  shell <- expand_grid(hts8, i_iso3) %>% 
    arrange(hts8, i_iso3)
  
  ave_by_pair <- shell %>%
    left_join(ave_by_hts8, by = "hts8") %>% 
    na.omit()
  
  return(ave_by_pair)
  
}

# function to put 0 back at front of commodity code if missing

fix_commodity <- function(commodity){
  
  commodity <- format(commodity, scientific = FALSE)
  
  commodity <- as.character(commodity)
  
  commodity <- if_else(substr(commodity, 10, 10) == "", 
                       paste("0", commodity, sep = ""), 
                       commodity) 
  
  commodity <- if_else(substr(commodity, 1, 1) == " ", 
                       paste("0", substr(commodity, 2, 10), sep = ""), 
                       commodity)
  
}
