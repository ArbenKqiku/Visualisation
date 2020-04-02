library(dplyr)
library(zoo)
library(scales)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(readxl)
library(readr)

campaign_data = read_csv("conversions_by_month_and_language_facets_wrap.csv")

# wrangle data
wrangled_data_tbl = campaign_data %>% 
    
    # modify column names
    rename_all(~ str_to_lower(.) %>% 
                   str_replace(" ", "_")) %>% 
    
    # change date column to date format
    mutate(date = date %>% as.Date(format = "%d/%m/%Y")) %>% 
    
    # create month columns
    mutate(month_number = date %>% month,
           month_text = date %>% as.yearmon()) %>%
    
    # convert conversions to numeric
    mutate(conversions = conversions %>% 
               str_replace(",", ".") %>% 
               as.numeric()) %>% 
    
    # summarize data
    group_by(language, month_text, month_number) %>% 
    summarize_if(is.numeric, sum) %>% 
    ungroup() %>% 
    
    # transform month_number into factor
    mutate(month_number = month_number %>% 
               as_factor() %>% 
               fct_relevel(c("8", "9", "10", "11", "12")))

# determine language order
language_order = c("Language 1", "Language 2", "Language 3", "Language 4", "Language 5",
                   "Language 6", "Language 7", "Language 8", "Language 9", "Language 10",
                   "Language 11", "Language 12", "Language 13", "Language 14",
                   "Language 15", "Language 16")

# set-up
wrangled_data_tbl %>% ggplot(aes(month_number %>% 
                                     as.numeric(),
                                 conversions, fill = language)) +
    
    # geometries
    geom_col() +
    geom_smooth(method = lm, se = FALSE) +
    
    # facet wrap
    facet_wrap(facets = ~ language %>% 
                   as_factor() %>% 
                   fct_relevel(language_order), scales = "free_y") +
    
    # formatting
    theme_tq() +
    scale_fill_tq() +
    scale_x_continuous(breaks = 1:9, labels = wrangled_data_tbl %>% 
                           pull(month_number) %>% 
                           unique()) + 
    labs(title = "Conversions by Month and Language",
         subtitle = "Some countries display a positive trend, other countries the opposite",
         x = "",
         y = "Conversions") +
    theme(legend.position = "none")







