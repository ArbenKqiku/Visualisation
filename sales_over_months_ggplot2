# Display sales by month with ggplot2

library(googlesheets)
library(dplyr)
library(zoo)
library(scales)
library(tidyverse)

sales_by_month_tbl <- conversion_tbl %>% 
    set_names(c("Date", "Conversions")) %>% 
    mutate(month_text = as.yearmon(Date),
           month_number = month(Date) %>% as_factor() %>% fct_reorder(1:length(Date))) %>%
    
    group_by(month_text, month_number) %>% 
    summarize(total_conversions = sum(Conversions)) %>% 
    ungroup() %>% 
    
    arrange(month_text) %>% 
    
    # format number column
    mutate(label_text = format(total_conversions, big.mark="'")) %>% 

    # add observation column for geom_smooth
    mutate(observation = 1:length(month_text) %>% as.numeric())

sales_by_month_tbl %>% 
    ggplot(aes(x = observation, y = total_conversions)) + 
    geom_col(fill = palette_light()[1]) +
    scale_x_continuous(breaks = 1:(sales_by_month_tbl %>% nrow()), 
                       labels = sales_by_month_tbl %>% pull(month_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_tq() +
    geom_label(aes(label = label_text)) +
    scale_y_continuous(labels = dollar_format(big.mark = "'", prefix = "")) +
    labs(title = "Conversions over Time",
         subtitle = "Seasonal fluctuation of conversions",
         x = "",
         y = "Conversions")


