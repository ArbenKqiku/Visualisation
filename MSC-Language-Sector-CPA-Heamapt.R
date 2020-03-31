library(googlesheets)
gs_auth("/Users/arbenkqiku/Desktop/GitHub/Scripts/googlesheets_token_arben.rds")

sector_language_cpa = gs_title("MSC - CPA per language and sector")
sector_language_cpa_table = gs_read(ss = sector_language_cpa, ws = "Rapport sur les performances des campagnes")

sector_language_cpa_table = 
    sector_language_cpa_table %>%
    # divide CPA by a 100
    mutate(CPA = CPA/100) %>% 
    # select only interesting columns
    select(Language, Sector, CPA) %>%
    # widen the table so that we can have the same amount of combinations between qualitative variables
    pivot_wider(names_from = Sector, values_from = CPA, values_fill = list(CPA = 0)) %>%
    # make it longer again so that languages is a column again
    pivot_longer(names_to = "Sector", cols = `1`:`11`, values_to = "CPA") %>%
    # unnest CPA as it has been transformed into a list previously
    unnest(CPA) %>%
    # transform sector into a factor
    mutate(Sector = as_factor(Sector) %>% fct_reorder(as.numeric(Sector)))

# create graph with previous table
sector_language_cpa_table %>% 
    ggplot(aes(x = Language, y = Sector, fill = CPA)) +
    geom_tile(color = "white", size = 0.1) +
    geom_text(aes(label = CPA), color = "white") + 
    scale_fill_viridis_c(direction = 1) +
    scale_x_discrete(labels = abbreviate) + 
    labs(title = "Language and Campaigns", subtitle = "CPA Interactions",
         x = "Languages", y = "Campaigns")
