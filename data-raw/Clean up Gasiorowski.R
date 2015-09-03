library(dplyr)
library(PoliticalDatasets)
library(lubridate)

gasiorowski <- read.csv("../Data/Gasiorowski.csv")

gasiorowski <- gasiorowski %>% group_by(country,regime,start,end) %>% do(data.frame(year = .$start:.$end)) 

gasiorowski <- gasiorowski %>% ungroup() %>% mutate(prc = plyr::mapvalues(regime, from = c("A","T","S","D"), to = 1:4), prc = as.numeric(as.character(prc)))

gasiorowski <- PoliticalDatasets::to_gw_system(gasiorowski, country_col = "country")

gasiorowski <- gasiorowski %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

gasiorowski <- gasiorowski %>% ungroup() %>% select(-country) %>% select(country_name, GWn, GWc, year, regime, prc, start, end, region:in_system)

gasiorowski <- gasiorowski %>% arrange(country_name,year,start,end)

devtools::use_data(gasiorowski, overwrite = TRUE)
