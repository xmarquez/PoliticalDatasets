library(dplyr)

ciri <- read.csv("https://drive.google.com/uc?export=download&id=0BxDpF6GQ-6fbbEdZYmRXekhGMFE")

ciri <- ciri %>% rename(ciri_country = CTRY, year = YEAR)

match_condition <-
  c("date_matches == max(date_matches) & !duplicated(date_matches)")

ciri <- PoliticalDatasets::to_gw_system(ciri, country_col = "ciri_country", code_col = "COW", match_condition = match_condition)

# ciri %>% filter(grepl("German",ciri_country)) %>% group_by(ciri_country,country_name, GW_startdate, GW_enddate, GWn, COW, date_matches) %>% summarise(min(year), max(year),n())
# ciri %>% filter(grepl("Serbia",ciri_country)) %>% group_by(ciri_country,country_name, GW_startdate, GWn, COW, date_matches) %>% summarise(min(year), max(year),n())
# ciri %>% filter(grepl("Yugosla",ciri_country)) %>% group_by(ciri_country,country_name, GW_startdate, GWn, COW, date_matches) %>% summarise(min(year), max(year),n())
# ciri %>% filter(grepl("Yemen",ciri_country)) %>% group_by(ciri_country,country_name, GW_startdate, GWn, COW, date_matches) %>% summarise(min(year), max(year),n())
# ciri %>% filter(grepl("Viet",ciri_country)) %>% group_by(ciri_country,country_name, GW_startdate, GWn, COW, date_matches) %>% summarise(min(year), max(year),n())
# ciri %>% filter(grepl("Haiti",ciri_country)) %>% group_by(ciri_country,country_name, GW_startdate, GWn, COW, date_matches) %>% summarise(min(year), max(year),n())
# ciri %>% filter(GWn != COW) %>% group_by(ciri_country,country_name, GW_startdate, GWn, COW) %>% summarise(min(year), max(year),n())

ciri <- ciri %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

devtools::use_data(ciri, overwrite = TRUE)
