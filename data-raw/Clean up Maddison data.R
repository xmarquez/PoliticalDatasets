library(dplyr)
library(readxl)
library(reshape2)

maddison <- read_excel("../Data/mpd_2013-01.xlsx",skip=2)

names(maddison)[1] <- "year"

maddison <- melt(maddison, id.vars = "year")

names(maddison)[2] <- "maddison_country"

maddison <- maddison %>% filter(!is.na(maddison_country))

maddison$maddison_country <- plyr::mapvalues(maddison$maddison_country, from = c("N. Zealand ", "Czecho-slovakia", "F. Czecho-slovakia", "Turk-menistan ","England/GB/UK"), to = c("New Zealand", "Czechoslovakia", "Former Czechoslovakia", "Turkmenistan","England or Great Britain or United Kingdom")) 

maddison <- na.omit(maddison)

match_condition <-
  c("date_matches == max(date_matches) & !duplicated(date_matches)")

maddison <- PoliticalDatasets::to_gw_system(maddison, country_col = "maddison_country", match_condition = match_condition)

# maddison %>% filter(grepl("German",maddison_country)) %>% group_by(maddison_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# maddison %>% filter(grepl("England",maddison_country)) %>% group_by(maddison_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# maddison %>% filter(grepl("Serbia",maddison_country)) %>% group_by(maddison_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# maddison %>% filter(grepl("Yugosla",maddison_country)) %>% group_by(maddison_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# maddison %>% filter(grepl("Yemen",maddison_country)) %>% group_by(maddison_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# maddison %>% filter(grepl("Viet",maddison_country)) %>% group_by(maddison_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# maddison %>% filter(grepl("Ha",maddison_country)) %>% group_by(maddison_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# maddison %>% ungroup() %>% filter(stringr::str_trim(maddison_country) != country_name) %>% distinct(maddison_country,country_name, GW_startdate, GWn)

maddison <- maddison %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

devtools::use_data(maddison, overwrite = TRUE)
