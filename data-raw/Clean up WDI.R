library(dplyr)
library(reshape2)
library(WDI)

wdi <- WDI(indicator = c("NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD","NY.GDP.MKTP.PP.KD", "SP.POP.TOTL"), start = 1950, end = 2015, extra = TRUE)

wdi <- wdi %>% rename(wdi_country = country, wdi_region = region) 

match_condition <-
  c("date_matches == max(date_matches) & !duplicated(date_matches)")

wdi <- PoliticalDatasets::to_gw_system(wdi, country_col = "wdi_country", match_condition = match_condition)

# wdi %>% filter(grepl("German",wdi_country)) %>% group_by(wdi_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# wdi %>% filter(grepl("Serbia",wdi_country)) %>% group_by(wdi_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# wdi %>% filter(grepl("Yugosla",wdi_country)) %>% group_by(wdi_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# wdi %>% filter(grepl("Yemen",wdi_country)) %>% group_by(wdi_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# wdi %>% filter(grepl("Viet",wdi_country)) %>% group_by(wdi_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# wdi %>% filter(grepl("Haiti",wdi_country)) %>% group_by(wdi_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# wdi %>% ungroup() %>% filter(stringr::str_trim(wdi_country) != country_name) %>% distinct(wdi_country,country_name, GW_startdate, GWn) %>% select(wdi_country, country_name) %>% data.frame()

wdi <- wdi %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

wdi <- wdi %>% mutate(wdi_gdp_per_cap_constant = NY.GDP.MKTP.KD/SP.POP.TOTL)

devtools::use_data(wdi, overwrite = TRUE)
