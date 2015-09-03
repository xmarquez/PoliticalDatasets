library(dplyr)
library(PoliticalDatasets)
library(reshape2)
library(readxl)
library(stringr)

vanhanen.1 <- read_excel("../Data/FSD1289/Study/data/daF1289e.xls", skip = 1)

vanhanen.1 <- melt(vanhanen.1, id.vars = "Country")

vanhanen.1 <- vanhanen.1 %>% group_by(Country,variable) %>% mutate(year = str_extract(variable,"[0-9]+"))

vanhanen.1 <- vanhanen.1 %>% ungroup()  %>% rename(country = Country, vanhanen = value)

vanhanen.2 <- read_excel("../Data/FSD1289/Study/data/daF1289e.xls", skip = 1, sheet = 2)

vanhanen.2 <- melt(vanhanen.2, id.vars = "Country")

vanhanen.2 <- vanhanen.2 %>% group_by(Country,variable) %>% mutate(year = str_extract(variable,"[0-9]+"))

vanhanen.2 <- vanhanen.2 %>% ungroup() %>% rename(country = Country, vanhanen = value)

vanhanen.3 <- read_excel("../Data/FSD1289/Study/data/daF1289e.xls", skip = 1, sheet = 3)

vanhanen.3 <- melt(vanhanen.3, id.vars = "Country")

vanhanen.3 <- vanhanen.3 %>% group_by(Country,variable) %>% mutate(year = str_extract(variable,"[0-9]+"))

vanhanen.3 <- vanhanen.3 %>% ungroup() %>% rename(country = Country, vanhanen = value)

vanhanen <- rbind(vanhanen.1,vanhanen.2,vanhanen.3)

vanhanen <- vanhanen %>% filter(!is.na(vanhanen)) %>% arrange(country,year)

rm(vanhanen.1,vanhanen.2, vanhanen.3)

vanhanen <- vanhanen %>% mutate(country = plyr::mapvalues(str_trim(country), from = c("Vietnam, Socialist Republic of"), to = c("North Vietnam")))

vanhanen$year <- as.numeric(vanhanen$year)

match_condition <-  c("ifelse(!country %in% c('Germany'),date_matches == max(date_matches) & !duplicated(date_matches),TRUE)","!(country_name == 'Germany (Prussia)' & year >= 1945)","!(country_name == 'German Federal Republic' & year < 1945)")

vanhanen <- to_gw_system(vanhanen, match_condition = match_condition)

vanhanen <- vanhanen %>% mutate(in_system = country_matches & date_matches) %>% ungroup() %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods, -country) %>% select(country_name,GWn, GWc, year,variable, vanhanen, region:in_system)

# vanhanen %>% filter(num_matches > 3)
# 
# vanhanen %>% filter(grepl("German",country)) %>% group_by(country,country_name, GW_startdate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# 
# vanhanen %>% filter(grepl("Vietnam",country)) %>% group_by(country,country_name, GW_startdate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# 
# vanhanen %>% filter(grepl("Serbia",country)) %>% group_by(country,country_name, GW_startdate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# 
# vanhanen %>% filter(grepl("Yemen",country)) %>% group_by(country,country_name, GW_startdate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# 
# vanhanen %>% filter(country != country_name) %>% group_by(country,country_name, GW_startdate, GWn) %>% summarise(min(year), max(year),n()) %>% data.frame()

# load("../Pemstein Meserve and Melton Code/uds/data/democracy1946.2008.rda")
# 
# democracy <-  democracy %>% rename(uds_country = country, uds_ccode = cowcode)
# 
# democracy <- to_gw_system(democracy, country_col = "uds_country", include_extras = FALSE)
# 
# democracy <- democracy %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)
# 
# comparison <- left_join(democracy %>% select(country_name,GWn,year,vanhanen), vanhanen %>% filter(grepl("Index",variable)) %>% select(country_name,GWn, year,vanhanen), by=c("country_name","GWn","year"))
# 
# comparison %>% filter(!is.na(vanhanen.x), is.na(vanhanen.y))
# 
# comparison %>% filter(is.na(vanhanen.x), !is.na(vanhanen.y))
# 
# comparison %>% filter(vanhanen.x != vanhanen.y)

devtools::use_data(vanhanen, overwrite = TRUE)
