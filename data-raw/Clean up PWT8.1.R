library(dplyr)
library(readxl)
library(reshape2)

pwt8.1 <- read_excel("../Data/pwt81.xlsx", sheet = 3)
pwt8.0 <- read_excel("../Data/pwt80.xlsx", sheet = 3)

pwt8.1 <- pwt8.1 %>% rename(pwt_country = country)
pwt8.0 <- pwt8.0 %>% rename(pwt_country = country)

match_condition <-
  c("date_matches == max(date_matches) & !duplicated(date_matches)")

pwt8.1 <- PoliticalDatasets::to_gw_system(pwt8.1, country_col = "pwt_country", match_condition = match_condition)
pwt8.0 <- PoliticalDatasets::to_gw_system(pwt8.0, country_col = "pwt_country", match_condition = match_condition)

pwt8.1 %>% filter(pwt_country == "China")

# pwt8.1 %>% filter(grepl("China",pwt_country)) %>% group_by(pwt_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# pwt8.1 %>% filter(grepl("German",pwt_country)) %>% group_by(pwt_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# pwt8.1 %>% filter(grepl("Serbia",pwt_country)) %>% group_by(pwt_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# pwt8.1 %>% filter(grepl("Yugosla",pwt_country)) %>% group_by(pwt_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# pwt8.1 %>% filter(grepl("Yemen",pwt_country)) %>% group_by(pwt_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# pwt8.1 %>% filter(grepl("Viet",pwt_country)) %>% group_by(pwt_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# pwt8.1 %>% filter(grepl("Haiti",pwt_country)) %>% group_by(pwt_country,country_name, GW_startdate, GW_enddate, GWn, date_matches) %>% summarise(min(year), max(year),n())
# pwt8.1 %>% ungroup() %>% filter(stringr::str_trim(pwt_country) != country_name) %>% distinct(pwt_country,country_name, GW_startdate, GWn) %>% select(pwt_country, country_name) %>% data.frame()

pwt8.1 <- pwt8.1 %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)
pwt8.0 <- pwt8.0 %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

devtools::use_data(pwt8.1, overwrite = TRUE)
devtools::use_data(pwt8.0, overwrite = TRUE)
