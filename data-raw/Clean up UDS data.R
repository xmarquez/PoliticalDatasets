library(dplyr)

uds <- read.csv("../Data/uds_summary.csv") %>% 
  rename(uds_country = country, uds_ccode = cowcode) 

uds <- PoliticalDatasets::to_gw_system(uds, country_col = "uds_country")

# uds %>% filter(grepl("German",uds_country)) %>% group_by(uds_country,country_name, GW_startdate, GWn, uds_ccode, date_matches) %>% summarise(min(year), max(year),n())
# uds %>% filter(grepl("Serbia",uds_country)) %>% group_by(uds_country,country_name, GW_startdate, GWn, uds_ccode, date_matches) %>% summarise(min(year), max(year),n())
# uds %>% filter(grepl("Yugosla",uds_country)) %>% group_by(uds_country,country_name, GW_startdate, GWn, uds_ccode, date_matches) %>% summarise(min(year), max(year),n())
# uds %>% filter(grepl("Yemen",uds_country)) %>% group_by(uds_country,country_name, GW_startdate, GWn, uds_ccode, date_matches) %>% summarise(min(year), max(year),n())
# uds %>% filter(grepl("Viet",uds_country)) %>% group_by(uds_country,country_name, GW_startdate, GWn, uds_ccode, date_matches) %>% summarise(min(year), max(year),n())
# uds %>% filter(grepl("Haiti",uds_country)) %>% group_by(uds_country,country_name, GW_startdate, GWn, uds_ccode, date_matches) %>% summarise(min(year), max(year),n())
# uds %>% filter(GWn != uds_ccode) %>% group_by(uds_country,country_name, GW_startdate, GWn, uds_ccode) %>% summarise(min(year), max(year),n())

uds <- uds %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

devtools::use_data(uds, overwrite = TRUE)
