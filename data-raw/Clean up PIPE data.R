library(foreign)
library(dplyr)

PIPE <- read.dta("https://sites.google.com/a/nyu.edu/adam-przeworski/home/data/PIPE_081813.dta?attredirects=0&d=1")

PIPE <- PIPE %>%
  rename(country_number = country) %>%
  group_by(country_number) %>%
  mutate(countryn = first(countryn[ !is.na(countryn) & countryn != "" ]),cowcodes = ifelse(is.na(cowcodes),first(cowcodes[ !is.na(cowcodes) ]),cowcodes)) %>% 
  ungroup() %>% 
  filter(!is.na(countryn)) 

match_condition <-  c("ifelse(countryn != 'Vietnam', date_matches == max(date_matches) & !duplicated(date_matches),TRUE)","ifelse(countryn == 'Vietnam' & year < 1954, GWn == 815,date_matches == max(date_matches) & !duplicated(date_matches))")

PIPE <- PoliticalDatasets::to_gw_system(PIPE, country_col = "countryn", match_condition = match_condition)

# PIPE %>% ungroup() %>% select(country_number, countryn, country_name, cowcodes, GWn) %>% distinct() %>% filter(country_name != countryn) %>% data.frame()

# PIPE %>% group_by(country_number, countryn, country_name, cowcodes, GWn) %>% filter(cowcodes !=GWn | is.na(cowcodes)) %>% summarise(min(year), max(year))
# 
# PIPE %>% group_by(country_number, countryn, country_name, cowcodes, GWn) %>% filter(grepl("German",country_name,ignore.case=TRUE)) %>% summarise(min(year), max(year))
# 
# PIPE %>% group_by(country_number, countryn, country_name, cowcodes, GWn) %>% filter(grepl("Vietn",country_name,ignore.case=TRUE)) %>% summarise(min(year), max(year))
# 
# PIPE %>% group_by(country_number, countryn, country_name, cowcodes, GWn) %>% filter(grepl("Hunga",country_name,ignore.case=TRUE)) %>% summarise(min(year), max(year))

PIPE <- PIPE %>% ungroup() %>% mutate(country_name = ifelse(is.na(country_name),countryn,country_name), country_name = plyr::mapvalues(country_name,from=c("west indies f","leeward island f"),to=c("West Indies Federation","Leeward Islands Federation")))

PIPE <- PIPE %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

devtools::use_data(PIPE, overwrite = TRUE)
