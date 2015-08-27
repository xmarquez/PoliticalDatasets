library(dplyr)
library(foreign)

autocratic_gwf <- read.dta("../Data/GWF Autocratic Regimes 1.2/GWFcases.dta")
all_gwf <- read.dta("../Data/GWF Autocratic Regimes 1.2/GWF_AllPoliticalRegimes.dta")

autocratic_gwf_events <- read.delim("./data-raw/GWF start-end events.txt")
autocratic_gwf_events_cases <- grep("^[A-Z].*\\([0-9]+-.*\\)",autocratic_gwf_events$gwf_case,value=TRUE,perl=TRUE)
# data.frame(autocratic_gwf_events_cases,autocratic_gwf$gwf_casename)
autocratic_gwf_events$gwf_casename <- qdap::mgsub(autocratic_gwf_events_cases,autocratic_gwf$gwf_casename,autocratic_gwf_events$gwf_case)
autocratic_gwf <- left_join(autocratic_gwf, autocratic_gwf_events)

all_gwf <- all_gwf %>% rename(gwf_cowcode = cowcode) %>% mutate(gwf_regimetype = as.factor(plyr::mapvalues(gwf_regimetype, from="NA", to=NA)), gwf_nonautocracy = as.factor(plyr::mapvalues(gwf_nonautocracy, from="NA", to=NA)), gwf_full_regimetype = as.factor(ifelse(is.na(gwf_regimetype),as.character(gwf_nonautocracy),as.character(gwf_regimetype))))

autocratic_gwf <- autocratic_gwf %>% mutate(gwf_startdate = ymd(gwf_startdate), gwf_enddate = ymd(gwf_enddate))

autocratic_gwf <- PoliticalDatasets::to_gw_system(autocratic_gwf, country_col = "gwf_country", date_col = c("gwf_startdate","gwf_enddate"))

autocratic_gwf$gwf_howend <- plyr::mapvalues(autocratic_gwf$gwf_howend, from=0:9, c("0: regime still in power on December 31, 2010","1: regime insiders change rules of regime","2: incumbent loses elections","3: no incumbent runs in competitive election won by opponent","4: popular uprising","5: military coup","6: insurgents, revolutionaries, or combatants fighting a civil war","7: foreign imposition or invasion","8: new autocratic leader selected, changes rules, and remains in power","9: state ceases to exist ends or government fails to control most of the territory"))

autocratic_gwf$gwf_howend <- as.factor(autocratic_gwf$gwf_howend)

autocratic_gwf$gwf_violent <- plyr::mapvalues(autocratic_gwf$gwf_violent, from=0:4, c("0: regime still in power on December 31, 2010","1: no deaths","2: 1-25 deaths","3: 26-1000 deaths","4: >1000"))

autocratic_gwf$gwf_violent <- as.factor(autocratic_gwf$gwf_violent)

# autocratic_gwf %>% filter(grepl("Vietnam",gwf_country)) %>% ungroup() %>% select(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn,gwf_startdate,GW_startdate)
# autocratic_gwf %>% filter(grepl("German",gwf_country)) %>% ungroup() %>% select(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn,gwf_startdate,GW_startdate)
# autocratic_gwf %>% filter(grepl("Serbia",gwf_country)) %>% ungroup() %>% select(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn,gwf_startdate,GW_startdate)
# autocratic_gwf %>% filter(grepl("Yugos",gwf_country)) %>% ungroup() %>% select(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn,gwf_startdate,GW_startdate)
# autocratic_gwf %>% filter(grepl("Yemen",gwf_country)) %>% ungroup() %>% select(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn,gwf_startdate,GW_startdate)

autocratic_gwf <- autocratic_gwf %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, - problem_history,-num_periods)

all_gwf <- PoliticalDatasets::to_gw_system(all_gwf, country_col = "gwf_country")

# all_gwf %>% filter(grepl("Vietnam",gwf_country)) %>% ungroup() %>% group_by(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn) %>% summarise(min_year= min(year),max_year = max(year))
# all_gwf %>% filter(grepl("German",gwf_country)) %>% ungroup() %>% group_by(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn) %>% summarise(min_year= min(year),max_year = max(year))
# all_gwf %>% filter(grepl("Serbia",gwf_country)) %>% ungroup() %>% group_by(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn) %>% summarise(min_year= min(year),max_year = max(year))
# all_gwf %>% filter(grepl("Yugos",gwf_country)) %>% ungroup() %>% group_by(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn) %>% summarise(min_year= min(year),max_year = max(year))
# all_gwf %>% filter(grepl("Yemen",gwf_country)) %>% ungroup() %>% group_by(gwf_casename,gwf_country,country_name,gwf_cowcode,GWn) %>% summarise(min_year= min(year),max_year = max(year))

all_gwf <- all_gwf %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, - problem_history,-num_periods)

missing <- anti_join(autocratic_gwf %>% ungroup() %>% select(country_name,gwf_casename),all_gwf %>% select(country_name,gwf_casename)) 

missing_casenames <- all_gwf %>% ungroup() %>% filter(country_name %in% missing$country_name) %>% distinct(gwf_casename) 
missing_casenames <- missing_casenames %>% rename(gwf_casename_missing = gwf_casename) %>% select(country_name,gwf_casename_missing)
missing_casenames <- left_join(missing,missing_casenames) %>% select(gwf_casename,gwf_casename_missing)
missing_casenames <- missing_casenames[c(6,7,8,13,15,16,18,24,27,32,33,36),]
all_gwf$gwf_casename <- plyr::mapvalues(all_gwf$gwf_casename, from = missing_casenames$gwf_casename_missing, to = missing_casenames$gwf_casename)

all_gwf_periods <- all_gwf %>% group_by(gwf_casename, country_name, GWn, gwf_spell, gwf_full_regimetype) %>% summarise(gwf_startdate2 = ymd(paste0(min(year),"-01-01")),gwf_enddate2 = ymd(paste0(max(year),"-12-31")))
all_gwf_periods <- left_join(all_gwf_periods, autocratic_gwf)
all_gwf_periods <- all_gwf_periods %>% group_by(country_name) %>% arrange(gwf_startdate2)
all_gwf_periods <- all_gwf_periods %>% mutate(next_startdate = lead(as.character(gwf_startdate)), prev_enddate = lag(as.character(gwf_enddate)))
all_gwf_periods <- all_gwf_periods %>% group_by(gwf_casename) %>% mutate(gwf_startdate = ifelse(is.na(gwf_startdate),prev_enddate,as.character(gwf_startdate)), gwf_enddate = ifelse(is.na(gwf_enddate),next_startdate,as.character(gwf_enddate))) 
all_gwf_periods <- all_gwf_periods %>% group_by(gwf_casename) %>% mutate(gwf_startdate = ifelse(is.na(gwf_startdate),as.character(gwf_startdate2),gwf_startdate), gwf_enddate = ifelse(is.na(gwf_enddate),as.character(gwf_enddate2),gwf_enddate)) %>% select(-gwf_startdate2,-gwf_enddate2,-prev_enddate,-next_startdate,-gwf_country,-gwf_regimetype, -cowcode)
all_gwf_periods <- all_gwf_periods %>% ungroup() %>% mutate(gwf_startdate = ymd(gwf_startdate), gwf_enddate = ymd(gwf_enddate))

# all_gwf_periods %>% filter(country_name == "Lesotho")
# all_gwf_periods %>% filter(country_name == "Laos")
# all_gwf_periods %>% filter(country_name == "Haiti")
# anti_join(autocratic_gwf %>% ungroup() %>% select(country_name,gwf_casename),all_gwf_periods %>% select(country_name,gwf_casename))
# anti_join(all_gwf %>% ungroup() %>% select(country_name,gwf_casename),all_gwf_periods %>% select(country_name,gwf_casename))
# anti_join(autocratic_gwf ,all_gwf_periods)

devtools::use_data(autocratic_gwf, overwrite=TRUE)
devtools::use_data(all_gwf, overwrite=TRUE)
devtools::use_data(all_gwf_periods, overwrite=TRUE)
