library(dplyr)

load(url("http://www.politicalterrorscale.org/Data/Files/PTS2014.RData")) 

PTS2014 <- PTS2014 %>% rename(pts_country = Country, year = Year)

match_condition <-
  c("date_matches == max(date_matches) & !duplicated(date_matches)")


PTS2014 <- PoliticalDatasets::to_gw_system(PTS2014, country_col = "pts_country", code_col = "COWnum", match_condition = match_condition)

# PTS2014 %>% filter(grepl("German",pts_country)) %>% group_by(pts_country,country_name, GW_startdate, GW_enddate, GWn, COWnum, date_matches) %>% summarise(min(year), max(year),n())
# PTS2014 %>% filter(grepl("Serbia",pts_country)) %>% group_by(pts_country,country_name, GW_startdate, GWn, COWnum, date_matches) %>% summarise(min(year), max(year),n())
# PTS2014 %>% filter(grepl("Yugosla",pts_country)) %>% group_by(pts_country,country_name, GW_startdate, GWn, COWnum, date_matches) %>% summarise(min(year), max(year),n())
# PTS2014 %>% filter(grepl("Yemen",pts_country)) %>% group_by(pts_country,country_name, GW_startdate, GWn, COWnum, date_matches) %>% summarise(min(year), max(year),n())
# PTS2014 %>% filter(grepl("Viet",pts_country)) %>% group_by(pts_country,country_name, GW_startdate, GWn, COWnum, date_matches) %>% summarise(min(year), max(year),n())
# PTS2014 %>% filter(grepl("Haiti",pts_country)) %>% group_by(pts_country,country_name, GW_startdate, GWn, COWnum, date_matches) %>% summarise(min(year), max(year),n())
# PTS2014 %>% filter(GWn != COWnum) %>% group_by(pts_country,country_name, GW_startdate, GWn, COWnum) %>% summarise(min(year), max(year),n())

PTS2014 <- PTS2014 %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

PTS2014 <- PTS2014 %>% group_by(COWnum,year) %>% mutate(avgScore = mean(c(Amnesty,StateDept,HRW),na.rm=TRUE))

devtools::use_data(PTS2014, overwrite = TRUE)
