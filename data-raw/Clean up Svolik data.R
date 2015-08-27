library(foreign)
library(dplyr)
library(lubridate)

dotsToNA <- function(x) {
  plyr::mapvalues(x, from = c("."),to = c(NA))
}

SvolikCoalition <-
  read.dta(
    "../Data/leader and ruling coalition data/ruling coalitions in dictatorships, 1946-2008.dta"
  )
SvolikLeader <-
  read.dta(
    "../Data/leader and ruling coalition data/leadership change in dictatorships, 1946-2008.dta"
  )
SvolikInstitutions <-
  read.dta(
    "../Data/institutions in dictatorships, 1946-2008/institutions in dictatorships, 1946-2008.dta"
  )
SvolikRegimeAll <-
  read.dta("../Data/regime data/regime and no authority spells, 1946-2008.dta")
SvolikAuthoritarianSpells <-
  read.dta("../Data/regime data/authoritarian spells, 1946-2008.dta")
SvolikNoAuthority <-
  read.dta("../Data/regime data/no authority spells, 1946-2008.dta")

SvolikInstitutions <- plyr::colwise(dotsToNA)(SvolikInstitutions)
SvolikLeader <- plyr::colwise(dotsToNA)(SvolikLeader)
SvolikCoalition <- plyr::colwise(dotsToNA)(SvolikCoalition)
SvolikRegimeAll <- plyr::colwise(dotsToNA)(SvolikRegimeAll)
SvolikAuthoritarianSpells <-
  plyr::colwise(dotsToNA)(SvolikAuthoritarianSpells)
SvolikNoAuthority <- plyr::colwise(dotsToNA)(SvolikNoAuthority)

SvolikLeader$exit_summary <-
  factor(SvolikLeader$cex,labels = c("Irregular","Regular"))
SvolikLeader$entry_summary <-
  factor(SvolikLeader$cen,labels = c("Irregular","Regular"))

SvolikLeader <- SvolikLeader %>% rename(svolik_ccode = ccode)
SvolikInstitutions <-
  SvolikInstitutions %>% rename(svolik_ccode = ccode)
SvolikCoalition <- SvolikCoalition %>% rename(svolik_ccode = ccode)
SvolikRegimeAll <- SvolikRegimeAll %>% rename(svolik_ccode = ccode)
SvolikAuthoritarianSpells <-
  SvolikAuthoritarianSpells %>% rename(svolik_ccode = ccode)
SvolikNoAuthority <-
  SvolikNoAuthority %>% rename(svolik_ccode = ccode)

SvolikRegimeAll_cy <-
  SvolikRegimeAll %>% group_by(cname) %>% mutate(num = seq_along(cname)) %>% group_by(cname, num) %>% do(data.frame(.,year = .$start_year:.$end_year))

SvolikRegimeAll_cy <-
  PoliticalDatasets::to_gw_system(SvolikRegimeAll_cy, country_col = "cname")

# SvolikRegimeAll_cy %>% filter(GWn != svolik_ccode) %>% group_by(cname,country_name, GW_startdate, svolik_ccode, GWn) %>% summarise(min(year), max(year),n())

SvolikRegimeAll <-
  SvolikRegimeAll_cy %>% group_by(GWn,num) %>% mutate(start_year = min(year), end_year = max(year)) %>% select(-year) %>% ungroup() %>% distinct() %>% mutate(startdate = ymd(paste0(start_year,"-1-1")), enddate = ymd(paste0(end_year,"-12-31")))

# SvolikRegimeAll %>% filter(grepl("Germa",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(startdate), max(enddate),n())
# SvolikRegimeAll %>% filter(grepl("Serbia",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(startdate), max(enddate),n())
# SvolikRegimeAll %>% filter(grepl("Yugoslavia",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(startdate), max(enddate),n())
# SvolikRegimeAll %>% filter(grepl("Yemen",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(startdate), max(enddate),n())
# SvolikRegimeAll %>% filter(grepl("Viet",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(startdate), max(enddate),n())
# SvolikRegimeAll %>% filter(grepl("Korea",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(startdate), max(enddate),n())
# SvolikRegimeAll %>% filter(GWn != svolik_ccode) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(startdate), max(enddate),n())

SvolikRegimeAll <- SvolikRegimeAll %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)
SvolikRegimeAll_cy <- SvolikRegimeAll_cy %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

SvolikAuthoritarianSpells <-
  SvolikAuthoritarianSpells %>% filter(cabb != "NAM") # Does not exist

SvolikAuthoritarianSpells$e_enddate[is.na(SvolikAuthoritarianSpells$e_enddate)] <-
  c("2008-12-31","1991-12-31","1980-12-31") # Dates for China, Tajikistan, and Uganda missing

SvolikAuthoritarianSpells$o_startdate[is.na(SvolikAuthoritarianSpells$o_startdate)] <-
  "1947-08-14" # Date of Pakistan independence

SvolikAuthoritarianSpells$o_startdate[SvolikAuthoritarianSpells$o_startdate  == "1945-09-02"] <-
  "1954-05-01" # Ho Chi Minh entry and exit date are wrong

SvolikAuthoritarianSpells$e_enddate[SvolikAuthoritarianSpells$e_enddate  == "1945-09-02"] <-
  "1955-11-01" # Ho Chi Minh entry and exit date are wrong

SvolikAuthoritarianSpells$o_startdate[SvolikAuthoritarianSpells$o_startdate  == "1967-10-04"] <-
  "1984-01-01" # Brunei entry and exit dates are wrong

SvolikAuthoritarianSpells$e_enddate[SvolikAuthoritarianSpells$e_enddate  == "1967-10-04"] <-
  "2008-12-31" # Brunei entry and exit dates are wrong

SvolikAuthoritarianSpells$svolik_ccode[SvolikAuthoritarianSpells$svolik_ccode  == 540 &
                                         SvolikAuthoritarianSpells$cabb == "ARG"] <-
  160 # SvolikCode for Argentina is wrong

SvolikAuthoritarianSpells <-
  PoliticalDatasets::to_gw_system(
    SvolikAuthoritarianSpells, country_col = "cname", date_col = c("o_startdate", "e_enddate")
  )
# SvolikAuthoritarianSpells %>% filter(grepl("Germa",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(o_startdate), max(e_enddate),n())
# SvolikAuthoritarianSpells %>% filter(grepl("Serbia",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(o_startdate), max(e_enddate),n())
# SvolikAuthoritarianSpells %>% filter(grepl("Yugoslavia",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(o_startdate), max(e_enddate),n())
# SvolikAuthoritarianSpells %>% filter(grepl("Sudan",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(o_startdate), max(e_enddate),n())
# SvolikAuthoritarianSpells %>% filter(grepl("Argentina",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(o_startdate), max(e_enddate),n())
# SvolikAuthoritarianSpells %>% filter(grepl("Brunei",cname)) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(o_startdate), max(e_enddate),n())
# SvolikAuthoritarianSpells %>% filter(GWn != svolik_ccode) %>% group_by(cname,country_name, GW_startdate, GWn, svolik_ccode, date_matches) %>% summarise(min(o_startdate), max(e_enddate),n())

SvolikAuthoritarianSpells <- SvolikAuthoritarianSpells %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

SvolikInstitutions <-
  left_join(SvolikInstitutions, SvolikRegimeAll_cy)

SvolikCoalition <-
  merge_by_date_interval(
    SvolikCoalition %>% rename(svolik_cabb = cabb), SvolikAuthoritarianSpells %>% ungroup() %>% select(svolik_ccode, o_startdate, e_enddate,country_name:in_system), key_col = "svolik_ccode", "rc_start", "rc_end", "o_startdate","e_enddate","rc_id"
  )
SvolikCoalition <-
  SvolikCoalition %>% rename(svolik_ccode = key_col, rc_start = startdate1, rc_end = enddate1, o_startdate = startdate2, e_enddate = enddate2)

# Get rid of 1-day matches
SvolikCoalition <- SvolikCoalition %>% filter(rc_end != o_startdate)
SvolikCoalition <- SvolikCoalition %>% filter(rc_start != e_enddate)
SvolikCoalition <- SvolikCoalition %>% filter(!duplicated(rc_id))
SvolikCoalition <- SvolikCoalition %>% select(-o_startdate,-e_enddate)

SvolikLeader <-
  merge_by_date_interval(
    SvolikLeader %>% rename(svolik_cabb = cabb), SvolikAuthoritarianSpells  %>% ungroup() %>% select(svolik_ccode,country_name:in_system) %>% distinct(), key_col = "svolik_ccode", "startdate", "enddate", "GW_startdate","GW_enddate","leadid"
  )

SvolikLeader <-
  SvolikLeader %>% rename(
    svolik_ccode = key_col, startdate = startdate1, enddate = enddate1, GW_startdate = startdate2, GW_enddate = enddate2
  )

SvolikLeader <- SvolikLeader %>% filter(!duplicated(leadid))

SvolikNoAuthority <-
  PoliticalDatasets::to_gw_system(
    SvolikNoAuthority %>% mutate(startdate = ymd(paste0(start_year,"-1-1")), enddate = ymd(paste0(end_year,"-12-31"))), country_col = "cname", date_col = c("startdate", "enddate")
  )

devtools::use_data(SvolikLeader, overwrite = TRUE)
devtools::use_data(SvolikInstitutions, overwrite = TRUE)
devtools::use_data(SvolikCoalition, overwrite = TRUE)
devtools::use_data(SvolikRegimeAll, overwrite = TRUE)
devtools::use_data(SvolikRegimeAll_cy, overwrite = TRUE)
devtools::use_data(SvolikAuthoritarianSpells, overwrite = TRUE)
devtools::use_data(SvolikNoAuthority, overwrite = TRUE)
