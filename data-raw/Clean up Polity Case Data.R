library(foreign)
library(dplyr)
library(lubridate)

polity_cases <- read.spss("http://www.systemicpeace.org/inscr/p4v2013d.sav",to.data.frame=TRUE)

polity_cases <- polity_cases %>% mutate(polity_startdate = mdy(paste0(bmonth,"-",bday,"-",byear)),
                                        polity_enddate = mdy(paste0(emonth,"-",eday,"-",eyear)), 
                                        xrreg = as.numeric(xrreg),
                                        xrcomp = as.numeric(xrcomp),
                                        xropen = as.numeric(xropen), 
                                        xconst=as.numeric(xconst),
                                        parreg=as.numeric(parreg),
                                        parcomp=as.numeric(parcomp),
                                        exrec=factor(exrec,
                                                     labels=c("Transition",
                                                              "Interruption",
                                                              "Interregnum", 
                                                              names(sort(attr(exrec,"value.labels"))))),
                                        exconst=factor(exconst,
                                                       labels=c("Transition",
                                                                "Interruption",
                                                                "Interregnum",
                                                                names(sort(attr(exconst,"value.labels"))))),
                                        polcomp=factor(polcomp,
                                                       labels=c("Transition",
                                                                "Interruption",
                                                                "Interregnum",
                                                                names(sort(attr(polcomp,"value.labels"))))),
                                        polity_enddate = ymd(ifelse(is.na(polity_enddate),"2013-12-31",as.character(polity_enddate))),
                                        country = stringr::str_trim(country)) %>%
  rename(polity_country = country, polity_ccode = ccode)

match_condition <-  c("ifelse(!polity_country %in% c('Germany West','Serbia'), date_matches == max(date_matches) & !duplicated(date_matches),TRUE)","!(polity_country == 'Germany West' & country_name == 'Germany (Prussia)')","ifelse(polity_country == 'Germany West' & polity_startdate >= lubridate::ymd('1945-05-08'),problem_history,TRUE)","ifelse(polity_country == 'Serbia' & polity_enddate <= lubridate::ymd('1920-12-31') ,country_name != 'Yugoslavia' & GW_startdate == '1878-07-13',TRUE)", "ifelse(polity_country == 'Serbia' & polity_startdate >= lubridate::ymd('2006-06-03') ,country_name != 'Yugoslavia' & GW_startdate == '2006-06-05',TRUE)")


polity_cases <- PoliticalDatasets::to_gw_system(polity_cases, country_col = "polity_country", date_col = c("polity_startdate","polity_enddate"), code_col = "polity_ccode", match_condition = match_condition)

# polity_cases %>% filter(grepl("German",polity_country)) %>% group_by(polity_country,country_name, GW_startdate, GWn, polity_ccode, date_matches) %>% summarise(min(polity_startdate), max(polity_enddate),n())
# 
# polity_cases %>% filter(grepl("Vietnam",polity_country)) %>% group_by(polity_country,country_name, GW_startdate, GWn, polity_ccode, date_matches) %>% summarise(min(polity_startdate), max(polity_enddate),n())
# 
# polity_cases %>% filter(grepl("Serbia",polity_country)) %>% group_by(polity_country,country_name, GW_startdate, GW_enddate,GWn, polity_ccode, date_matches) %>% summarise(min(polity_startdate), max(polity_enddate),n())
# 
# polity_cases %>% filter(GWn != polity_ccode, !microstate) %>% group_by(polity_country,country_name, GW_startdate, polity_ccode, GWn) %>% summarise(min(polity_startdate), max(polity_enddate),n())


polity_annual <- read.spss("http://www.systemicpeace.org/inscr/p4v2014.sav",to.data.frame=TRUE)

polity_annual <- polity_annual %>% mutate(country = stringr::str_trim(country)) %>%
  rename(polity_country = country, polity_ccode = ccode)


match_condition <-  c("ifelse(!polity_country %in% c('Germany West','Serbia'), date_matches == max(date_matches) & !duplicated(date_matches),TRUE)","!(polity_country == 'Germany West' & country_name == 'Germany (Prussia)')","ifelse(polity_country == 'Germany West' & year >= 1945,problem_history,TRUE)","ifelse(polity_country == 'Serbia' & year <= 1920 ,country_name != 'Yugoslavia' & GW_startdate == '1878-07-13',TRUE)", "ifelse(polity_country == 'Serbia' & year >= 2006 ,country_name != 'Yugoslavia' & GW_startdate == '2006-06-05',TRUE)")

polity_annual <- PoliticalDatasets::to_gw_system(polity_annual, country_col = "polity_country", code_col = "polity_ccode", match_condition = match_condition, include_extras = TRUE)

rm(match_condition)

polity_cases <- polity_cases %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -code_matches, -num_matches, -problem_history,-num_periods)

polity_annual <- polity_annual %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -code_matches, -num_matches, -problem_history,-num_periods)

devtools::use_data(polity_cases, overwrite = TRUE)
devtools::use_data(polity_annual, overwrite = TRUE)
