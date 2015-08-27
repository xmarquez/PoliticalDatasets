library(foreign)
library(dplyr)

bmr <-
  read.dta("https://sites.google.com/site/mkmtwo/democracy-v2.0.dta?attredirects=0")
bmr <- bmr %>% mutate(transition = as.factor(democracy_trans))
levels(bmr$transition) <- c("To dictatorship",NA,"To democracy")

bmr <-
  bmr %>% rename(
    bmr_country = country, bmr_ccode = ccode, bmr_abbreviation = abbreviation
  )

match_condition <-
  c(
    "ifelse(!problem_history & country_name != 'Germany (Prussia)'  & bmr_country != 'KOREA' | bmr_country %in% c('ETHIOPIA','ETHIOPIA  (INCL. ERIT)','ITALY','PAKISTAN','PAKISTAN  (INCL. BANGLAD.)','RUSSIA','USSR','SARDINIA','MONTENEGRO'), date_matches == max(date_matches) & !duplicated(date_matches),TRUE)","!(bmr_country == 'VIETNAM, SOUTH' & country_name == 'Vietnam, Democratic Republic of')","!(bmr_country == 'GERMANY, WEST' & country_name == 'Germany (Prussia)')","ifelse(bmr_country == 'GERMANY' & year <= 1945,code_matches,TRUE)","ifelse(bmr_country == 'GERMANY' & year > 1945,date_matches == max(date_matches) & !duplicated(date_matches),TRUE)","ifelse(bmr_country == 'GERMANY, WEST' & year > 1945,!duplicated(date_matches),TRUE)","ifelse(bmr_country == 'SERBIA' & year < 1879,GW_startdate == '1878-07-13',TRUE)","ifelse(bmr_country == 'SERBIA' & year > 1878, date_matches == max(date_matches), TRUE)","ifelse(bmr_country == 'SERBIA' & year %in% c(1916,1917), GW_startdate == '1878-07-13', TRUE)","ifelse(bmr_country == 'SERBIA' & year == 2006, GW_startdate == '2006-06-05', TRUE)", "ifelse(bmr_country == 'YUGOSLAVIA' & year == 1991, !duplicated(date_matches), TRUE)","ifelse(bmr_country == 'KOREA',code_matches,TRUE)"
  )

bmr <-
  PoliticalDatasets::to_gw_system(
    bmr, country_col = "bmr_country", code_col = "bmr_ccode", match_condition = match_condition
  )

# bmr %>% filter(grepl("GERMANY",bmr_country)) %>% group_by(bmr_country,country_name, GW_startdate, GWn, bmr_ccode, date_matches) %>% summarise(min(year), max(year),n())
# bmr %>% filter(grepl("SERBIA",bmr_country)) %>% group_by(bmr_country,country_name, GW_startdate, GWn, bmr_ccode, date_matches) %>% summarise(min(year), max(year),n())
# bmr %>% filter(grepl("YUGOSLAVIA",bmr_country)) %>% group_by(bmr_country,country_name, GW_startdate, GWn, bmr_ccode, date_matches) %>% summarise(min(year), max(year),n())
# bmr %>% filter(grepl("YEMEN",bmr_country)) %>% group_by(bmr_country,country_name, GW_startdate, GWn, bmr_ccode, date_matches) %>% summarise(min(year), max(year),n())
# bmr %>% filter(grepl("VIETNAM",bmr_country)) %>% group_by(bmr_country,country_name, GW_startdate, GWn, bmr_ccode, date_matches) %>% summarise(min(year), max(year),n())
# bmr %>% filter(grepl("HAITI",bmr_country)) %>% group_by(bmr_country,country_name, GW_startdate, GWn, bmr_ccode, date_matches) %>% summarise(min(year), max(year),n())
# bmr %>% filter(grepl("KOREA",bmr_country)) %>% group_by(bmr_country,country_name, GW_startdate, GWn, bmr_ccode, date_matches) %>% summarise(min(year), max(year),n())
# bmr %>% filter(GWn != bmr_ccode, !microstate) %>% group_by(bmr_country,country_name, GW_startdate, bmr_ccode, GWn) %>% summarise(min(year), max(year),n())

bmr <- bmr %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -code_matches, -num_matches, -problem_history,-num_periods)

devtools::use_data(bmr, overwrite = TRUE)
