library(dplyr)
library(foreign)
library(lubridate)

nelda <- read.dta("../Data/NELDA_version3/nelda.dta")

nelda <- nelda %>% 
  mutate(date = ymd(paste0(year,ifelse(mmdd < 999, paste0("0",mmdd),mmdd)))) %>%
  rename(nelda_ccode = ccode, nelda_country = country)

convert_to_NA <- function(x) { plyr::mapvalues(x, from = c("N/A"),to = c(NA)) }

nelda <- plyr::colwise(convert_to_NA)(nelda)

nelda <- PoliticalDatasets::to_gw_system(nelda, country_col = "nelda_country", date_col = "date")

nelda <- nelda %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, - problem_history,-num_periods)

devtools::use_data(nelda, overwrite=TRUE)
