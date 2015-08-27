library(dplyr)
library(lubridate)

PowellThyne <- read.table("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt",header=TRUE) %>% 
  rename(powell_country = country, powell_ccode = ccode) %>% 
  mutate(attempt_type = factor(coup,labels=c("Unsuccessful","Successful")),
         date = ymd(paste(year,month,day,sep="-")))

PowellThyne <- PoliticalDatasets::to_gw_system(PowellThyne, country_col = "powell_country", date_col = "date")

PowellThyne <- PowellThyne %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, - problem_history,-num_periods)

devtools::use_data(PowellThyne, overwrite = TRUE)
