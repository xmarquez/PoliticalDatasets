library(dplyr)
library(lubridate)

archigos2014 <- read.delim("../Data/15Jan15_Archigos_4.0.txt") %>% 
  mutate(startdate = ymd(startdate),
         enddate = ymd(enddate),
         borndate = ymd(borndate),
         deathdate = ymd(deathdate)) %>%
  rename(GWn = ccode)

added_columns <- mutate(PoliticalDatasets::data, 
                        GW_enddate = plyr::mapvalues(GW_enddate,from = NA, to = as.character(round_date(now(),unit = "day")))) %>% 
  select(country_name,GWn,GWc,region,continent,microstate,lon,lat) %>%
  distinct()

archigos2014 <- left_join(archigos2014,added_columns)

rm(added_columns)

devtools::use_data(archigos2014, overwrite = TRUE)