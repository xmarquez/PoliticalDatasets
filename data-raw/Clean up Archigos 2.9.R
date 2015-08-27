library(foreign)
library(dplyr)
library(lubridate)

archigos2.9 <- read.dta("http://www.rochester.edu/college/faculty/hgoemans/Archigos_2.9-Public.dta")

archigos2.9 <- archigos2.9 %>% 
  mutate(startdate = dmy(startdate),
         enddate = dmy(enddate),
         died = dmy(died)) %>%
  rename(GWn = ccode)

added_columns <- mutate(PoliticalDatasets::data, 
                        GW_enddate = plyr::mapvalues(GW_enddate,from = NA, to = as.character(round_date(now(),unit = "day")))) %>% 
  select(country_name,GWn,GWc,region,continent,microstate,lon,lat) %>%
  distinct()

archigos2.9 <- left_join(archigos2.9,added_columns)

rm(added_columns)

devtools::use_data(archigos2.9, overwrite = TRUE)