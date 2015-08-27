library(dplyr)
library(lubridate)

expanded_gdp <- read.table("../Data/gdpv6.txt",header=TRUE)

expanded_gdp <- expanded_gdp %>% rename(GWn = statenum)

added_columns <- mutate(PoliticalDatasets::data, 
                        GW_enddate = plyr::mapvalues(GW_enddate,from = NA, to = as.character(round_date(now(),unit = "day"))), 
                        start_year = year(GW_startdate), 
                        end_year = year(GW_enddate)) %>% 
  select(country_name,GWn,GWc,region,continent,microstate,lon,lat) %>%
  distinct()


expanded_gdp <- left_join(expanded_gdp,added_columns) 

rm(added_columns)

expanded_gdp$origin <- plyr::mapvalues(expanded_gdp$origin, from = c(0, -1, -2, 1, 2, 3), to = c("PWT8.0","PWT5.6","Maddison","Imputed based on first/last available","Interpolated within series","WDI")) 

devtools::use_data(expanded_gdp, overwrite=TRUE)