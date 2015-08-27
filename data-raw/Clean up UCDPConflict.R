

load(url("http://www.pcr.uu.se/digitalAssets/124/124920_1ucdpprioarmedconflictdataset4.2014a.rdata")) 

library(tidyr) # Need this for the "unnest" function
ucdpConflict <- ucdpConflict %>% 
  rename(year = Year) %>%
  mutate(GWNoLoc = strsplit(as.character(GWNoLoc), ", ")) %>%  
  unnest(GWNoLoc) %>% # Expands list values such as "33","45" so that 33 and 45 appear in 2 rows
  mutate(GWn = as.numeric(as.character(GWNoLoc))) %>% filter(GWn != -99)

ucdpConflict <- left_join(ucdpConflict,gw_system %>% rename(GWn = code, country_name = country)) 

ucdpConflict <- ucdpConflict %>% 
  group_by(country_name,ConflictId,SideB) %>%
  mutate(period_number =  (c(diff(IntensityLevel),0)), 
         period_number = cumsum(abs(period_number)),
         period_number = lag(period_number),
         period_number=ifelse(is.na(period_number),0,period_number)) %>% 
  group_by(country_name,ConflictId,SideB,period_number) %>% 
  mutate(start_year = min(year),end_year=max(year), 
         EpEndDate = ifelse(is.na(EpEndDate),paste0(end_year,"-12-31"),as.character(EpEndDate)),
         StartDate2 = ifelse(StartDate2 < start_year, paste0(start_year,"-01-01"),as.character(StartDate2)),startdate = ymd(StartDate2),enddate = ymd(EpEndDate)) %>% 
  ungroup() %>%
  distinct(ConflictId,country_name,IntensityLevel,startdate,enddate) %>%
  mutate(TypeOfConflict = factor(TypeOfConflict,labels=c("Extrasystemic/Colonial","Interstate","Internal","Internationalized internal")), IntensityLevel = factor(IntensityLevel,labels=c("Minor armed conflict","War")))

ucdpConflict$country_name[ is.na(ucdpConflict$country_name)] <- "India"

devtools::use_data(ucdpConflict, overwrite = TRUE)

