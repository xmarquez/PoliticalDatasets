library(dplyr)
library(PoliticalDatasets)

mainwaring <- read.delim("../Data/Mainwaring Linan.txt")

mainwaring <- mainwaring %>% group_by(Country,Regime,Elections,Franchise,Civil.Liberties,Civilian.Power,From,To) %>% do(data.frame(year = .$From:.$To)) %>% ungroup() %>% arrange(Country,year)

mainwaring <- PoliticalDatasets::to_gw_system(mainwaring, country_col = "Country")

mainwaring <- mainwaring %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods) %>% ungroup() %>% select(-Country)

mainwaring <- mainwaring %>% select(country_name,GWn,GWc,year,Regime:Civilian.Power,From,To,region:in_system)

mainwaring <- mainwaring %>% mutate(mainwaring = plyr::mapvalues(Regime, from = c("A","SD","D"), to=c(-1,0,1)), mainwaring = as.numeric(as.character(mainwaring)))

devtools::use_data(mainwaring, overwrite = TRUE)
