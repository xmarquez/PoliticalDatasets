library(dplyr)
library(PoliticalDatasets)
library(reshape2)

load("../Pemstein Meserve and Melton Code/uds/data/democracy1946.2008.rda")

democracy <-  democracy %>% rename(uds_country = country, uds_ccode = cowcode)

original.pmm.democracy.data.1946.2008 <- democracy

devtools::use_data(original.pmm.democracy.data.1946.2008, overwrite = TRUE)

democracy <- to_gw_system(democracy, country_col = "uds_country", include_extras = FALSE)

democracy <- democracy %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods)

# First we create the panel
democracy <- democracy %>% ungroup() %>% select(-freedomhouse, -polity, -mainwaring, -prc, -blm, -vanhanen, -uds_country, -uds_ccode)

freedomhouse <- fh %>% ungroup() %>% select(country_name, year, GWn, fh_total_reversed, in_system) %>% mutate(freedomhouse = (fh_total_reversed+2)/2) %>% select(-fh_total_reversed) %>% filter(!is.na(freedomhouse))

polity_annual <- polity_annual %>% ungroup() %>% select(country_name,GWn,year, polity, polity2,in_system) %>% filter(!is.na(polity) | !is.na(polity2))

bmr <- bmr %>% ungroup() %>% select(country_name, year, GWn, democracy, democracy_omitteddata, in_system) %>% rename(bmr_all = democracy, bmr_omitted = democracy_omitteddata) %>% filter(!is.na(bmr_all) | !is.na(bmr_omitted))

mainwaring <- mainwaring %>% select(country_name, year, GWn, mainwaring, in_system)

gasiorowski <- gasiorowski %>% select(country_name, year, GWn, prc, in_system)

blm <- blm %>% select(country_name, year, GWn, blm, in_system)

vanhanen <- vanhanen %>% filter(grepl("Index",variable)) %>% select(country_name, year, GWn, vanhanen, in_system) 


svolik <- SvolikRegimeAll_cy %>% ungroup() %>% select(country_name, year, GWn, regime, in_system) %>% rename(svolik = regime) %>% mutate(svolik = as.character(svolik), svolik = ifelse(svolik == "democracy", 2, ifelse(svolik == "dictatorship", 1, NA)))

gwf <- all_gwf %>% select(gwf_casename, country_name, GWn, year, gwf_full_regimetype, gwf_spell, in_system) %>% group_by(gwf_casename) %>% mutate(max_year = max(year), min_year = max_year - gwf_spell) %>% select(gwf_casename,country_name, GWn, gwf_full_regimetype, max_year, min_year) %>% distinct() %>% group_by(gwf_casename, country_name, GWn, gwf_full_regimetype, min_year, max_year) %>% do(data.frame(year = .$max_year:.$min_year)) %>% ungroup() %>% select(-gwf_casename,-max_year, -min_year) %>% arrange(country_name,year)

gwf <- gwf %>% mutate(gwf = plyr::mapvalues(gwf_full_regimetype, from = levels(gwf$gwf_full_regimetype), to = c(2,NA,1,1,1,1,NA,1,1,1,1,1,1,NA,NA,NA)), gwf = as.numeric(as.character(gwf))) %>% select(-gwf_full_regimetype)

democracy <- merge(democracy,freedomhouse, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,bmr, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,svolik, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,gwf, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,mainwaring, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,gasiorowski, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,blm, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,vanhanen, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- merge(democracy,polity_annual, all.x=TRUE,all.y=TRUE)

democracy <- distinct(democracy)

democracy <- democracy %>% arrange(country_name,year)

rm(bmr, polity_annual, freedomhouse, gwf, svolik, mainwaring, gasiorowski, blm, vanhanen)

democracy <- left_join(democracy, PoliticalDatasets::data %>% select(country_name,GWc, region, continent, microstate, lon, lat))

democracy <- democracy %>% select(country_name, GWn, GWc, year, arat:vanhanen, freedomhouse:polity2, in_system, region:lat)

democracy_long <- melt(democracy, id.vars = c("country_name", "GWn", "GWc", "year", "in_system", "region", "continent", "microstate", "lon", "lat")) 

democracy_long <- democracy_long %>% filter(!is.na(value))

devtools::use_data(democracy, overwrite = TRUE)
devtools::use_data(democracy_long, overwrite = TRUE)
