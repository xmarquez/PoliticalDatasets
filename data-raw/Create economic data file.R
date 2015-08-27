library(PoliticalDatasets)
library(reshape2)
library(dplyr)


pwt8.1 <- pwt8.1 %>% ungroup() %>% select(country_name, GWn, year, pop, rgdpe, rgdpo, cgdpe, cgdpo, rgdpna, ccon, in_system) 
pwt8.0 <- pwt8.0 %>% ungroup() %>% select(country_name, GWn, year, pop, rgdpe, rgdpo, cgdpe, cgdpo, rgdpna, in_system) 

pwt8.1 <- melt(pwt8.1, measure.vars = c("rgdpe", "rgdpo", "cgdpe", "cgdpo", "rgdpna", "ccon"))
pwt8.0 <- melt(pwt8.0, measure.vars = c("rgdpe", "rgdpo", "cgdpe", "cgdpo", "rgdpna"))

pwt8.1 <- pwt8.1 %>% filter(!is.na(value)) %>% mutate(per_capita = value/pop, primary_source = "PWT8.1", origin = "PWT 8.1")

pwt8.1 <- pwt8.1 %>% mutate(variable = plyr::mapvalues(variable, from = c("rgdpe", "rgdpo", "cgdpe", "cgdpo", "rgdpna", "ccon"), to = c("PWT 8.1: Expenditure side, chained PPPs", "PWT 8.1: Output side, chained PPPs", "PWT 8.1: Expenditure side, current PPPs, 2005$", "PWT 8.1: Output side, current PPPs, 2005$", "PWT 8.1: National-accounts growth rates, 2005$", "PWT 8.1: Real consumption of households and\ngovernment, current PPPs, 2005$")))

pwt8.0 <- pwt8.0  %>% filter(!is.na(value)) %>% mutate(per_capita = value/pop, primary_source = "PWT8.0", origin = "PWT 8.0")

pwt8.0 <- pwt8.0 %>% mutate(variable = plyr::mapvalues(variable, from = c("rgdpe", "rgdpo", "cgdpe", "cgdpo", "rgdpna"), to = c("PWT 8.0: Expenditure side, chained PPPs", "PWT 8.0: Output side, chained PPPs", "PWT 8.0: Expenditure side, current PPPs, 2005$", "PWT 8.0: Output side, current PPPs, 2005$", "PWT 8.0: National-accounts growth rates, 2005$")))

economic.data <- rbind(pwt8.1 %>% select(-pop),pwt8.0 %>% select(-pop))

rm(pwt8.0, pwt8.1)

maddison <- maddison %>% ungroup() %>% select(country_name, GWn, year, value, in_system) %>% mutate(variable = "Maddison 2013: Real GDP per capita, 1990 Geary-Khamis", per_capita = value) %>% select(country_name, GWn, year, in_system, variable, value, per_capita) %>% mutate(primary_source = "Maddison", origin = "Maddison")

economic.data <- rbind(economic.data, maddison)
rm(maddison)

wdi <- wdi %>% ungroup() %>% select(country_name, GWn, year, SP.POP.TOTL, NY.GDP.MKTP.KD, NY.GDP.MKTP.PP.KD, in_system) %>% arrange(year) %>% rename(pop = SP.POP.TOTL)

wdi <- melt(wdi, measure.vars = c("NY.GDP.MKTP.KD", "NY.GDP.MKTP.PP.KD"))

wdi <- wdi %>% filter(!is.na(value)) %>% mutate(per_capita = value/(pop), variable = plyr::mapvalues(variable, from = c("NY.GDP.MKTP.KD", "NY.GDP.MKTP.PP.KD"), to = c("WDI: GDP per capita, constant 2005$", "WDI: GDP per capita, PPP, constant 2005$"))) %>% mutate(primary_source = "WDI", origin = "World Bank")

economic.data <- rbind(economic.data, wdi %>% select(-pop))
rm(wdi)

gleditsch <- expanded_gdp %>% select(country_name, GWn, year, pop, rgdppc, origin) %>% mutate(pop = pop / 1000) 
gleditsch <- melt(gleditsch, measure.vars = c("rgdppc"))
gleditsch <- gleditsch %>% mutate(per_capita = value, variable = plyr::mapvalues(variable, from = c("rgdppc", "cgdppc"), to = c("Real GDP, 2005 prices, output side", "GDP, current prices")), variable = "Gleditsch", in_system = TRUE, primary_source = "Gleditsch") %>% select(-pop) 
gleditsch <- gleditsch %>% group_by(GWn) %>% mutate(origin = paste(unique(origin), collapse = ", "), origin = paste0("Gleditsch, from ",origin))
gleditsch <- gleditsch %>% select(country_name, GWn, year, in_system, variable, value, per_capita, primary_source, origin)

economic.data <- rbind(economic.data, gleditsch)

rm(gleditsch)

economic.data <- economic.data %>% arrange(country_name, variable, year)
economic.data <- economic.data %>% filter(!is.na(country_name), per_capita > 0)

devtools::use_data(economic.data, overwrite = TRUE)