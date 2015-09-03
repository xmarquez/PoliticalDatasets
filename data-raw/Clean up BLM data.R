library(dplyr)
library(PoliticalDatasets)
library(readxl)
library(reshape2)

blm <- read_excel("../Data/blm final data.xls")

names(blm)[1] <- "year"

blm <- melt(blm, id.vars = "year")

names(blm)[2:3] <- c("country","blm")

blm <- to_gw_system(blm)

blm <- blm %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, -problem_history,-num_periods) %>% ungroup() %>% select(-country) %>% select(country_name, GWn, GWc, year, blm, region:in_system) 

devtools::use_data(blm, overwrite = TRUE)
