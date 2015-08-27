library(dplyr)

fh <- read.csv("../Data/fh.19722014.csv")

fh$yrdied <- plyr::mapvalues(fh$yrdied, from = 9999, to = NA)
fh$fh_total <- fh$pr + fh$cl
fh$fh_total_reversed <- 14-fh$fh_total

fh <- fh %>% group_by(country) %>% mutate(fh_total_reversed_cum = cumsum(fh_total_reversed)) %>% rename(fh_country = country)

fh <- PoliticalDatasets::to_gw_system(fh, country_col = "fh_country")

fh <- fh %>% mutate(in_system = country_matches & date_matches) %>% select(-country_matches,-date_matches, -num_matches, - problem_history,-num_periods)

devtools::use_data(fh, overwrite = TRUE)
