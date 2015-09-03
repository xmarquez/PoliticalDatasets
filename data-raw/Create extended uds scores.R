library(QuickUDS)
library(PoliticalDatasets)

extended.data <- prepare.data(democracy)
extended.3 <- c('arat', 'blm', 'bollen', 'freedomhouse','hadenius', 'pacl', 'polity2', 'polyarchy', 'prc', 'vanhanen','munck', 'mainwaring', 'bmr_omitted', 'gwf', 'svolik')

extended.model.3 <- generate.scores(extended.data, extended.3)

# For testing purposes
comparison.extended_uds <-left_join(extended.model.3, uds, by=c("country_name","GWn","year"))

cor.extended.3 <- cor(comparison.extended_uds$z1, comparison.extended_uds$mean, use="pairwise.complete")

difference <- mean(comparison.extended_uds$z1 - comparison.extended_uds$mean, na.rm = TRUE)

correlations.extended.3 <- comparison.extended_uds %>% group_by(country_name) %>% do(data.frame(correlation = cor(.$z1,.$mean, use = "pairwise.complete"))) %>% ungroup() %>% arrange(correlation)

correlations.extended.3 %>% data.frame()

extended_uds <- extended.model.3 %>% dplyr::select(country_name, GWn, year, z1, pct025, pct975, in_system) %>% rename(mean = z1)

extended_uds$variable <- "Extended UDS"

uds$variable <- "Original UDS"

extended_uds <- rbind(extended_uds, uds %>% ungroup() %>% dplyr::select(country_name, GWn, year, mean, pct975, pct025, in_system, variable))

extended_uds <- left_join(extended_uds, correlations.extended.3)
extended_uds <- extended_uds %>% arrange(variable, country_name, year)
extended_uds <- extended_uds %>% mutate(country_corr = paste0(country_name," (",round(correlation,2),")"))
extended_uds <- extended_uds %>% group_by(country_name) %>% mutate(period = PoliticalDatasets::count_sequence_breaks(in_system,0))
extended_uds <- extended_uds %>% ungroup() %>% distinct()

devtools::use_data(extended_uds, overwrite =TRUE)
