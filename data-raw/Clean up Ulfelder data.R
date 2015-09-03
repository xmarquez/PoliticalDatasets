library(dplyr)
library(PoliticalDatasets)


ulfelder <- read.csv("../Data/rgj2010.csv")



devtools::use_data(ulfelder, overwrite = TRUE)
