library(lubridate)

colonial <- read.csv("../Data/ICOW Colonial History 1.0/coldata100.csv")

colonial$IndDate <- ymd(paste0(colonial$IndDate,"01"))

devtools::use_data(colonial,overwrite = FALSE)