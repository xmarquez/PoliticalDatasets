library(lubridate)

states <- read.delim("http://privatewww.essex.ac.uk/~ksg/data/iisystem.dat",header=FALSE)
microstates <- read.delim("http://privatewww.essex.ac.uk/~ksg/data/microstatessystem.dat",header=FALSE)

states$microstate <- FALSE
microstates$microstate <- TRUE

gw_system <- rbind(states,microstates)

names(gw_system) <- c("code","cabb","country","startdate","enddate","microstate")
gw_system$startdate <- dmy(gw_system$startdate)
gw_system$enddate <- dmy(gw_system$enddate)

gw_system$enddate <- ifelse(as.character(gw_system$enddate) == "2012-12-31",as.character(today()), as.character(gw_system$enddate))
gw_system$enddate <- ymd(gw_system$enddate) 

devtools::use_data(gw_system, overwrite=TRUE)
