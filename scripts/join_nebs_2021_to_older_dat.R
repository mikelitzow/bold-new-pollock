#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Join new NEBS and 2021 data to older sebs data

#Krista, May 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#load cleaned new data 
wd <- getwd()
newdat <- read.csv(paste(wd,"/data/survey data/cleaned_nebs_and_2021_bot_trawl_data.csv", sep=""))

#load other data also cleaned
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv", sep=""), row.names = 1)

#long format older data
sel.trawl.dat <- read.csv(paste(wd,"/data/select_trawl_dat.csv", sep=""), row.names = 1)

#cleaning  
sel.trawl.dat$YEAR_factor <- as.factor(sel.trawl.dat$YEAR)
sel.trawl.dat$BOT_TEMP[which(sel.trawl.dat$BOT_TEMP=="-9999")]<-NA
sel.trawl.dat$SURF_TEMP[which(sel.trawl.dat$SURF_TEMP=="-9999")]<-NA
sel.trawl.dat$WTCPUE[which(sel.trawl.dat$WTCPUE=="-9999")]<-NA
sel.trawl.dat$NUMCPUE[which(sel.trawl.dat$NUMCPUE=="-9999")]<-NA

sel.trawl.dat$LATITUDE  <- as.numeric(sel.trawl.dat$LATITUDE)
sel.trawl.dat$LONGITUDE <- as.numeric(sel.trawl.dat$LONGITUDE)
sel.trawl.dat$YEAR <- as.numeric(sel.trawl.dat$YEAR )
sel.trawl.dat$WTCPUE <- as.numeric(sel.trawl.dat$WTCPUE)
sel.trawl.dat$NUMCPUE <- as.numeric(sel.trawl.dat$NUMCPUE)
sel.trawl.dat$BOT_DEPTH <- as.numeric(sel.trawl.dat$BOT_DEPTH )
sel.trawl.dat$BOT_TEMP <- as.numeric(sel.trawl.dat$BOT_TEMP)
sel.trawl.dat$SURF_TEMP <- as.numeric(sel.trawl.dat$SURF_TEMP)


sel.trawl.dat$logCPUE <- log(sel.trawl.dat$WTCPUE + 1)

#add date, time, and julian to sel trawl dat
library(lubridate)
#split date and time
#2019 date time doesn't match!
sel.trawl.dat <- separate(sel.trawl.dat, DATETIME, into = c("Date", "Time"), sep = " ")
#this throws an error because many 2019 times are missing, but date is ok (if different format that other years)

sel.trawl.dat$julian <- yday(parse_date_time(sel.trawl.dat$Date, orders = "mdy"))
#the 1152 dates from 2019 that don't match don't work here

#let's see if I can parse these dates correctly
badies2019 <- sel.trawl.dat[is.na(sel.trawl.dat$Time)==TRUE,]
badies2019$julian <- yday(parse_date_time(badies2019$Date, orders = "dmy")) #yep works

#add Year_factor to new dat
newdat$Year_factor <- as.factor(newdat$YEAR)

#check station assignments in sel.trawl.dat
#remove 2018 from sel.trawl.dat for join
#remove nebs from sel.trawl.dat for join
#join
#remove duplicates



#DO THIS AFTER JOIN
#convert latitude & longitude to x/y coordinates that reflect 
# actual distances using the equal-distance Albers projection:
# (I chose the parameters based on a 'rule of thumb' in the help file)
x <- mapproject(sel.trawl.dat$LONGITUDE, sel.trawl.dat$LATITUDE, "albers", param=c(55.9, 60.8))
sel.trawl.dat$long_albers <- x$x
sel.trawl.dat$lat_albers <- x$y


                   
                   





















