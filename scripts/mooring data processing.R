library(ncdf4)
library(tidyverse)
library(chron)

# load, process, combine M2 files!!

# get a list of files

files <- list.files("data/M2 mooring data")

# drop the readme file!

drop <- grep("Readme", files)
files <- files[-drop]

# # now dropping the file(s) that is(are) causing trouble
# drop <- c(grep("95bsm2_sc_0010m.cf.nc", files),
#           grep("96bsm2a_sc_0001m.cf.nc", files), 
#           grep("96bsm2a_sc_0006m.cf.nc", files))
# 
# files <- files[-drop]

# now add in the replacement files that Shaun shared on 3/12/20
replacement <- list.files("data/replacement M2")

# check for repeats!
both <- intersect(files, replacement)

# compare!
compare <- NA

for(i in 1:length(both)){
  i <- 1
  path <- paste("data/M2 mooring data/", both[i], sep="")
  file1 <- nc_open(path)

  path <- paste("data/replacement M2/", both[i], sep="")
  file2 <- nc_open(path)
  
  compare[i] <- identical(file1, file2)
  
}

# not the same!
# so for now (3/12/20) I'll use the versions he sent me today

drop <- files %in% both

files <- files[!drop]

# loop through each and see if we can combine! 
# :)

# object to combine all files!
all.files <- data.frame()

# start with files in "both"
for(i in 1:length(both)){
  
  # i <- 1
  
  path <- paste("data/replacement M2/", both[i], sep="")
  
  # load file
  nc <- nc_open(path)
  
  # get variable names
  vars <- names(nc$var)
  
  # extract dates
  raw <- ncvar_get(nc, "time") # dates since 1-1-1900
  date <- dates(raw, origin = c(1,1,1900))
  
  # extract depth
  depth <- ncvar_get(nc, "depth")
  
  # just to be sure/check, extract lat/long
  lat <- ncvar_get(nc, "lat")
  long <- ncvar_get(nc, "lon")
  
  vars <- names(nc$var)
  
  # attributes(nc)$names
  
  # dates_df %>% 
  #   separate(date, c("month", "day", "year"), "/")
  
  # now loop through each variable
  
  # set up an object to capture summarized data from this file
  this.file <- data.frame()
  
  for(j in 1:length(vars)){ 
    
    # j <- 1
    # choose <- paste()
    # var <- ncvar_get(nc, nc$var[j], verbose = F)
    
    var <- ncvar_get(nc, vars[j], verbose = F)
    
    meta.data <- ncatt_get(nc, attributes(nc$var)$names[j])
    
    # so we'll put together a data frame with daily means and sample size 
    
    temp.dat <- data.frame(dat=var,
                           date=date) %>%
      group_by(date) %>%
      summarise(daily.mean=mean(dat), n=n()) %>%
      separate(date, c("month", "day", "year"), "/") 
    
    # add identifying meta-data
    
    temp.dat$name <- meta.data$name
    temp.dat$long.name <- meta.data$long_name
    temp.dat$generic.name <- meta.data$generic_name
    temp.dat$units <- meta.data$units
    temp.dat$depth <- depth
    temp.dat$lat <- lat
    temp.dat$long <- long
    
    this.file <- rbind(this.file, temp.dat)
    
  }
  
  all.files <- rbind(all.files, this.file)
  
}

###################################
# now the one in the original list!
for(i in 1:length(files)){
  
  # i <- 1
  
  path <- paste("data/M2 mooring data/", files[i], sep="")
  
  # load file
  nc <- nc_open(path)
  
  # get variable names
  vars <- names(nc$var)
  
  # extract dates
  raw <- ncvar_get(nc, "time") # dates since 1-1-1900
  date <- dates(raw, origin = c(1,1,1900))
  
  # extract depth
  depth <- ncvar_get(nc, "depth")
  
  # just to be sure/check, extract lat/long
  lat <- ncvar_get(nc, "lat")
  long <- ncvar_get(nc, "lon")

  vars <- names(nc$var)
  
  # attributes(nc)$names
  
  # dates_df %>% 
  #   separate(date, c("month", "day", "year"), "/")
  
  # now loop through each variable
  
  # set up an object to capture summarized data from this file
  this.file <- data.frame()
  
  for(j in 1:length(vars)){ 
    
    # j <- 1
    # choose <- paste()
    # var <- ncvar_get(nc, nc$var[j], verbose = F)
    
    var <- ncvar_get(nc, vars[j], verbose = F)
    
    meta.data <- ncatt_get(nc, attributes(nc$var)$names[j])
    
    # so we'll put together a data frame with daily means and sample size 
    
    temp.dat <- data.frame(dat=var,
                           date=date) %>%
      group_by(date) %>%
      summarise(daily.mean=mean(dat), n=n()) %>%
      separate(date, c("month", "day", "year"), "/") 
    
    # add identifying meta-data
    
    temp.dat$name <- meta.data$name
    temp.dat$long.name <- meta.data$long_name
    temp.dat$generic.name <- meta.data$generic_name
    temp.dat$units <- meta.data$units
    temp.dat$depth <- depth
    temp.dat$lat <- lat
    temp.dat$long <- long

    this.file <- rbind(this.file, temp.dat)
  
}
  
  all.files <- rbind(all.files, this.file)
  
}

# now...get monthly means for each variable!
head(all.files)

unique(all.files$year)


# there are some NAs - let's examine!

View(filter(all.files, year=="NA"))
# appears that those are four instances where there are only flagged missing values for a variable!!

# let's change m/d/y to numbers and get a better format for year

all.files$month <- as.numeric(all.files$month)
View(filter(all.files, is.na(month)==T)) # same thing - only flagged missing values!


all.files$day <- as.numeric(all.files$day)
View(filter(all.files, is.na(day)==T))

all.files$year <- as.numeric(all.files$year)

all.files$year <- as.numeric(
  ifelse(is.na(all.files$year)==T, all.files$year,
         ifelse(all.files$year>20,
                all.files$year+1900,
                all.files$year+2000))
)

unique(all.files$year)

unique(all.files$lat)
unique(all.files$long)

# looks good


# ok! summarize!

head(all.files)

month.mean <- all.files %>%
  group_by(year, month, long.name, depth) %>%
  summarise(mean=mean(daily.mean), n=n())

unique(month.mean$long.name) # that's a mess!
unique(month.mean$depth)

# ok!
# add decimal year for plotting
month.mean$decimal.year <- month.mean$year + (month.mean$month-0.5)/12

# try to put together temp at various depths!
keep <- grep("TEMP", month.mean$long.name)

temp <- month.mean[keep,]

ggplot(temp, aes(decimal.year, n)) +
  geom_line() +
  facet_wrap(~depth) 

# lots of missing depths!

temp <- temp %>%
  filter(depth != -9999)

ggplot(temp, aes(decimal.year, n)) +
  geom_line() +
  facet_wrap(~depth)

# limit to bottom water (>= 60m)
deep.temp <- temp %>%
  filter(depth >=60, mean < 50) %>% # removing the NA flags!
  group_by(year, month) %>%
  summarise(mean=mean(mean), n=sum(n)) 

# now limit to months with at least 15 daily measurements!
deep.temp <- deep.temp %>%
  filter(n >=15) %>%
  pivot_longer(-c(year, month), names_to = "key", values_to = "values")

deep.temp$decimal.year <- deep.temp$year + (deep.temp$month-0.5)/12 
  
ggplot(deep.temp, aes(decimal.year, values)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~key, scales="free_y") 

# now remove n as we know they're all ok

deep.temp <- deep.temp %>%
  filter(key=="mean")

# now get monthly climatology
climatology <- deep.temp %>%
  group_by(month) %>%
  summarise(clim.mean=mean(values))

ggplot(climatology, aes(month, clim.mean)) +
  geom_line() +
  geom_point()

# now plug back into the df to calculate monthly anomalies
deep.temp <- left_join(deep.temp, climatology)

deep.temp$anomaly <- deep.temp$values-deep.temp$clim.mean

# now get JFMAM means for each year
JFMAM.temp <- deep.temp %>%
  filter(month <=5) %>%
  group_by(year) %>%
  summarise(mean=mean(anomaly))

ggplot(JFMAM.temp, aes(year, mean)) +
  geom_line()

# now add to the climate data file

all.dat <- read.csv("data/climate data.csv")
head(all.dat)

names(JFMAM.temp)[2] <- "M2.JFMAM.60.72m.temp"

all.dat <- left_join(all.dat, JFMAM.temp)

##################
# now check out salinity
unique(month.mean$long.name) 
unique(month.mean$depth)

# ok!
# add decimal year for plotting
month.mean$decimal.year <- month.mean$year + (month.mean$month-0.5)/12

# try to put together salinity and conductivity
keep <- c(grep("CONDUCT", month.mean$long.name),
          grep("SALIN", month.mean$long.name),
          grep("Salin", month.mean$long.name))
          

temp <- month.mean[keep,]

ggplot(temp, aes(decimal.year, n)) +
  geom_line() +
  facet_wrap(~depth) 

# lots of missing depths!

temp <- temp %>%
  filter(depth != -9999)

ggplot(temp, aes(decimal.year, n)) +
  geom_line() +
  facet_wrap(~depth)

# limit to 10-15m
deep.temp <- temp %>%
  filter(depth %in% 10:15, mean < 50, mean >20) %>% # removing the NA flags!
  group_by(year, month) %>%
  summarise(mean=mean(mean), n=sum(n)) 


ggplot(deep.temp, aes(mean)) +
  geom_histogram() 
# looks good!

# now limit to months with at least 15 daily measurements!
deep.temp <- deep.temp %>%
  filter(n >=15) %>%
  pivot_longer(-c(year, month), names_to = "key", values_to = "values")

deep.temp$decimal.year <- deep.temp$year + (deep.temp$month-0.5)/12 

ggplot(deep.temp, aes(decimal.year, values)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~key, scales="free_y") 

# now remove n as we know they're all ok

deep.temp <- deep.temp %>%
  filter(key=="mean")

# now get monthly climatology
climatology <- deep.temp %>%
  group_by(month) %>%
  summarise(clim.mean=mean(values))

ggplot(climatology, aes(month, clim.mean)) +
  geom_line() +
  geom_point()

# now plug back into the df to calculate monthly anomalies
deep.temp <- left_join(deep.temp, climatology)

deep.temp$anomaly <- deep.temp$values-deep.temp$clim.mean

# now get JFMAM means for each year
JFMAM.temp <- deep.temp %>%
  filter(month <=5) %>%
  group_by(year) %>%
  summarise(mean=mean(anomaly))

ggplot(JFMAM.temp, aes(year, mean)) +
  geom_line()

# now add to the climate data file

names(JFMAM.temp)[2] <- "M2.JFMAM.10.15m.salinity"

all.dat <- left_join(all.dat, JFMAM.temp)

# next step - add summer 10-15m salinity, and also salinity at depth!
# use JAS for summer, as these are the peak salinity values

ggplot(temp, aes(decimal.year, n)) +
  geom_line() +
  facet_wrap(~depth)

# limit to 10-15m
deep.temp <- temp %>%
  filter(depth %in% 10:15, mean < 50, mean >20) %>% # removing the NA flags!
  group_by(year, month) %>%
  summarise(mean=mean(mean), n=sum(n)) 

ggplot(deep.temp, aes(mean)) +
  geom_histogram() 

# looks a litte funky!

# now limit to months with at least 15 daily measurements!
deep.temp <- deep.temp %>%
  filter(n >=15) %>%
  pivot_longer(-c(year, month), names_to = "key", values_to = "values")

deep.temp$decimal.year <- deep.temp$year + (deep.temp$month-0.5)/12 

ggplot(deep.temp, aes(decimal.year, values)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~key, scales="free_y") 

# now remove n as we know they're all ok

deep.temp <- deep.temp %>%
  filter(key=="mean")

# now get monthly climatology
climatology <- deep.temp %>%
  group_by(month) %>%
  summarise(clim.mean=mean(values))

ggplot(climatology, aes(month, clim.mean)) +
  geom_line() +
  geom_point()

# now plug back into the df to calculate monthly anomalies
deep.temp <- left_join(deep.temp, climatology)

deep.temp$anomaly <- deep.temp$values-deep.temp$clim.mean

# now get JAS means for each year
JAS.temp <- deep.temp %>%
  filter(month %in% 7:9) %>%
  group_by(year) %>%
  summarise(mean=mean(anomaly))

ggplot(JAS.temp, aes(year, mean)) +
  geom_line()

# now add to the climate data file

names(JAS.temp)[2] <- "M2.JAS.10.15m.salinity"

all.dat <- left_join(all.dat, JAS.temp)
###############################
# and, salinity at depth!
# will use 40-55 m....

deep.temp <- temp %>%
  filter(depth %in% 40:55, mean < 50, mean >20) %>% # removing the NA flags!
  group_by(year, month) %>%
  summarise(mean=mean(mean), n=sum(n)) 


ggplot(deep.temp, aes(mean)) +
  geom_histogram() 

# looks good!

# now limit to months with at least 15 daily measurements!
deep.temp <- deep.temp %>%
  filter(n >=15) %>%
  pivot_longer(-c(year, month), names_to = "key", values_to = "values")

deep.temp$decimal.year <- deep.temp$year + (deep.temp$month-0.5)/12 

ggplot(deep.temp, aes(decimal.year, values)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~key, scales="free_y") 

# surprisingly low values in the 90s!
# save the fig to ask Shaun for the guidance...

ggsave("figs/M2 salinity 40-55m by month - mean and n.png", width=8, height=4, units='in')


# now remove n 

deep.temp <- deep.temp %>%
  filter(key=="mean")

# now get monthly climatology
climatology <- deep.temp %>%
  group_by(month) %>%
  summarise(clim.mean=mean(values))

ggplot(climatology, aes(month, clim.mean)) +
  geom_line() +
  geom_point()

# looks like JFMAM is the minimum / OND the max!
# will include those two periods and also JAS as that is the summer high-productivity period!

# now plug back into the df to calculate monthly anomalies
deep.temp <- left_join(deep.temp, climatology)

deep.temp$anomaly <- deep.temp$values-deep.temp$clim.mean

# now get JFMAM means for each year
JFMAM.temp <- deep.temp %>%
  filter(month <=5) %>%
  group_by(year) %>%
  summarise(mean=mean(anomaly))

ggplot(JFMAM.temp, aes(year, mean)) +
  geom_line()

# now add to the climate data file

names(JFMAM.temp)[2] <- "M2.JFMAM.40.55m.salinity"

all.dat <- left_join(all.dat, JFMAM.temp)

# next steps - add JAS and OND 40-55m salinity

JAS.temp <- deep.temp %>%
  filter(month %in% 7:9) %>%
  group_by(year) %>%
  summarise(mean=mean(anomaly))

ggplot(JAS.temp, aes(year, mean)) +
  geom_line()

# now add to the climate data file

names(JAS.temp)[2] <- "M2.JAS.40.55m.salinity"

all.dat <- left_join(all.dat, JAS.temp)

# and! OND!!
OND.temp <- deep.temp %>%
  filter(month >= 10) %>%
  group_by(year) %>%
  summarise(mean=mean(anomaly))

ggplot(OND.temp, aes(year, mean)) +
  geom_line()

# now add to the climate data file

names(OND.temp)[2] <- "M2.OND.40.55m.salinity"

all.dat <- left_join(all.dat, OND.temp)

###############################

write.csv(all.dat, "data/climate data.csv", row.names = F)

plot.dat <- all.dat %>%
  pivot_longer(-year, names_to = "key", values_to = "value")

ggplot(plot.dat, aes(year, value)) +
  geom_line() +
  facet_wrap(~key, scales="free_y")

ggsave("figs/climate time series collected to date.png", width=10, height=8, units='in')

