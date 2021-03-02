library(tidyverse)
library(ncdf4)
library(maps)
library(maptools)
library(mapdata)
library(fields)
library(chron)

theme_set(theme_bw())

# daily winds for 57.5ºN 170ºW to calculate proportion of NE winds

URL <- "http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_a66c_3524_57cf.nc?uwnd[(1948-01-01):1:(2020-12-31T00:00:00Z)][(57.5):1:(57.5)][(190):1:(190)]"

download.file(URL, "data/NCEP.NCAR.daily.u-wind.nc")

# and load
dat <- nc_open("data/NCEP.NCAR.daily.u-wind.nc")

# paste directly into browser for windows!
dat <- nc_open("./data/hawaii_soest_a66c_3524_57cf_11c8_8435_d379.nc")

uwnd <- ncvar_get(dat, "uwnd", verbose = F)

# extract dates
raw <- ncvar_get(dat, "time") # seconds since 1-1-1970
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))
m <- months(d)
yr <- as.numeric(as.character(years(d)))

# add v-wind
URL <- "http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_3fcd_f037_d8fc.nc?vwnd[(1948-01-01):1:(2020-12-31T00:00:00Z)][(57.5):1:(57.5)][(190):1:(190)]"

download.file(URL, "data/NCEP.NCAR.daily.v-wind.nc")
dat <- nc_open("data/NCEP.NCAR.daily.v-wind.nc")

# or paste URL into browser for windows!
dat <- nc_open("./data/hawaii_soest_3fcd_f037_d8fc_bb1e_b937_8695.nc")

vwnd <- ncvar_get(dat, "vwnd", verbose = F)

# check that the dates are identical between the two data sets
raw <- ncvar_get(dat, "time") # seconds since 1-1-1970
h <- raw/(24*60*60)
d.v <- dates(h, origin = c(1,1,1970))

identical(d, d.v) # True!

# now make a dataframe with date info and uwnd

daily.wind <- data.frame(date=as.character(d),
                         month=m,
                         year=yr,
                         uwnd=uwnd,
                         vwnd=vwnd)

# calculate daily direction from u- and v- vectors
daily.wind$direction <-(180/pi * atan2(-daily.wind$uwnd, -daily.wind$vwnd))+180
range(daily.wind$direction) # perfecto

# now make columns of 1s and 0s to indicate if the wind is blowing NE
# Rosenkranz et al. 2001 use 60 degrees, we'll figure the # of days within 15 degrees of that (45-75)

daily.wind$NE <- 0

for(i in 1:nrow(daily.wind)){
  # i <- 2
  if(daily.wind$dir[i] >=45 & daily.wind$dir[i] <=75) daily.wind$NE[i] <- 1

}

# function to calculate the proportion of days with a particular wind direction
f <- function(x) sum(x)/sum(!is.na(x)) 

# get monthly sums of proportion of days with wind from NE
prop.NE <- tapply(daily.wind$NE, list(yr,m), f)


# Rosenkranz et al. 2001 use May-June
MayJunNE <- prop.NE[,5:6] 

# now get annual means
annNE <- rowMeans(MayJunNE)
plot(names(annNE), annNE, type="o")

# and get mean wind strength of NE winds
daily.wind$NE.velocity <- if_else(daily.wind$NE==1, sqrt(daily.wind$uwnd^2+daily.wind$vwnd^2), NA_real_)

mean.NE.wind <- daily.wind %>%
  filter(month %in% c("May", "Jun")) %>%
  group_by(year) %>%
  summarize(mean.NE.wind = mean(NE.velocity, na.rm = T))
  
ggplot(mean.NE.wind, aes(year, mean.NE.wind)) +
  geom_line() +
  geom_point()

## see if we can get 60 degree velocity
# first, wind speed
daily.wind$wind.speed <- sqrt(daily.wind$uwnd^2+daily.wind$vwnd^2)
daily.wind$tanner.vector <- daily.wind$wind.speed*cos((daily.wind$direction-60)*pi/180)

plot(daily.wind$direction, daily.wind$tanner.vector) #this looks right!

mean.tanner.wind <- daily.wind %>%
  filter(month %in% c("May", "Jun")) %>%
  group_by(year) %>%
  summarize(mean.60.wind.may.jun = mean(tanner.vector, na.rm = T))

ggplot(mean.tanner.wind, aes(year, mean.60.wind.may.jun)) +
  geom_line() +
  geom_point()

# save for crab analysis
write.csv(mean.tanner.wind, "./data/mean.tanner.wind.csv")
