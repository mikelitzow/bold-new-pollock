library(tidyverse)
library(ncdf4)
library(maps)
library(maptools)
library(mapdata)
library(fields)

# download various climate data sets!

################
# download NCEP/NCAR
################

# downloading global NCEP/NCAR slp

  URL <- 
    "https://upwell.pfeg.noaa.gov/erddap/griddap/noaa_esrl_118e_d5aa_117b.nc?slp[(1948-01-01):1:(2019-12-01)][(90.0):1:(20)][(0.0):1:(357.5)]"
  
  download.file(URL, "data/NCEP.NCAR.slp.nc")

# and test
test <- nc_open("data/NCEP.NCAR.slp.nc")
test

x <- ncvar_get(test, "longitude")
y <- ncvar_get(test, "latitude")
slp <- ncvar_get(test, "slp", verbose = F)
dim(slp) # 144 long, 29 lat, 864 months

# need to reverse latitude for plotting!
y <- rev(y)
slp <- slp[,29:1,]

# Change data into a matrix with months / cells for rows / columns
slp <- aperm(slp, 3:1)  
slp <- matrix(slp, nrow=dim(slp)[1], ncol=prod(dim(slp)[2:3]))  

z <- colMeans(slp, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", yaxt="n", xaxt="n")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)
# looks good!

####################
# add ERSST v5

download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1950-01-01):1:(2020-2-01)][(0.0):1:(0.0)][(54):1:(66)][(188):1:(202)]", "data/ersst")
# load and process SST data
nc <- nc_open("data/ersst")

# extract dates

ncvar_get(nc, "time")   # seconds since 1-1-1970
raw <- ncvar_get(nc, "time")
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))

# extract study area
# 54-66 deg. N, 188-202 deg. E
x <- ncvar_get(nc, "longitude")
y <- ncvar_get(nc, "latitude")

SST <- ncvar_get(nc, "sst", verbose = F)

# Change data from a 3-D array to a matrix of monthly data by grid point:
# First, reverse order of dimensions ("transpose" array)
SST <- aperm(SST, 3:1)  

# Change to matrix with column for each grid point, rows for monthly means
SST <- matrix(SST, nrow=dim(SST)[1], ncol=prod(dim(SST)[2:3]))  

# Keep track of corresponding latitudes and longitudes of each column:
lat <- rep(y, length(x))   
lon <- rep(x, each = length(y))   
dimnames(SST) <- list(as.character(d), paste("N", lat, "E", lon, sep=""))
# need to drop GOA cells
GOA <- c("N54E194", "N54E196", "N54E198", "N54E200", "N54E202", "N56E200", "N56E202")
SST[,GOA] <- NA

# plot to check
SST.mean <- colMeans(SST)
z <- t(matrix(SST.mean,length(y)))  # Re-shape to a matrix with latitudes in columns, longitudes in rows
image(x,y,z, col=tim.colors(64))
contour(x, y, z, add=T)  # Mean temperature pattern! Looks correct, that's good!
map('world2Hires',fill=F,xlim=c(130,250), ylim=c(20,66),add=T, lwd=2)


# now split into northern and southern EBS
south <- lat < 64
sst.south <- SST[,south]

north <- lat >= 64
sst.north <- SST[,north]

# get anomalies for 1981:2010
yr <- as.numeric(as.character(years(d)))

m <- months(d[yr %in% 1981:2010])

f <- function(x) tapply(x, m, mean)

# first for southern area
mu.south <- apply(sst.south[yr %in% 1981:2010,], 2, f)	# Compute monthly means for each time series (location)
mu.south <- mu.south[rep(1:12, floor(length(d)/12)),] 

# add trailing months (fractions of a full year at the end of time series)
xtra <- 12*((length(d)/12)-floor(length(d)/12))

mu.south <- rbind(mu.south, mu.south[1:xtra,])

anom.south <- rowMeans(sst.south - mu.south, na.rm=T)   # Compute matrix of anomalies!

# now for northern area
mu.north <- apply(sst.north[yr %in% 1981:2010,], 2, f)	# Compute monthly means for each time series (location)
mu.north <- mu.north[rep(1:12, floor(length(d)/12)),] 

mu.north <- rbind(mu.north, mu.north[1:xtra,])

anom.north <- rowMeans(sst.north - mu.north, na.rm=T)   # Compute matrix of anomalies!

m <- as.numeric(months(d))

dec.yr <- yr + (m-0.5)/12

par(las=1)
plot(dec.yr, anom.south, type="l", xlab="", ylab = "Anomaly wrt 1981-2010 (ºC)", col="grey")
abline(h=0)

par(las=1)
plot(dec.yr, anom.north, type="l", xlab="", ylab = "Anomaly wrt 1981-2010 (ºC)", col="grey")
abline(h=0)

annual.south <- tapply(anom.south, yr, mean)

# drop incomplete 2020
annual.south <- annual.south[names(annual.south) %in% 1950:2019]
plot(names(annual.south), annual.south, type="o", pch=19)

annual.north <- tapply(anom.north, yr, mean)
# drop incomplete 2020
annual.north <- annual.north[names(annual.north) %in% 1950:2019]
plot(names(annual.north), annual.north, type="o", pch=19)

# divide into seasons!
# I think for now we'll do two for each area - winter (NDJFM) and spring/summer (AMJ)

win <- c(11, 12, 1:3)
spr <- 4:6
win.yr <- ifelse(m %in% 11:12, yr+1, yr)
win.yr <- win.yr[m %in% win]
spr.yr <- yr[m %in% spr]

f.win <- function(x) tapply(x, win.yr, mean)

win.south <- sst.south[m %in% win,]
win.south <- rowMeans(apply(win.south, 2, f.win), na.rm=T)

win.north <- sst.north[m %in% win,]
win.north <- rowMeans(apply(win.north, 2, f.win), na.rm=T)

f.spr <- function(x) tapply(x, spr.yr, mean)

spr.south <- sst.south[m %in% spr,]
spr.south <- rowMeans(apply(spr.south, 2, f.spr), na.rm = T)

spr.north <- sst.north[m %in% spr,]
spr.north <- rowMeans(apply(spr.north, 2, f.spr), na.rm = T)

# now combine into a data frame
clim.dat <- data.frame(
  year=1951:2019,
  south.sst.ndjfm=win.south[names(win.south) %in% 1951:2019],
  south.sst.amj=spr.south[names(spr.south) %in% 1951:2019],
  north.sst.ndjfm=win.north[names(win.north) %in% 1951:2019],
  north.sst.amj=spr.north[names(spr.north) %in% 1951:2019]
)

# now plot to check
plot.dat <- clim.dat %>%
  pivot_longer(-year, names_to = "key", values_to = "value")

ggplot(plot.dat, aes(year, value)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~key, scales="free")
 # looks right!

##################
# add AO

ao <- read.csv("data/ao.csv")
head(ao)

# restrict to JFM and get annual means
ao <- ao %>%
  filter(month <= 3)

ao <- tapply(ao$value, ao$year, mean)

# limit to 1951-2019
ao <- ao[names(ao) %in% 1951:2019]

clim.dat$AO.jfm <- ao

########
# now NCEP/NCAR winds
# first U-wind (zonal / east-west winds)
URL <- 
  "http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_e77d_1b03_9908.nc?uwnd[(1948-01-01):1:(2020-01-01T00:00:00Z)][(45):1:(75)][(150):1:(225)]"

download.file(URL, "data/NCEP.NCAR.u-wind.nc")

# and test
test <- nc_open("data/NCEP.NCAR.u-wind.nc")
test

x <- ncvar_get(test, "longitude")
y <- ncvar_get(test, "latitude")
uwnd <- ncvar_get(test, "uwnd", verbose = F)

# Change data into a matrix with months / cells for rows / columns
uwnd <- aperm(uwnd, 3:1)  
uwnd <- matrix(uwnd, nrow=dim(uwnd)[1], ncol=prod(dim(uwnd)[2:3]))  

z <- colMeans(uwnd, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)
# looks good!

# first V-wind (meridional / north-south winds)
URL <- 
  "http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_aa14_316a_e154.nc?vwnd[(1948-01-01):1:(2020-01-01T00:00:00Z)][(45):1:(75)][(150):1:(225)]"

download.file(URL, "data/NCEP.NCAR.v-wind.nc")

# and test
test <- nc_open("data/NCEP.NCAR.v-wind.nc")
test

x <- ncvar_get(test, "longitude")
y <- ncvar_get(test, "latitude")
vwnd <- ncvar_get(test, "vwnd", verbose = F)

# Change data into a matrix with months / cells for rows / columns
vwnd <- aperm(vwnd, 3:1)  
vwnd <- matrix(vwnd, nrow=dim(vwnd)[1], ncol=prod(dim(vwnd)[2:3]))  

z <- colMeans(vwnd, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", yaxt="n", xaxt="n")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)
# looks good!

#####################
# now we need daily winds for 60ºN 170ºW to calculate proportion of NW/SE winds

URL <- "http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_a66c_3524_57cf.nc?uwnd[(1948-01-01):1:(2019-12-31T00:00:00Z)][(60):1:(60)][(190):1:(190)]"

download.file(URL, "data/NCEP.NCAR.daily.u-wind.nc")

# and load
dat <- nc_open("data/NCEP.NCAR.daily.u-wind.nc")

uwnd <- ncvar_get(dat, "uwnd", verbose = F)

# extract dates
raw <- ncvar_get(dat, "time") # seconds since 1-1-1970
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))
m <- months(d)
yr <- as.numeric(as.character(years(d)))

# add v-wind
URL <- "http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_3fcd_f037_d8fc.nc?vwnd[(1948-01-01):1:(2019-12-31T00:00:00Z)][(60):1:(60)][(190):1:(190)]"

download.file(URL, "data/NCEP.NCAR.daily.v-wind.nc")
dat <- nc_open("data/NCEP.NCAR.daily.v-wind.nc")
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

# now make columns of 1s and 0s to indicate if the wind is blowing NW or SE

daily.wind$NW <- daily.wind$SE <- 0

for(i in 1:nrow(daily.wind)){
  # i <- 2
  if(daily.wind$dir[i] >=105 & daily.wind$dir[i] <=165) daily.wind$SE[i] <- 1
  if(daily.wind$dir[i] >=285 & daily.wind$dir[i] <=345) daily.wind$NW[i] <- 1
}

# function to calculate the proportion of days with a particular wind direction
f <- function(x) sum(x)/sum(!is.na(x)) 

# get monthly sums of proportion of days with wind from each direction
prop.SE <- tapply(daily.wind$SE, list(yr,m), f)
prop.NW <- tapply(daily.wind$NW, list(yr,m), f)

# Danielson et al. 2012 GRL recommend seasons of Oct-Apr and May-Sept

MaySepNW <- prop.NW[,5:9] 
MaySepSE <- prop.SE[,5:9]

# now get summer means
sumNW <- rowMeans(MaySepNW)
plot(names(sumNW), sumNW, type="o")
sumSE <- rowMeans(MaySepSE)
plot(names(sumSE), sumSE, type="o")

# and winter...
OctAprNW <- prop.NW[2:nrow(prop.NW), 1:4]
#add in Oct Nov Dec from previous year
OctAprNW <- cbind(OctAprNW, prop.NW[1:(nrow(prop.NW)-1),10:12]) 
colMeans(OctAprNW, na.rm=T) # check how the months compare - pretty similar

# get means
winNW <- rowMeans(OctAprNW)
plot(names(winNW), winNW, type="o") # more of a coherent trend than the summer TS!

OctAprSE <- prop.SE[2:nrow(prop.SE), 1:4]
#add in Oct Nov Dec from previous year
OctAprSE <- cbind(OctAprSE, prop.SE[1:(nrow(prop.SE)-1),10:12]) 
colMeans(OctAprSE, na.rm=T) # check how the months compare - pretty similar

# get means
winSE <- rowMeans(OctAprSE)
plot(names(winSE), winSE, type="o") 

# very unusual proportions of winter proportions in recent years! should double-check these!

# add to climate data

clim.dat$SE.wind.Oct.Apr <- winSE[names(winSE) %in% clim.dat$year]
clim.dat$NW.wind.Oct.Apr <- winNW[names(winNW) %in% clim.dat$year]



