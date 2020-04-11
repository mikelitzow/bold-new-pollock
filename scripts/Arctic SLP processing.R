library(ncdf4)
library(zoo)
library(gplots)
library(dplyr)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(tidyr)
library(nlme)
library(ggplot2)

# get mean winter Arctic SLP >= 75N

# using monthly NCEP/NCAR!
nc.slp <- nc_open("data/NCEP.NCAR.slp.nc")

# now process SLP data - first, extract dates
raw <- ncvar_get(nc.slp, "time")  # seconds since 1-1-1970
h <- raw/(24*60*60)
slp.d <- dates(h, origin = c(1,1,1970))

slp.x <- ncvar_get(nc.slp, "longitude")
slp.y <- ncvar_get(nc.slp, "latitude")

SLP <- ncvar_get(nc.slp, "slp", verbose = F)
# Change data from a 3-D array to a matrix of monthly data by grid point:
# First, reverse order of dimensions ("transpose" array)
SLP <- aperm(SLP, 3:1)  

# reverse latitude to plot
slp.y <- rev(slp.y)
SLP <- SLP[,29:1,]

# Change to matrix with column for each grid point, rows for monthly means
SLP <- matrix(SLP, nrow=dim(SLP)[1], ncol=prod(dim(SLP)[2:3]))  

# Keep track of corresponding latitudes and longitudes of each column:
slp.lat <- rep(slp.y, length(slp.x))   
slp.lon <- rep(slp.x, each = length(slp.y))   
dimnames(SLP) <- list(as.character(slp.d), paste("N", slp.lat, "E", slp.lon, sep=""))

# plot to check
SLP.mean <- colMeans(SLP)
z <- t(matrix(SLP.mean,length(slp.y)))  # Re-shape to a matrix with latitudes in columns, longitudes in rows
image(slp.x,slp.y,z, col=tim.colors(64))
contour(slp.x, slp.y, z, add=T) 
map('world2Hires',fill=F,add=T, lwd=2)

# get month and year
slp.m <- months(slp.d)
slp.yr <- as.numeric(as.character(years(slp.d)))

# calculate area-weighted mean N 
# 75 - 90 N

arctic <- SLP

drop <- slp.lat < 75
arctic[,drop] <- NA

# plot to check
arctic.mean <- colMeans(arctic)
z <- t(matrix(arctic.mean,length(slp.y)))  # Re-shape to a matrix with latitudes in columns, longitudes in rows
image(slp.x,slp.y,z, col=tim.colors(64))
contour(slp.x, slp.y, z, add=T) 
map('world2Hires',fill=F,add=T, lwd=2)

# now weight by latitude
weight <- sqrt(cos(slp.lat*pi/180))
ff <- function(x) weighted.mean(x, w=weight, na.rm=T)

arctic <- apply(arctic, 1, ff)

arctic <- data.frame(year=slp.yr, month=as.numeric(slp.m), arctic=arctic)

arctic$winter.year <- ifelse(arctic$month %in% 11:12, arctic$year+1, arctic$year)

arctic <- arctic %>%
  group_by(winter.year) %>%
  summarise(arctic.slp.ndjfm=mean(arctic)) %>%
  filter(winter.year %in% 1949:2019)

names(arctic)[1] <- "year"

write.csv(arctic, "data/mean winter arctic slp.csv", row.names = F)
