# adding this from another project to preserve the history of how I 
# estimated recent values for the PDO
# haven't re-run the code at this point


# being a script to estimate 2019 values of the PDO
# via PCA on ERSSTa values, until such a time as the official index values have been posted

library(ncdf)
library(maps)
library(mapdata)
library(chron)
library(fields)
setwd("/Users/MikeLitzow 1/Documents/R/AK salmon fisheries and SST")

nc <- nc_open("/Users/MikeLitzow 1/Documents/R/climate-data/data/North.Pacific.ersst")

ncvar_get(nc, "time")   # seconds since 1-1-1970
raw <- ncvar_get(nc, "time")
h <- raw/(24*60*60)
sst.d <- dates(h, origin = c(1,1,1970))

sst.x <- ncvar_get(nc, "longitude")
sst.y <- ncvar_get(nc, "latitude")

SST <- ncvar_get(nc,  "sst")
# Change data from a 3-D array to a matrix of monthly data by grid point:
# First, reverse order of dimensions ("transpose" array)
SST <- aperm(SST, 3:1)  

# Change to matrix with column for each grid point, rows for monthly means
SST <- matrix(SST, nrow=dim(SST)[1], ncol=prod(dim(SST)[2:3]))  

# Keep track of corresponding latitudes and longitudes of each column:
sst.lat <- rep(sst.y, length(sst.x))   
sst.lon <- rep(sst.x, each = length(sst.y))   
dimnames(SST) <- list(as.character(sst.d), paste("N", sst.lat, "E", sst.lon, sep=""))

# Limit to 20-66 deg. N, 132-250 deg. E:
drop <- sst.lat > 66
SST[,drop] <- NA

drop <- sst.lon < 132
SST[,drop] <- NA

# limit to Jan 1900 - Dec 1993 and later
sst.d[c(553,1680)]

SST <- SST[553:1680,]

# plot to check
SST.mean <- colMeans(SST)
z <- t(matrix(SST.mean,length(sst.y)))  # Re-shape to a matrix with latitudes in columns, longitudes in rows
image(sst.x,sst.y,z, col=tim.colors(64))
contour(sst.x, sst.y, z, add=T) 
map('world2Hires',fill=F,xlim=c(130,250), ylim=c(20,66),add=T, lwd=2)

# drop NAs and compute anomalies
land <- is.na(SST.mean) 

X <- SST[,!land] 

m <- months(rownames(SST))  # Extracts months from the date vector
m  # Result is a factor
f <- function(x) tapply(x, m, mean)  # function to compute monthly means for a single time series
mu <- apply(X, 2, f)	# Compute monthly means for each time series (location)
mu
mu <- mu[rep(1:12, nrow(SST)/12),]  # Replicate means matrix for each year at each location (112 times)
# (i.e. "stack" monthly means on top of each other 112 times!)
X.anom <- X - mu 

SST.pca <- svd(cov(X.anom))

# plot pc1 to check

pc1 <- X.anom %*% SST.pca$u[,1]
pc1 <- as.vector(scale(pc1))  # Scale to unit variance (to compare to "true PDO")

pc1 <- -pc1  # Reverse sign of PC 1 to match "true PDO" (see below)

xx <- seq(1900+0.5/12, 1993+11.5/12, length=94*12)  # for plotting (decimal year)
plot(xx, pc1, type="l")
abline(v=1977, lty=2)

# check for warming in mean anomalies at this scale - none evident
plot(1:nrow(X), rowMeans(X.anom), type="l")

# now get the 1900-2019 values to calculate the full time series of the estimated PDO!

# reload and re-process
nc <- nc_open("/Users/MikeLitzow 1/Documents/R/climate-data/data/North.Pacific.ersst")

ncvar_get(nc, "time")   # seconds since 1-1-1970
raw <- ncvar_get(nc, "time")
h <- raw/(24*60*60)
sst.d <- dates(h, origin = c(1,1,1970))

sst.x <- ncvar_get(nc, "longitude")
sst.y <- ncvar_get(nc, "latitude")

SST <- ncvar_get(nc,  "sst")
# Change data from a 3-D array to a matrix of monthly data by grid point:
# First, reverse order of dimensions ("transpose" array)
SST <- aperm(SST, 3:1)  

# Change to matrix with column for each grid point, rows for monthly means
SST <- matrix(SST, nrow=dim(SST)[1], ncol=prod(dim(SST)[2:3]))  

# Keep track of corresponding latitudes and longitudes of each column:
sst.lat <- rep(sst.y, length(sst.x))   
sst.lon <- rep(sst.x, each = length(sst.y))   
dimnames(SST) <- list(as.character(sst.d), paste("N", sst.lat, "E", sst.lon, sep=""))

# Limit to 20-66 deg. N, 132-250 deg. E:
drop <- sst.lat > 66
SST[,drop] <- NA

drop <- sst.lon < 132
SST[,drop] <- NA

# limit to Jan 1900 - latest
sst.d[c(553,length(sst.d))]

SST <- SST[553:nrow(SST),]

# plot to check
SST.mean <- colMeans(SST)
z <- t(matrix(SST.mean,length(sst.y)))  # Re-shape to a matrix with latitudes in columns, longitudes in rows
image(sst.x,sst.y,z, col=tim.colors(64))
contour(sst.x, sst.y, z, add=T) 
map('world2Hires',fill=F,xlim=c(130,250), ylim=c(20,66),add=T, lwd=2)

# drop NAs and compute anomalies
land <- is.na(SST.mean) 

X <- SST[,!land] 

m <- months(rownames(SST))  # Extracts months from the date vector
m  # Result is a factor
f <- function(x) tapply(x, m, mean)  # function to compute monthly means for a single time series
mu <- apply(X, 2, f)	# Compute monthly means for each time series (location)
mu
mu <- mu[rep(1:12, nrow(SST)/12),]  

xtra <- 12*((nrow(SST)/12)-floor(nrow(SST)/12))

mu <- rbind(mu, mu[1:xtra,])

X.anom <- X - mu 

# now detrend each cell

X.anom.detrended <- X.anom
xx <- 1:nrow(X.anom)

for(j in 1:ncol(X.anom)){
  
  mod <- lm(X.anom[,j] ~ xx)
  
  X.anom.detrended[,j] <- residuals(mod)
  
}

# now project!

new.pc1 <- X.anom.detrended %*% SST.pca$u[,1]
new.pc1 <- as.vector(scale(new.pc1))
plot(1:length(new.pc1), new.pc1, type="l")

year <- as.numeric(as.character(years(rownames(SST))))
year[1:360] <- year[1:360]-100

new.pdo <- data.frame(year=year,
                      month=months(rownames(SST)), 
                      est.PDO = -new.pc1)

# load real PDO
names <- read.table("/Users/MikeLitzow 1/Documents/R/FATE2/non-som/~pdo", skip=30, nrows=1, as.is = T)
pdo <- read.table("/Users/MikeLitzow 1/Documents/R/FATE2/non-som/~pdo", skip=31, nrows=119, fill=T, col.names = names)
pdo$YEAR <- 1900:(1899+nrow(pdo)) # drop asterisks!
pdo <- pdo %>%
  gather(month, value, -YEAR) %>%
  arrange(YEAR)

new.pdo$real.pdo <- c(pdo$value, rep(NA,7))

plot(new.pdo$est.PDO[new.pdo$year > 1993], new.pdo$real.pdo[new.pdo$year > 1993])
# that'll do!
mod <- lm(real.pdo ~ est.PDO, data=new.pdo[new.pdo$year > 1993,], na.action="na.exclude")

new.pdo$real.pdo[1426:1435] <- coef(mod)[1] + new.pdo$est.PDO[1426:1435]*coef(mod)[2]

write.csv(new.pdo, "estimated PDO through May 2019.csv")
