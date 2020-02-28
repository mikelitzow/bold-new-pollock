# combine and summarize the data 

library(tidyverse)
library(maps)
library(mapdata)

# get all the trawl file names!
path <- paste(getwd(), "/data", sep="") # I know there's a better way to do this!
files <- list.files(path=path)

keep <- grep("ebs", files)
files <- files[keep]

# load and combine!

trawl.data <- data.frame()

for(i in 1:length(files)){

  temp <- read.csv(file=paste(path, "/", files[i], sep=""))

  trawl.data <- rbind(trawl.data, temp)
  
  # ifelse(i==1,
  # trawl.data <- rbind(temp[2:nrow(temp),], temp)
  # trawl.data <- rbind(temp[2:nrow(temp),], temp))
  
}

str(trawl.data) # so these classes are wrong!

# reset the classes
numeric <- c(1,2,4,5,7,8,11:17)

for(i in 1:length(numeric)){
  
  trawl.data[,numeric[i]] <- as.numeric(trawl.data[,numeric[i]])
  
}

# this is creating NAs, need to check and make sure this is OK!

# now add NBS
temp <- read.csv("data/cpue_nbs_FULL_allspecies_2010_17_18.csv")

trawl.data <- rbind(trawl.data, temp)

lat <- tapply(trawl.data$LATITUDE, list(trawl.data$YEAR, trawl.data$STATION), mean, na.rm=T)
long <- tapply(trawl.data$LONGITUDE, list(trawl.data$YEAR, trawl.data$STATION), mean, na.rm=T)
j <- apply(lat, 2, function(x) sum(is.na(x)))
table(j)  

ff <- function(x) sum(!is.na(x))

sample.size <- apply(lat, 2, ff) # see that 'station' is a station name! I think are some colnames that got introduced into the data - need to check!

# remove stations with no samples!
keep <- sample.size > 0
sample.size <- sample.size[keep]

# get mean lat/long and plot
mean.lat <- apply(lat, 2, mean, na.rm=T)
mean.long <- apply(long, 2, mean, na.rm=T)

# remove the bogus 'station'
mean.lat <- mean.lat[keep]
mean.long <- mean.long[keep]

# and plot 

plot(mean.long+360, mean.lat, type="n")
text(mean.long+360, mean.lat, labels = sample.size)
map('world2Hires', c('usa', 'USSR'), 
    fill=T,xlim=c(130,250), ylim=c(20,70),add=T, lwd=0.5, col="darkgoldenrod3")

# now limit to pollock!
trawl.data <- trawl.data %>%
  filter(SID==21740)

# fit some spatial GAMs to cpue by era
library(mgcv)

# first redefine mean lat/long for each station
mean.lat <- tapply(trawl.data$LATITUDE, trawl.data$STATION, mean)
mean.long <- tapply(trawl.data$LONGITUDE, trawl.data$STATION, mean)

trawl.data$mean.lat <- mean.lat[match(trawl.data$STATION, names(mean.lat))]
trawl.data$mean.long <- mean.long[match(trawl.data$STATION, names(mean.long))]


fit.1 <- gam(log(WTCPUE) ~ s(mean.lat, mean.long, k=60), data=filter(trawl.data, YEAR <= 2013))
fit.2 <- gam(log(WTCPUE) ~ s(mean.lat, mean.long, k=60), data=filter(trawl.data, YEAR > 2013))

dir.create("figs", showWarnings = FALSE)

png("figs/cpue by decade.png", 8, 3, units="in", res=300)

par(mfrow=c(1,2), mar=c(2,2,1,1))
zlim <- range(fitted(fit.1), fitted(fit.2))

# 80s
vis.gam(fit.1, view = c("mean.long", "mean.lat"), n.grid=100, zlim=zlim,	plot.type="contour", color="topo", nlevels=8, labcex=0.8,
        too.far = 0.04, xlab = "", ylab = "", 	xlim=c(-180,-157), ylim=c(54,67), main="")  
map('worldHires',fill=T,add=T)
text(-160, 61, "1982-2013", col="white")

# 90s
vis.gam(fit.2, view = c("mean.long", "mean.lat"), n.grid=100, zlim=zlim,	plot.type="contour", color="topo", nlevels=8, labcex=0.8,
              too.far = 0.04, xlab = "", ylab = "", 	xlim=c(-180,-157), ylim=c(54,67), main="")  
map('worldHires',fill=T,add=T)
text(-160, 61, "2014-2019", col="white")

dev.off()

# NOT SURE ALL THE YEARS ARE IN NBS DATA!