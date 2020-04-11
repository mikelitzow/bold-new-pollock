library(tidyverse)

# combine AO, PDO, NPGO (winter values) in one data frame
dat1 <- read.csv("data/climate data.csv")

head(dat1)

dat2 <- read.csv("data/winter pdo-npgo.csv")

dat1 <- dat1 %>%
  select(year, AO.jfm)

dat <- left_join(dat1, dat2)

# 11-yr rolling correlations

correlations <- as.data.frame(matrix(nrow=69-25, ncol=3))
colnames(correlations) <- c("PDO-NPGO", "PDO-AO", "NPGO-AO")


for(i in 1951:1995){
  # i <- 1951
  
  temp <- dat[dat$year %in% i:(i+24),]
  
  correlations[(i-1950),1] <- cor(temp[,2], temp[,3])
  correlations[(i-1950),2] <- cor(temp[,1], temp[,2])
  correlations[(i-1950),3] <- cor(temp[,1], temp[,3])
}

correlations$year <- 1963:2007
correlations <- correlations %>%
  pivot_longer(cols=-year)

# load colorblind palette 
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(correlations, aes(year, value)) +
  facet_wrap(~name, nrow=3) + geom_hline(yintercept = 0) + theme_bw() +
  geom_line(color=cb[2]) + 
  xlab("") + ylab("Correlation") + theme(axis.title.x = element_blank())

ggsave("figs/rolling 25-yr correlations pdo npgo ao.png", width=6, height=6, units='in')
# PDO-NPGO doesn't look right! need to check

# another tack here - let's look at correlations between monthly AL & AO

# load monthly ao 
ao <- read.csv("data/ao.csv")
names(ao)[1] <- "year"

# now load SLP and calculate NPI!
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

# calculate NPI - 
# 30º-65ºN, 160ºE-140ºW 

NPI <- SLP

drop <- slp.lat < 30
NPI[,drop] <- NA

drop <- slp.lat > 65
NPI[,drop] <- NA

drop <- slp.lon < 160
NPI[,drop] <- NA

drop <- slp.lon > 220
NPI[,drop] <- NA

# plot to check
NPI.mean <- colMeans(NPI)
z <- t(matrix(NPI.mean,length(slp.y)))  # Re-shape to a matrix with latitudes in columns, longitudes in rows
image(slp.x,slp.y,z, col=tim.colors(64))
contour(slp.x, slp.y, z, add=T) 
map('world2Hires',fill=F,add=T, lwd=2)

# now weight by latitude
weight <- sqrt(cos(slp.lat*pi/180))
ff <- function(x) weighted.mean(x, w=weight, na.rm=T)

NPI <- apply(NPI, 1, ff)

NPI <- data.frame(year=slp.yr, month=as.numeric(slp.m), NPI=NPI)

dat <- left_join(NPI, ao)
names(dat)[4] <- "AO"

# get 11-month rolling means
dat$NPI.11 <- rollapply(dat$NPI, 11, mean, na.rm=T, fill=NA)
dat$AO.11 <- rollapply(dat$AO, 11, mean, na.rm=T, fill=NA)

plot <- dat %>%
  pivot_longer(cols=c(-year, -month)) %>%
  mutate(decimal.yr=year + (month-0.5)/12)

ggplot(plot, aes(decimal.yr, value)) +
  theme_bw() + 
  geom_line() +
  facet_wrap(~name, scales="free_y")

dat$cor.11 <- dat$cor <- NA

for(i in 1:(nrow(dat)-132)){
  # i <- 1
  
  dat$cor[(i+67)] <- cor(dat$NPI[i:(i+132)], dat$AO[i:(i+132)])
  dat$cor.11[(i+67)] <- cor(dat$NPI.11[i:(i+132)], dat$AO.11[i:(i+132)])  

}


cor.plot <- dat %>%
  select(year, month, cor, cor.11) %>%
  pivot_longer(cols=c(-year, -month)) %>%
  mutate(decimal.yr=year + (month-0.5)/12)

ggplot(cor.plot, aes(decimal.yr, value)) +
  theme_bw() + 
  geom_line() +
  facet_wrap(~name)

# look at ccf by era for smoothed observations
dat <- dat %>%
  select(-cor, -cor.11)

dat <- na.omit(dat)
  
dat$era <- ifelse(dat$year <= 1988, "1951-1988",
                         ifelse(dat$year %in% 1989:2013, "1989-2013", "2014-2019"))

era1 <- dat %>%
  filter(era=="1951-1988")

x1 <- ccf(era1$NPI.11, era1$AO.11)

era2 <- dat %>%
  filter(era=="1989-2013")

x2 <- ccf(era2$NPI.11, era2$AO.11)

era3 <- dat %>%
  filter(era=="2014-2019")

x3 <- ccf(era3$NPI.11, era3$AO.11)

era.cors <- data.frame(lag=-23:23,
                       era1=x1$acf,
                       era2=c(NA, NA, x2$acf, NA, NA),
                       era3=c(rep(NA,8), x3$acf, rep(NA,8)))

era.cors <- era.cors %>%
  pivot_longer(cols=-lag)

ggplot(era.cors, aes(lag, value, fill=name)) +
  theme_bw() + 
  geom_bar(position="dodge", stat = "identity") +
  xlim(-18,18) +
  ggtitle("Data smoothed with 11-month rolling mean")

# now raw / unsmoothed underlying data
x1 <- ccf(era1$NPI, era1$AO)

x2 <- ccf(era2$NPI, era2$AO)

x3 <- ccf(era3$NPI, era3$AO)

era.cors <- data.frame(lag=-23:23,
                       era1=x1$acf,
                       era2=c(NA, NA, x2$acf, NA, NA),
                       era3=c(rep(NA,8), x3$acf, rep(NA,8)))

era.cors <- era.cors %>%
  pivot_longer(cols=-lag)

ggplot(era.cors, aes(lag, value, fill=name)) +
  theme_bw() + 
  geom_bar(position="dodge", stat = "identity") +
  xlim(-18,18) +
  ggtitle("Raw data")

ggsave("figs/AO NPI cross-correlation by era.png", width=6.5, height=3, units='in')

# so...these are a little confusing (the difference between the raw/unsmoothed)
# will look at winter as I think that is more relevant!

# start by cleaning up...
dat <- dat %>%
  select(year, month, NPI, AO)

dat$winter.year <- ifelse(dat$month %in% 11:12, dat$year+1, dat$year)

winter.dat <- dat %>%
  filter(month %in% c(11,12,1:3)) %>%
  pivot_longer(cols=c(NPI,AO)) %>%
  group_by(name, winter.year) %>%
  summarise(winter.mean=mean(value)) # something is wrong here, need to figure out later!

# summarise winter NPI and save
sum.dat <- dat %>%
  filter(month %in% c(11,12,1:3)) %>%
  select(winter.year, month, NPI) %>%
  group_by(winter.year)

export <- data.frame(year=unique(sum.dat$winter.year),
                     NPI.ndjfm=tapply(sum.dat$NPI, sum.dat$winter.year, mean))

ggplot(export, aes(year, NPI.ndjfm)) +
  theme_bw() +
  geom_line()

write.csv(export[2:nrow(export),], "data/winter.NPI.csv", row.names = F)


ggplot(winter.dat, aes(winter.year, winter.mean)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~name, scales="free_y", nrow=2)

winter.dat$era <- ifelse(winter.dat$winter.year <= 1988, "1951-1988",
                         ifelse(winter.dat$winter.year %in% 1989:2013, "1989-2013", "2014-2019"))

cor.dat <- winter.dat %>%
  filter(winter.year <= 2019) %>%
  pivot_wider(names_from = name, values_from = winter.mean)

cor.dat <- na.omit(cor.dat)

era1 <- cor.dat %>%
  filter(era=="1951-1988")

ccf(era1$NPI, era1$AO, xlim=c(-2,2))

era2 <- cor.dat %>%
  filter(era=="1989-2013")

ccf(era2$NPI, era2$AO, xlim=c(-2,2))


