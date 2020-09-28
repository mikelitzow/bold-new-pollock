library(tidyverse)
theme_set(theme_bw())

d1 <- read.csv("data/survey data/Poll_LW_BS_GOA.csv")

head(d1)
tail(d1)
unique(d1$SPECIES_CODE)

d2 <- read.csv("data/survey data/poll_specimen_haul.csv")

head(d2)
unique(d2$REGION)
unique(d2$SPECIES_CODE)
unique(d2$SURVEY)

d1 <- d1 %>%
  filter(REGION=="BS") %>%
  select(HAULJOIN, SPECIMENID, LENGTH, WEIGHT, AGE)

d2 <- d2 %>%
  filter(SURVEY=="EBS") %>%
  select(HAULJOIN, SPECIMENID, LENGTH, WEIGHT, AGE)

# confirm that there are no hauls in both data sets

intersect(d1$HAULJOIN, d2$HAULJOIN)
# looks good!

data <- rbind(d1, d2)

tt <- read.csv("data/survey data/poll_cpue_by_sex_cm.csv")
head(tt)
tail(tt)

hist(data$LENGTH)
nrow(data)

# limit to 10-20 cm

data <- data %>%
  filter(LENGTH >= 100 & LENGTH <= 200)

sum.cpue <- tt %>%
  filter(LENGTH >= 100 & LENGTH <= 200) %>%
  group_by(HAULJOIN) %>%
  summarise(sum.cpue=sum(NUMCPUE_LENGTH))

data <- left_join(data, sum.cpue)


ggplot(data, aes(LENGTH, WEIGHT)) + 
  geom_point() +
  geom_smooth(method="gam")

# outlier!
arrange(data, desc(WEIGHT))
# remove that one...
data <- data %>%
  filter(WEIGHT<100)

ggplot(data, aes(LENGTH, WEIGHT)) + 
  geom_point() +
  geom_smooth(method="gam")

# look at strata by year
ff <- function(x) sum(!is.na(x))
strata <- tapply(tt$HAUL, list(tt$YEAR, tt$STRATUM), ff)
strata  

# add station, stratum, year
xtra <- tt %>%
  select(YEAR, STRATUM, STATIONID, HAULJOIN)

data <- left_join(data, xtra) 

unique(data$YEAR)

# exclude data without years...
keep <- !is.na(data$YEAR)
data <- data[keep,]

# and fit a gam
mod <- mgcv::gam(WEIGHT ~ s(LENGTH, k=4), data=data)
summary(mod)
mgcv::plot.gam(mod, resid=T, pch=1, se=T, shade=T)

data$RESIDUAL.WEIGHT <- resid(mod)

# missing cpue seems to be causing a problem - drop for now
look <- is.na(data$sum.cpue)
View(data[look,])
data <- data[!look,]

# get average residual for each haul
haul.mean <- data %>%
  group_by(HAULJOIN) %>%
  summarise(mean.residual=mean(RESIDUAL.WEIGHT),
            year=mean(YEAR),
            sum.cpue=mean(sum.cpue))

annual.mean <- plyr::ddply(haul.mean, "year", function(x) {
  mu <- weighted.mean(x$mean.residual, log(x$sum.cpue))
  data.frame(mu=mu)
}
  )

# load climate data
clim.dat <- read.csv("data/climate data.csv")

ggplot(clim.dat, aes(south.sst.ndjfm, south.sst.amj)) + 
  geom_point()

annual.mean$sst.ndjfm <- clim.dat$south.sst.ndjfm[match(annual.mean$year, clim.dat$year)] 
annual.mean$era <- as.factor(ifelse(annual.mean$year < 2014, 1, 2))
annual.mean$sst.amj <- clim.dat$south.sst.amj[match(annual.mean$year, clim.dat$year)] 
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(annual.mean, aes(sst.ndjfm, mu, color=era)) +
  geom_point()

ggplot(annual.mean, aes(sst.amj, mu, color=era)) +
  geom_point() 

mod <- mgcv::gam(mu ~ s(sst.amj, k=4), data=annual.mean)
summary(mod)

plot(mod, se=T, shade=T, resid=T, pch=1)

# try fitting a model to lat/long as well