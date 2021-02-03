
library(tidyverse)
library(mgcv)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(rgdal)

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw())


# load confirmation data query to check these are complete
dat <- read.csv("./data/survey data/pollock_survey_specimen_data_confirmation.csv")

unique(dat$SURVEY) # EBS and NBS

# begin with fitting to EBS data only
# and only 1999-on to get good dates

dat <- dat %>%
  mutate(YEAR=floor(CRUISE/100)) %>%
  filter(SURVEY == "EBS", YEAR >= 1999) 

# exploratory plots
space.plot <- dat %>%
  group_by(LATITUDE, LONGITUDE, YEAR) %>%
  summarise(size = log(n()+1))

ak <- ne_countries(scale = "medium", returnclass = "sf", continent="north america")

min <- min(space.plot$size)
max <- max(space.plot$size)

map.plot <- ggplot(ak) +
  geom_point(data=space.plot, aes(LONGITUDE, LATITUDE)) +
  scale_radius(range=c(min, max)) +
  geom_sf(fill="darkgoldenrod3", color=NA) + 
  coord_sf(xlim = c(-180, -156), ylim = c(52, 66), expand = FALSE) +
  facet_wrap(~YEAR)

map.plot # pretty light before 2006!

ggplot(filter(dat, AGE <= 15), aes(AGE)) +
  geom_histogram(fill="grey", color="black", bins=15) +
  facet_wrap(~YEAR, scales="free_y")
  
  
ggplot(dat, aes(AGE, WEIGHT)) +
  geom_point() +
  facet_wrap(~YEAR)

ggplot(filter(dat, AGE <= 12), aes(YEAR, WEIGHT)) +
  geom_point() +
  facet_wrap(~AGE, scales="free_y") + 
  geom_smooth()


# load climate data
clim.dat <- read.csv("data/climate data.csv")

ggplot(clim.dat, aes(south.sst.ndjfm, south.sst.amj)) + 
  geom_point()

ggplot(clim.dat, aes(year, south.sst.amj)) +
  geom_line() +
  geom_point()

dat$sst.ndjfm <- clim.dat$south.sst.ndjfm[match(dat$YEAR, clim.dat$year)] 
dat$era <- as.factor(if_else(dat$YEAR < 2014, 1, 2))
dat$sst.amj <- clim.dat$south.sst.amj[match(dat$YEAR, clim.dat$year)] 

ggplot(filter(dat, AGE <= 12), aes(sst.amj, WEIGHT)) +
  geom_point() +
  facet_wrap(~AGE, scales="free_y") + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))

## scale length by age to allow comparison / analysis across ages

scale_this <- function(x) as.vector(scale(x))

scale.dat <- plyr::ddply(dat, "AGE", transform, sc.weight = scale(WEIGHT))

# check
ggplot(scale.dat, aes(WEIGHT, sc.weight)) +
  geom_point() +
  facet_wrap(~AGE, scales = "free")

# looks good!

# exploratory TS plots - one panel!

# object to catch results and predict time evolution
mod.summary.out <- age.out <- data.frame()
new.dat <- data.frame(YEAR = 1999:2019)

for(i in 1:12){
  
  mod <- gam(sc.weight ~ s(YEAR, k=6), data=filter(scale.dat, AGE==i))
  
  temp.out <- data.frame(age = i,
                         edf = summary(mod)$edf,
                         nominal_p = summary(mod)$s.table[1,4])
  
  mod.summary.out <- rbind(mod.summary.out, temp.out)
  
  temp.out <- data.frame(age = i,
                         year = 1999:2019,
                         weight_anomaly = predict.gam(mod, newdata = new.dat))
  
age.out <- rbind(age.out, temp.out)
  
}

age.out$age <- as.factor(age.out$age)
ggplot(age.out, aes(year, weight_anomaly, color=age)) +
  geom_line() 

mod1 <- gam(WEIGHT ~ s(sst.amj, k = 3) + te(LATITUDE, LONGITUDE), data=dat[dat$AGE == 1,])
summary(mod1)
plot(mod1, se=F, select=1)

mod2 <- gam(WEIGHT ~ s(sst.amj, k = 3) + te(LATITUDE, LONGITUDE), data=dat[dat$AGE == 2,])
summary(mod2)
plot(mod2, se=F, select=1)

mod3 <- gam(WEIGHT ~ s(sst.amj, k = 3) + te(LATITUDE, LONGITUDE), data=dat[dat$AGE == 3,])
summary(mod3)
plot(mod3, se=F, select=1)

mod4 <- gam(WEIGHT ~ s(sst.amj, k = 3) + te(LATITUDE, LONGITUDE), data=dat[dat$AGE == 4,])
summary(mod4)
plot(mod4, se=F, select=1)

mod5 <- gam(WEIGHT ~ s(sst.amj, k = 3) + te(LATITUDE, LONGITUDE), data=dat[dat$AGE == 5,])
summary(mod5)
plot(mod5, se=F, select=1)

mod6 <- gam(WEIGHT ~ s(sst.amj, k = 3) + te(LATITUDE, LONGITUDE), data=dat[dat$AGE == 6,])
summary(mod6)
plot(mod6, se=F, select=1)

mod7 <- gam(WEIGHT ~ s(sst.amj, k = 3) + te(LATITUDE, LONGITUDE), data=dat[dat$AGE == 7,])
summary(mod7)
plot(mod7, se=F, select=1)



