## reconcile the two data versions that we have...

library(tidyverse)
library(mgcv)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load confirmation data query to check these are complete
dat <- read.csv("./data/survey data/pollock_survey_specimen_data_confirmation.csv")

unique(dat$SURVEY) # EBS and NBS

# begin with fitting to EBS data only
# and only 1999-on to get good dates

dat <- dat %>%
  mutate(YEAR=floor(CRUISE/100)) %>%
  filter(SURVEY == "EBS", YEAR >= 1999) 

# exploratory plots
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


scale_this <- function(x) as.vector(scale(x))
