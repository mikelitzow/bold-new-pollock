
library(tidyverse)
library(mgcv)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(rgdal)
library(MuMIn)

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw())


# load confirmation data query to check these are complete
# dat <- read.csv("./data/survey data/pollock_survey_specimen_data_confirmation.csv")

# version with date!
dat <- read.csv(("./data/survey data/Litzow_pollock_02032021.csv"))

unique(dat$SURVEY) # EBS and NBS

# begin with fitting to EBS data only
# and only 1999-on to get good dates

dat$YEAR <- as.numeric(as.character((chron::years(dat$TOW_DATE))))

dat <- dat %>%
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

scale.dat <- plyr::ddply(dat, "AGE", transform, sc.weight = scale(WEIGHT))

# check
ggplot(scale.dat, aes(WEIGHT, sc.weight)) +
  geom_point() +
  facet_wrap(~AGE, scales = "free")

# looks good!

# and examine day of year distributions
scale.dat$Date <- chron::dates(as.character(scale.dat$TOW_DATE))

scale.dat$julian <- lubridate::yday(scale.dat$Date)

ggplot(scale.dat, aes(julian)) +
  geom_histogram(color="black", fill="grey") +
  facet_wrap(~YEAR)

# distribution of weights by age/year
ggplot(filter(scale.dat, AGE <= 12), aes(sc.weight)) +
  geom_histogram(color="black", fill="grey") +
  facet_grid(AGE~YEAR, scales = "free")

ggplot(filter(scale.dat, AGE <= 12), aes(sc.weight)) +
  geom_histogram(color="black", fill="grey") +
  facet_wrap(~AGE, scales = "free")

# a bit of skew!

## weight by year ----------------------------------------------------------

# create object to catch results and predict time evolution
mod.summary.out <- age.out <- data.frame()
new.dat <- data.frame(YEAR = 1999:2019, 
                      LATITUDE = mean(scale.dat$LATITUDE),
                      LONGITUDE = mean(scale.dat$LONGITUDE),
                      julian = mean(scale.dat$julian))

for(i in 1:12){
  
  mod <- gam(sc.weight ~ s(YEAR, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k=4),
             data=filter(scale.dat, AGE==i))
  
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
  geom_line() +
  scale_color_viridis_d()

## based on the above, responses to the recent anomalies
## appear to fall into three groups:
## age 1-2, age 5-8, age 9-12

# confirm that!
ggplot(age.out, aes(year, weight_anomaly)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~age)

## analyze by these group
mod.summary.out <- age.out <- data.frame()

new.dat <- data.frame(YEAR = 1999:2019, 
                      LATITUDE = mean(scale.dat$LATITUDE),
                      LONGITUDE = mean(scale.dat$LONGITUDE),
                      julian = mean(scale.dat$julian))
# age 1-2
  mod <- gam(sc.weight ~ s(YEAR, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k=4),
             data=filter(scale.dat, AGE %in% 1:2))
  
  summary(mod)
  
  plot(mod, se=F, resid=T, pages=1, rug=F)
  
  temp.out <- data.frame(age = "1-2",
                         edf = summary(mod)$edf,
                         nominal_p = summary(mod)$s.table[1,4])
  
  mod.summary.out <- rbind(mod.summary.out, temp.out)
  
  temp.out <- data.frame(age = "1-2",
                         year = 1999:2019,
                         weight_anomaly = predict.gam(mod, newdata = new.dat))
  
  age.out <- rbind(age.out, temp.out)
  

# 5-8
  mod <- gam(sc.weight ~ s(YEAR, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k=4),
             data=filter(scale.dat, AGE %in% 5:8))

  summary(mod)
  plot(mod, se=F, resid=T, pages=1, rug=F)
  
  temp.out <- data.frame(age = "5-8",
                         edf = summary(mod)$edf,
                         nominal_p = summary(mod)$s.table[1,4])
  
  mod.summary.out <- rbind(mod.summary.out, temp.out)
  
  temp.out <- data.frame(age = "5-8",
                         year = 1999:2019,
                         weight_anomaly = predict.gam(mod, newdata = new.dat))
  
  age.out <- rbind(age.out, temp.out)
  
# 9-12
  mod <- gam(sc.weight ~ s(YEAR, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k=4),
             data=filter(scale.dat, AGE %in% 9:12))
  
  summary(mod)
  plot(mod, se=F, resid=T, pages=1, rug=F)
  
  temp.out <- data.frame(age = "9-12",
                         edf = summary(mod)$edf,
                         nominal_p = summary(mod)$s.table[1,4])
  
  mod.summary.out <- rbind(mod.summary.out, temp.out)
  
  temp.out <- data.frame(age = "9-12",
                         year = 1999:2019,
                         weight_anomaly = predict.gam(mod, newdata = new.dat))
  
  age.out <- rbind(age.out, temp.out)
  

ggplot(age.out, aes(year, weight_anomaly, color=age)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d()

## temperature effect -------------------------------------------------------------

# object to catch results and predict time evolution
mod.summary.out <- sst.out <- data.frame()
new.dat <- data.frame(sst.amj = unique(scale.dat$sst.amj),
                      LATITUDE = mean(scale.dat$LATITUDE),
                      LONGITUDE = mean(scale.dat$LONGITUDE),
                      julian = mean(scale.dat$julian))

for(i in 1:12){
 
  mod <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE==i))
  
  temp.out <- data.frame(age = i,
                         edf = summary(mod)$edf[1],
                         nominal_p = summary(mod)$s.table[1,4])
  
  mod.summary.out <- rbind(mod.summary.out, temp.out)
  
  temp.out <- data.frame(age = i,
                         sst.amj = unique(scale.dat$sst.amj),
                         weight_anomaly = predict.gam(mod, newdata = new.dat))
  
  sst.out <- rbind(sst.out, temp.out)
  
}

sst.out$age <- as.factor(sst.out$age)

ggplot(sst.out, aes(sst.amj, weight_anomaly, color=age)) +
  geom_line() +
  scale_color_viridis_d()

# confirm that!
ggplot(sst.out, aes(sst.amj, weight_anomaly)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~age)

## analyze by these group
mod.summary.out <- sst.out <- data.frame()


# age 1-2
mod <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 1:2))

summary(mod)

temp.out <- data.frame(age = "1-2",
                       edf = summary(mod)$edf,
                       nominal_p = summary(mod)$s.table[1,4])

mod.summary.out <- rbind(mod.summary.out, temp.out)

temp.out <- data.frame(age = "1-2",
                       sst.amj = unique(scale.dat$sst.amj),
                       weight_anomaly = predict.gam(mod, newdata = new.dat))

sst.out <- rbind(sst.out, temp.out)


# 5-8
mod <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 5:8))

summary(mod)

temp.out <- data.frame(age = "5-8",
                       edf = summary(mod)$edf,
                       nominal_p = summary(mod)$s.table[1,4])

mod.summary.out <- rbind(mod.summary.out, temp.out)

temp.out <- data.frame(age = "5-8",
                       sst.amj = unique(scale.dat$sst.amj),
                       weight_anomaly = predict.gam(mod, newdata = new.dat))

sst.out <- rbind(sst.out, temp.out)

# 9-12
mod <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 9:12))

summary(mod)

temp.out <- data.frame(age = "9-12",
                       edf = summary(mod)$edf,
                       nominal_p = summary(mod)$s.table[1,4])

mod.summary.out <- rbind(mod.summary.out, temp.out)

temp.out <- data.frame(age = "9-12",
                       sst.amj = unique(scale.dat$sst.amj),
                       weight_anomaly = predict.gam(mod, newdata = new.dat))

sst.out <- rbind(sst.out, temp.out)


ggplot(sst.out, aes(sst.amj, weight_anomaly, color=age)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d()

## compare with sst*era model --------------------------------

# age 1-2
mod.null <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 1:2))

mod.alt <- gam(sc.weight ~ sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 1:2))

summary(mod.alt)

AICc_1.2 <- AICc(mod.null, mod.alt) 
AICc_1.2 # null model is better

# age 5-8
mod.null <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 5:8))

mod.alt <- gam(sc.weight ~ sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 5:8))

summary(mod.alt)

AICc_5.8 <- AICc(mod.null, mod.alt) 
AICc_5.8 # null model is better

# age 9-12
mod.null <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 9:12))

mod.alt <- gam(sc.weight ~ sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 9:12))

summary(mod.alt)

AICc_9.12 <- AICc(mod.null, mod.alt) 
AICc_9.12 # alt model is better; but nominal p-values are pretty high consider the # of obs in the model!

## add a size class:temp interaction

class.dat <- scale.dat %>%
  filter(AGE %in% c(1,2,9:12)) %>%
  mutate(age.class = as.factor(if_else(AGE %in% 1:2, "1-2", "9-12")))

mod1 <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=class.dat)
summary(mod1)

mod2 <- gam(sc.weight ~ sst.amj*age.class + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=class.dat)
summary(mod2)

mod3 <- gam(sc.weight ~ s(sst.amj, k = 6, by = age.class) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=class.dat)
summary(mod3)

AIC <- AICc(mod1, mod2, mod3)

AIC$model <- c("mod1", "mod2", "mod3")

AIC$delta.AICc <- AIC$AICc - min(AIC$AICc)

AIC <- AIC %>%
  arrange(delta.AICc) %>%
  select(model, df, AICc, delta.AICc)

AIC

plot(mod3, pages=1, se=F)

# so the interaction smooth model is clearly the best (mod3); 
# because the effects of date and lat, long are likely age-class specific,
# will plot models fit separately to each class

mod.1_2 <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 1:2))
png("./figs/age_1-2_scaled_weight_ebs_gam.png", width=8, height=8, units='in', res=300)
plot(mod.1_2, pages=1, se=F)
dev.off()

mod.9_12 <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 9:12))
png("./figs/age_9-12_scaled_weight_ebs_gam.png", width=8, height=8, units='in', res=300)
plot(mod.9_12, pages=1, se=F)
dev.off()

mod.9_12 <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 9:12))
plot(mod.9_12, pages=1, se=F)
