
library(tidyverse)
library(mgcv)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(rgdal)
library(MuMIn)
library(visreg)

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw())


# load confirmation data query to check these are complete
# old.dat <- read.csv("./data/survey data/pollock_survey_specimen_data_confirmation.csv")

# version with date!
dat <- read.csv(("./data/survey data/Litzow_pollock_02032021.csv"))

unique(dat$SURVEY) # EBS and NBS

# begin with fitting to EBS data only
# and only 1999-on to get good weights

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
  geom_point(data=space.plot, aes(LONGITUDE, LATITUDE), size = 0.5) +
  geom_sf(fill="darkgoldenrod3", color=NA) + 
  coord_sf(xlim = c(-180, -156), ylim = c(52, 66), expand = FALSE) +
  facet_wrap(~YEAR)

map.plot # pretty light before 2006!

ggsave("./figs/age_weight_sampling_maps.png", width=7, height=9, units='in')

ggplot(filter(dat, AGE <= 15), aes(AGE)) +
  geom_histogram(fill="grey", color="black", bins=15) +
  facet_wrap(~YEAR, scales="free_y")

ggsave("./figs/age_sample_size_histograms.png", width=9, height=7, units='in')

ggplot(dat, aes(AGE, WEIGHT)) +
  geom_point() +
  facet_wrap(~YEAR)

ggplot(filter(dat, AGE <= 12), aes(YEAR, WEIGHT)) +
  geom_point() +
  facet_wrap(~AGE, scales="free_y") + 
  geom_smooth()

ggsave("./figs/age_weight_scatter.png", width=9, height=6, units='in')

# load climate data
clim.dat <- read.csv("data/climate data.csv")

sst.scatter <- ggplot(clim.dat, aes(south.sst.ndjfm, south.sst.amj)) + 
  geom_point()
sst.scatter

amj.ts <- ggplot(clim.dat, aes(year, south.sst.amj)) +
  geom_line() +
  geom_point()

amj.ts + ylab("April-June SST (°C)") + xlab("Year") + theme_bw()

png("./figs/winter_spring_ebs_sst_plots.png", width=6, height = 3, units='in', res=300)

ggpubr::ggarrange(sst.scatter, amj.ts, ncol=2, widths = c(0.45, 0.55))

dev.off()

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
ggsave("./figs/age_sc.weight_hist.png", width=9, height=6, units='in')


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
                         weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))
  
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

ggsave("./figs/weight_time_series_smooths_by_age.png", width=9, height=6, units='in')

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
                         weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))
  
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
                         weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))
  
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
                         weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))
  
  age.out <- rbind(age.out, temp.out)
  

ggplot(age.out, aes(year, weight_anomaly, color=age)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d()

## temperature effect -------------------------------------------------------------

# object to catch results and predict time evolution
mod.summary.out <- sst.out <- data.frame()
new.dat <- data.frame(sst.amj = seq(from = min(scale.dat$sst.amj), to = max(scale.dat$sst.amj), length.out = 100),
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
                         sst.amj = seq(from = min(scale.dat$sst.amj), to = max(scale.dat$sst.amj), length.out = 100),
                         weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))
  
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

ggsave("./figs/age_sc.weight_amj.sst_smooths.png", width=9, height=6, units='in')

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
                       sst.amj = seq(from = min(scale.dat$sst.amj), to = max(scale.dat$sst.amj), length.out = 100),
                       weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))

sst.out <- rbind(sst.out, temp.out)


# 5-8
mod <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 5:8))

summary(mod)

temp.out <- data.frame(age = "5-8",
                       edf = summary(mod)$edf,
                       nominal_p = summary(mod)$s.table[1,4])

mod.summary.out <- rbind(mod.summary.out, temp.out)

temp.out <- data.frame(age = "5-8",
                       sst.amj = seq(from = min(scale.dat$sst.amj), to = max(scale.dat$sst.amj), length.out = 100),
                       weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))

sst.out <- rbind(sst.out, temp.out)

# 9-12
mod <- gam(sc.weight ~ s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=filter(scale.dat, AGE %in% 9:12))

summary(mod)

temp.out <- data.frame(age = "9-12",
                       edf = summary(mod)$edf,
                       nominal_p = summary(mod)$s.table[1,4])

mod.summary.out <- rbind(mod.summary.out, temp.out)

temp.out <- data.frame(age = "9-12",
                       sst.amj = seq(from = min(scale.dat$sst.amj), to = max(scale.dat$sst.amj), length.out = 100),
                       weight_anomaly = predict.gam(mod, newdata = new.dat, type = "response"))

sst.out <- rbind(sst.out, temp.out)


ggplot(sst.out, aes(sst.amj, weight_anomaly, color=age)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d()

ggsave("./figs/age_class_vs_sst_smooths.png", width = 6, height = 5, units = 'in')

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


#log weight models for R skew====================================================================

#diagnostic plots for models above show they are very right skewed
#does analyzing log(weight) help diagnostics?
#YES diagnostic plots look MUCH BETTER


ggplot(dat[which(dat$AGE<11),], aes(WEIGHT, LENGTH)) + geom_point() + facet_wrap(~AGE)

ggplot(dat[which(dat$AGE<11),], aes(WEIGHT, LENGTH)) + geom_point() + facet_wrap(~AGE, scales="free")

#because we still want to be able to compare across ages, will now log then scale weight
scale.dat$logWEIGHT <- log(scale.dat$WEIGHT)
logscale.dat <- plyr::ddply(scale.dat, "AGE", transform, log_sc_weight = scale(logWEIGHT))

# check
ggplot(logscale.dat, aes(WEIGHT, log_sc_weight)) +
  geom_point() +
  facet_wrap(~AGE, scales = "free")

# looks good!




#repeating steps from above to compare w v w/o era*sst 

#March 4 2020 we are dropping era from the models because there just isn't enough SST variation in late era 
#the cold temps are entirely missing, seems to be leading to nonsensical models

#==
dat1_2 <- filter(logscale.dat, AGE %in% 1:2)

# age 1-2
log.null1 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat1_2)
gam.check(log.null1)

#log.alt1 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat1_2)
#gam.check(log.alt1)

# log.by1 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat1_2)
# gam.check(log.by1) #

summary(log.alt1)

AICc_1.2 <- AICc(log.null1) 
AICc_1.2 # 


#==
dat5_8 <- filter(logscale.dat, AGE %in% 5:8)

# age 5-8
log.null5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat5_8)
gam.check(log.null5) #not great

#log.alt5 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat5_8)
#gam.check(log.alt5)

# log.by5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat5_8)
# gam.check(log.by5)

summary(log.alt5)

AICc_5.8 <- AICc(log.null5) 
AICc_5.8 # 


#==
dat9_12 <- filter(logscale.dat, AGE %in% 9:12)

# age 9-12
log.null9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat9_12)
gam.check(log.null9)

#log.alt9 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat9_12)
#gam.check(log.alt9)

#log.by9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat9_12)
#gam.check(log.by9) #bad hessian

summary(log.alt9)

AICc_9.12 <- AICc(log.null9) 
AICc_9.12 # 


#conclusions don't appear to change vs previous (non-log) models

#temp autocor?===================================================================================

#1 & 2==
#log.null1 fit above
draw(log.null1, select = 1)
draw(log.null1, select = 2)
draw(log.null1, select = 3)

#autocor?
E <- residuals(log.null1, type="deviance")
I1 <- !is.na(dat1_2$log_sc_weight)
Efull <- vector(length=length(dat1_2$log_sc_weight))
Efull <- NA
Efull[I1] <- E
plot(dat1_2$YEAR, Efull) #

res1 <- dat1_2
res1$Efull <- Efull

ggplot(res1, aes(YEAR, Efull)) + geom_point() + geom_smooth()

ggplot(res1, aes(as.factor(YEAR), Efull))  + geom_boxplot()

ggplot(res1, aes(as.factor(julian), Efull))  + geom_boxplot()

ggplot(res1, aes(sst.amj, Efull))  + geom_point()

ggplot(res1, aes(AGE, Efull))  + geom_point()



#5 - 8==
#log.null5 fit above
draw(log.null5, select = 1)
draw(log.null5, select = 2)
draw(log.null5, select = 3)

#autocor?
E <- residuals(log.null5, type="deviance")
I1 <- !is.na(dat5_8$log_sc_weight)
Efull <- vector(length=length(dat5_8$log_sc_weight))
Efull <- NA
Efull[I1] <- E
plot(dat5_8$YEAR, Efull) #

res5 <- dat5_8
res5$Efull <- Efull

ggplot(res5, aes(YEAR, Efull)) + geom_point() + geom_smooth()

ggplot(res5, aes(as.factor(YEAR), Efull))  + geom_boxplot()

ggplot(res5, aes(as.factor(julian), Efull))  + geom_boxplot()

ggplot(res5, aes(sst.amj, Efull))  + geom_point()

ggplot(res5, aes(AGE, Efull))  + geom_point()






#9 - 12==
#log.null5 fit above
draw(log.null9, select = 1)
draw(log.null9, select = 2)
draw(log.null9, select = 3)

#autocor?
E <- residuals(log.null9, type="deviance")
I1 <- !is.na(dat9_12$log_sc_weight)
Efull <- vector(length=length(dat9_12$log_sc_weight))
Efull <- NA
Efull[I1] <- E
plot(dat9_12$YEAR, Efull) #

res9 <- dat9_12
res9$Efull <- Efull

ggplot(res9, aes(YEAR, Efull)) + geom_point() + geom_smooth()

ggplot(res9, aes(as.factor(YEAR), Efull))  + geom_boxplot()

ggplot(res9, aes(as.factor(julian), Efull))  + geom_boxplot()

ggplot(res9, aes(sst.amj, Efull))  + geom_point()

ggplot(res9, aes(AGE, Efull))  + geom_point()


#last yrs weight anomaly===========================================================================

wmeans <- scale.dat %>% group_by(YEAR, AGE) %>% summarize(mean_annual_weight_at_age=mean(WEIGHT, na.rm=TRUE))

wtotalmeans <- scale.dat %>% group_by(AGE) %>% summarize(mean_overall_weight_at_age=mean(WEIGHT, na.rm=TRUE))

bothmeans <- left_join(wmeans, wtotalmeans)

#lag to same age in previous year
bothmeans$sameage_lastyr_weight_anom <- NA
bothmeans$sameage_lastyr_weight_anom <- bothmeans$mean_annual_weight_at_age - bothmeans$mean_overall_weight_at_age 

bothmeans$lag_year <- NA
bothmeans$lag_year <- bothmeans$YEAR + 1

#wmeans$temp_1yr_lag <- wmeans$mean_annual_sstamj

mergemeans <- bothmeans[,c(2,5:6)]

lagdat1 <- left_join(logscale.dat, mergemeans, by = c("YEAR" = "lag_year", "AGE"="AGE"))

#lag to previous age in previous year
lagdat1$prev_age <- NA
lagdat1$prev_age <- lagdat1$AGE - 1

mergemeans2 <- bothmeans[,c(2,5:6)]
mergemeans2$prevage_lastyr_weight_anom <- mergemeans2$sameage_lastyr_weight_anom
mergemeans2 <- mergemeans2[,c(1,3:4)]

lagdat <- left_join(lagdat1, mergemeans2, by = c("YEAR" = "lag_year", "prev_age"="AGE"))

lag12 <- lagdat[which(lagdat$AGE<3),]
lag34 <- lagdat[which(lagdat$AGE<5 & lagdat$AGE>2),]
lag58 <- lagdat[which(lagdat$AGE<9 & lagdat$AGE>4),]
lag912 <- lagdat[which(lagdat$AGE<13 & lagdat$AGE>8),]
lag1315 <- lagdat[which(lagdat$AGE<16 & lagdat$AGE>12),]

lag3only <- lagdat[which(lagdat$AGE==3),]
lag4only <- lagdat[which(lagdat$AGE==4),]
lag5only <- lagdat[which(lagdat$AGE==5),]
lag6only <- lagdat[which(lagdat$AGE==6),]
lag7only <- lagdat[which(lagdat$AGE==7),]
lag8only <- lagdat[which(lagdat$AGE==8),]
lag9only <- lagdat[which(lagdat$AGE==9),]
lag10only <- lagdat[which(lagdat$AGE==10),]
lag11only <- lagdat[which(lagdat$AGE==11),]
lag12only <- lagdat[which(lagdat$AGE==12),]
lag13only <- lagdat[which(lagdat$AGE==13),]


#models==


# age 1
#age 1 has to be different because no previous year
lag1 <- lag12[which(lag12$AGE==1),]
lag.null1 <- gam(log_sc_weight ~ s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(sameage_lastyr_weight_anom, k=4), data=lag1, method="REML")
gam.check(lag.null1) #
summary(lag.null1) #wow R2 SO MUCH BETTER
plot(lag.null1) #v wiggly
draw(lag.null1, select=1)
draw(lag.null1, select=2)
draw(lag.null1, select=3)
draw(lag.null1, select=4)

v1 <- visreg(lag.null1, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")


AICc(lag.null1) #

AICc_1lag <- AICc(lag.null1) #
AICc_1lag # 

R2.a1 <- 1-var(residuals(lag.null1))/(var(model.response(model.frame(lag.null1))))

lag.null1.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(sameage_lastyr_weight_anom, k=4), data=lag1)
R2.a1.nosst <- 1-var(residuals(lag.null1.nosst))/(var(model.response(model.frame(lag.null1.nosst))))
R2.a1 - R2.a1.nosst 
AICc_1lag_nosst <- AICc(lag.null1.nosst) #
AICc_1lag_nosst - AICc_1lag

lag.null1.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(sameage_lastyr_weight_anom, k=4), data=lag1)
R2.a1.nolatlong <- 1-var(residuals(lag.null1.nolatlong))/(var(model.response(model.frame(lag.null1.nolatlong))))
R2.a1 - R2.a1.nolatlong 
AICc_1lag_nolatlong <- AICc(lag.null1.nolatlong) #
AICc_1lag_nolatlong - AICc_1lag

lag.null1.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(sameage_lastyr_weight_anom, k=4), data=lag1)
R2.a1.nojul <- 1-var(residuals(lag.null1.nojul))/(var(model.response(model.frame(lag.null1.nojul))))
R2.a1 - R2.a1.nojul 
AICc_1lag_nojul <- AICc(lag.null1.nojul) #
AICc_1lag_nojul - AICc_1lag

lag.null1.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(sameage_lastyr_weight_anom, k=4), data=lag1)
R2.a1.nolag <- 1-var(residuals(lag.null1.nolag))/(var(model.response(model.frame(lag.null1.nolag))))
R2.a1 - R2.a1.nolag 
AICc_1lag_nolag <- AICc(lag.null1.nolag) #
AICc_1lag_nolag - AICc_1lag


# age 2
lag2 <- lag12[which(lag12$AGE==2),]
lag.null2 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag2)
gam.check(lag.null2) #
summary(lag.null2)
plot(lag.null2) #v wiggly
draw(lag.null2, select=1)
draw(lag.null2, select=2)
draw(lag.null2, select=3)
draw(lag.null2, select=4)

v2 <- visreg(lag.null2, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null2) #

AICc_2lag <- AICc(lag.null2) #
AICc_2lag #

R2.a2 <- 1-var(residuals(lag.null2))/(var(model.response(model.frame(lag.null2))))

lag.null2.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag2)
R2.a2.nosst <- 1-var(residuals(lag.null2.nosst))/(var(model.response(model.frame(lag.null2.nosst))))
R2.a2 - R2.a2.nosst 
AICc_2lag_nosst <- AICc(lag.null2.nosst) #
AICc_2lag_nosst - AICc_2lag

lag.null2.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag2)
R2.a2.nolatlong <- 1-var(residuals(lag.null2.nolatlong))/(var(model.response(model.frame(lag.null2.nolatlong))))
R2.a2 - R2.a2.nolatlong 
AICc_2lag_nolatlong <- AICc(lag.null2.nolatlong) #
AICc_2lag_nolatlong - AICc_2lag

lag.null2.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag2)
R2.a2.nojul <- 1-var(residuals(lag.null2.nojul))/(var(model.response(model.frame(lag.null2.nojul))))
R2.a2 - R2.a2.nojul 
AICc_2lag_nojul <- AICc(lag.null2.nojul) #
AICc_2lag_nojul - AICc_2lag


lag.null2.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag2)
R2.a2.nolag <- 1-var(residuals(lag.null2.nolag))/(var(model.response(model.frame(lag.null2.nolag))))
R2.a2 - R2.a2.nolag 
AICc_2lag_nolag <- AICc(lag.null2.nolag) #
AICc_2lag_nolag - AICc_2lag


# age 3-4
lag.null3 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag34, method="REML")
gam.check(lag.null3)
summary(lag.null3)
plot(lag.null3)


visreg(lag.null3, "sst.amj", scale="response",ylab="Log of scaled weight-at-age", xlab="April-June SST")


AICc(lag.null3, log.null3) #better with lagged weight anoms

AICc_3.4lag <- AICc(lag.null3) 
AICc_3.4lag #




# age 5-8
lag.null5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag58)
gam.check(lag.null5)
summary(lag.null5)
plot(lag.null5)

# lag.alt5 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(sameage_lastyr_weight_anom), data=lag58)
# gam.check(lag.alt5)
# 
# lag.by5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(sameage_lastyr_weight_anom), data=lag58)
# gam.check(lag.by5) #bad hessian

visreg(lag.null5, "sst.amj", scale="response",ylab="Log of scaled weight-at-age", xlab="April-June SST")


AICc(lag.null5, log.null5) #better with lagged weight anoms

AICc_5.8lag <- AICc(lag.null5) 
AICc_5.8lag # null better



# age 9-12
lag.null9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=3) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag912)
gam.check(lag.null9) #hessian good at k=3 for sst, bad above that
summary(lag.null9)
plot(lag.null9)

# lag.alt9 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(sameage_lastyr_weight_anom), data=lag912)
# gam.check(lag.alt9) #bad hessian
# 
# lag.by9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(sameage_lastyr_weight_anom), data=lag912)
# gam.check(lag.by9) #bad hessian

visreg(lag.null9, "sst.amj", scale="response",ylab="Log of scaled weight-at-age", xlab="April-June SST")

summary(lag.alt9)
AICc(lag.null9, log.null9) #better w lagged weight anom

AICc_9.12lag <- AICc(lag.null9) 
AICc_9.12lag # null better


# age 13-15
lag.null13 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag1315, method="REML")
gam.check(lag.null13)
summary(lag.null13)
plot(lag.null13)


visreg(lag.null13, "sst.amj", scale="response",ylab="Log of scaled weight-at-age", xlab="April-June SST")


#AICc(lag.null13, log.null13) #better with lagged weight anoms

AICc_13.15lag <- AICc(lag.null13) 
AICc_13.15lag #


#each age modeled separately===========


# age 3

lag.null3 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag3only)
gam.check(lag.null3) #
summary(lag.null3)
plot(lag.null3) #
draw(lag.null3, select=1)
draw(lag.null3, select=2)
draw(lag.null3, select=3)
draw(lag.null3, select=4)

lag3complete <- lag3only[complete.cases(lag3only)==TRUE,]
lag.null3_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4) + 
                         (1|YEAR/HAUL), data=lag3complete)


v3 <- visreg(lag.null3, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")


AICc_3lag <- AICc(lag.null3) #
AICc_3lag # 

R2.a3 <- 1-var(residuals(lag.null3))/(var(model.response(model.frame(lag.null3))))

lag.null3.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag3only)
R2.a3.nosst <- 1-var(residuals(lag.null3.nosst))/(var(model.response(model.frame(lag.null3.nosst))))
R2.a3 - R2.a3.nosst 
AICc_3lag_nosst <- AICc(lag.null3.nosst) #
AICc_3lag_nosst - AICc_3lag

lag.null3.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag3only)
R2.a3.nolatlong <- 1-var(residuals(lag.null3.nolatlong))/(var(model.response(model.frame(lag.null3.nolatlong))))
R2.a3 - R2.a3.nolatlong 
AICc_3lag_nolatlong <- AICc(lag.null3.nolatlong) #
AICc_3lag_nolatlong - AICc_3lag

lag.null3.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag3only)
R2.a3.nojul <- 1-var(residuals(lag.null3.nojul))/(var(model.response(model.frame(lag.null3.nojul))))
R2.a3 - R2.a3.nojul 
AICc_3lag_nojul <- AICc(lag.null3.nojul) #
AICc_3lag_nojul - AICc_3lag

lag.null3.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag3only)
R2.a3.nolag <- 1-var(residuals(lag.null3.nolag))/(var(model.response(model.frame(lag.null3.nolag))))
R2.a3 - R2.a3.nolag 
AICc_3lag_nolag <- AICc(lag.null3.nolag) #
AICc_3lag_nolag - AICc_3lag


# age 4

lag.null4 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag4only)
gam.check(lag.null4) #
summary(lag.null4)
plot(lag.null4) #
draw(lag.null4, select=1)
draw(lag.null4, select=2)
draw(lag.null4, select=3)
draw(lag.null4, select=4)

v4 <- visreg(lag.null4, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null4) #

AICc_4lag <- AICc(lag.null4) #
AICc_4lag

R2.a4 <- 1-var(residuals(lag.null4))/(var(model.response(model.frame(lag.null4))))

lag.null4.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag4only)
R2.a4.nosst <- 1-var(residuals(lag.null4.nosst))/(var(model.response(model.frame(lag.null4.nosst))))
R2.a4 - R2.a4.nosst 
AICc_4lag_nosst <- AICc(lag.null4.nosst) #
AICc_4lag_nosst - AICc_4lag

lag.null4.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag4only)
R2.a4.nolatlong <- 1-var(residuals(lag.null4.nolatlong))/(var(model.response(model.frame(lag.null4.nolatlong))))
R2.a4 - R2.a4.nolatlong 
AICc_4lag_nolatlong <- AICc(lag.null4.nolatlong) #
AICc_4lag_nolatlong - AICc_4lag

lag.null4.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag4only)
R2.a4.nojul <- 1-var(residuals(lag.null4.nojul))/(var(model.response(model.frame(lag.null4.nojul))))
R2.a4 - R2.a4.nojul 
AICc_4lag_nojul <- AICc(lag.null4.nojul) #
AICc_4lag_nojul - AICc_4lag

lag.null4.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag4only)
R2.a4.nolag <- 1-var(residuals(lag.null4.nolag))/(var(model.response(model.frame(lag.null4.nolag))))
R2.a4 - R2.a4.nolag 
AICc_4lag_nolag <- AICc(lag.null4.nolag) #
AICc_4lag_nolag - AICc_4lag


# age 5

lag.null5 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag5only)
gam.check(lag.null5) #
summary(lag.null5)
plot(lag.null5) #
draw(lag.null5, select=1)
draw(lag.null5, select=2)
draw(lag.null5, select=3)
draw(lag.null5, select=4)

v5 <- visreg(lag.null5, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null5) #

AICc_5lag <- AICc(lag.null5) #
AICc_5lag

R2.a5 <- 1-var(residuals(lag.null5))/(var(model.response(model.frame(lag.null5))))

lag.null5.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag5only)
R2.a5.nosst <- 1-var(residuals(lag.null5.nosst))/(var(model.response(model.frame(lag.null5.nosst))))
R2.a5 - R2.a5.nosst 
AICc_5lag_nosst <- AICc(lag.null5.nosst) #
AICc_5lag_nosst - AICc_5lag

lag.null5.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag5only)
R2.a5.nolatlong <- 1-var(residuals(lag.null5.nolatlong))/(var(model.response(model.frame(lag.null5.nolatlong))))
R2.a5 - R2.a5.nolatlong 
AICc_5lag_nolatlong <- AICc(lag.null5.nolatlong) #
AICc_5lag_nolatlong - AICc_5lag

lag.null5.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag5only)
R2.a5.nojul <- 1-var(residuals(lag.null5.nojul))/(var(model.response(model.frame(lag.null5.nojul))))
R2.a5 - R2.a5.nojul 
AICc_5lag_nojul <- AICc(lag.null5.nojul) #
AICc_5lag_nojul - AICc_5lag

lag.null5.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag5only)
R2.a5.nolag <- 1-var(residuals(lag.null5.nolag))/(var(model.response(model.frame(lag.null5.nolag))))
R2.a5 - R2.a5.nolag 
AICc_5lag_nolag <- AICc(lag.null5.nolag) #
AICc_5lag_nolag - AICc_5lag


# age 6

lag.null6 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag6only)
gam.check(lag.null6) #
summary(lag.null6)
plot(lag.null6) #
draw(lag.null6, select=1)
draw(lag.null6, select=2)
draw(lag.null6, select=3)
draw(lag.null6, select=4)

v6 <- visreg(lag.null6, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null6) #

AICc_6lag <- AICc(lag.null6) #
AICc_6lag 

R2.a6 <- 1-var(residuals(lag.null6))/(var(model.response(model.frame(lag.null6))))

lag.null6.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag6only)
R2.a6.nosst <- 1-var(residuals(lag.null6.nosst))/(var(model.response(model.frame(lag.null6.nosst))))
R2.a6 - R2.a6.nosst 
AICc_6lag_nosst <- AICc(lag.null6.nosst) #
AICc_6lag_nosst - AICc_6lag

lag.null6.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag6only)
R2.a6.nolatlong <- 1-var(residuals(lag.null6.nolatlong))/(var(model.response(model.frame(lag.null6.nolatlong))))
R2.a6 - R2.a6.nolatlong 
AICc_6lag_nolatlong <- AICc(lag.null6.nolatlong) #
AICc_6lag_nolatlong - AICc_6lag

lag.null6.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag6only)
R2.a6.nojul <- 1-var(residuals(lag.null6.nojul))/(var(model.response(model.frame(lag.null6.nojul))))
R2.a6 - R2.a6.nojul 
AICc_6lag_nojul <- AICc(lag.null6.nojul) #
AICc_6lag_nojul - AICc_6lag

lag.null6.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag6only)
R2.a6.nolag <- 1-var(residuals(lag.null6.nolag))/(var(model.response(model.frame(lag.null6.nolag))))
R2.a6 - R2.a6.nolag 
AICc_6lag_nolag <- AICc(lag.null6.nolag) #
AICc_6lag_nolag - AICc_6lag


# age 7

lag.null7 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag7only)
gam.check(lag.null7) #
summary(lag.null7)
plot(lag.null7) #
draw(lag.null7, select=1)
draw(lag.null7, select=2)
draw(lag.null7, select=3)
draw(lag.null7, select=4)

v7 <- visreg(lag.null7, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null7) #

AICc_7lag <- AICc(lag.null7) #
AICc_7lag  

R2.a7 <- 1-var(residuals(lag.null7))/(var(model.response(model.frame(lag.null7))))

lag.null7.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag7only)
R2.a7.nosst <- 1-var(residuals(lag.null7.nosst))/(var(model.response(model.frame(lag.null7.nosst))))
R2.a7 - R2.a7.nosst 
AICc_7lag_nosst <- AICc(lag.null7.nosst) #
AICc_7lag_nosst - AICc_7lag


lag.null7.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag7only)
R2.a7.nolatlong <- 1-var(residuals(lag.null7.nolatlong))/(var(model.response(model.frame(lag.null7.nolatlong))))
R2.a7 - R2.a7.nolatlong 
AICc_7lag_nolatlong <- AICc(lag.null7.nolatlong) #
AICc_7lag_nolatlong - AICc_7lag

lag.null7.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag7only)
R2.a7.nojul <- 1-var(residuals(lag.null7.nojul))/(var(model.response(model.frame(lag.null7.nojul))))
R2.a7 - R2.a7.nojul 
AICc_7lag_nojul <- AICc(lag.null7.nojul) #
AICc_7lag_nojul - AICc_7lag


lag.null7.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag7only)
R2.a7.nolag <- 1-var(residuals(lag.null7.nolag))/(var(model.response(model.frame(lag.null7.nolag))))
R2.a7 - R2.a7.nolag 
AICc_7lag_nolag <- AICc(lag.null7.nolag) #
AICc_7lag_nolag - AICc_7lag


# age 8

lag.null8 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag8only)
gam.check(lag.null8) #
summary(lag.null8)
plot(lag.null8) #
draw(lag.null8, select=1)
draw(lag.null8, select=2)
draw(lag.null8, select=3)
draw(lag.null8, select=4)

v8 <- visreg(lag.null8, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null8) #

AICc_8lag <- AICc(lag.null8) #
AICc_8lag 

R2.a8 <- 1-var(residuals(lag.null8))/(var(model.response(model.frame(lag.null8))))

lag.null8.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag8only)
R2.a8.nosst <- 1-var(residuals(lag.null8.nosst))/(var(model.response(model.frame(lag.null8.nosst))))
R2.a8 - R2.a8.nosst 
AICc_8lag_nosst <- AICc(lag.null8.nosst) #
AICc_8lag_nosst - AICc_8lag


lag.null8.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag8only)
R2.a8.nolatlong <- 1-var(residuals(lag.null8.nolatlong))/(var(model.response(model.frame(lag.null8.nolatlong))))
R2.a8 - R2.a8.nolatlong 
AICc_8lag_nolatlong <- AICc(lag.null8.nolatlong) #
AICc_8lag_nolatlong - AICc_8lag

lag.null8.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag8only)
R2.a8.nojul <- 1-var(residuals(lag.null8.nojul))/(var(model.response(model.frame(lag.null8.nojul))))
R2.a8 - R2.a8.nojul 
AICc_8lag_nojul <- AICc(lag.null8.nojul) #
AICc_8lag_nojul - AICc_8lag

lag.null8.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag8only)
R2.a8.nolag <- 1-var(residuals(lag.null8.nolag))/(var(model.response(model.frame(lag.null8.nolag))))
R2.a8 - R2.a8.nolag 
AICc_8lag_nolag <- AICc(lag.null8.nolag) #
AICc_8lag_nolag - AICc_8lag


# age 9

lag.null9 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag9only)
gam.check(lag.null9) #
summary(lag.null9)
plot(lag.null9) #
draw(lag.null9, select=1)
draw(lag.null9, select=2)
draw(lag.null9, select=3)
draw(lag.null9, select=4)

v9 <- visreg(lag.null9, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null9) #

AICc_9lag <- AICc(lag.null9) #
AICc_9lag 

R2.a9 <- 1-var(residuals(lag.null9))/(var(model.response(model.frame(lag.null9))))

lag.null9.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag9only)
R2.a9.nosst <- 1-var(residuals(lag.null9.nosst))/(var(model.response(model.frame(lag.null9.nosst))))
R2.a9 - R2.a9.nosst 
AICc_9lag_nosst <- AICc(lag.null9.nosst) #
AICc_9lag_nosst - AICc_9lag

lag.null9.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag9only)
R2.a9.nolatlong <- 1-var(residuals(lag.null9.nolatlong))/(var(model.response(model.frame(lag.null9.nolatlong))))
R2.a9 - R2.a9.nolatlong 
AICc_9lag_nolatlong <- AICc(lag.null9.nolatlong) #
AICc_9lag_nolatlong - AICc_9lag

lag.null9.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag9only)
R2.a9.nojul <- 1-var(residuals(lag.null9.nojul))/(var(model.response(model.frame(lag.null9.nojul))))
R2.a9 - R2.a9.nojul 
AICc_9lag_nojul <- AICc(lag.null9.nojul) #
AICc_9lag_nojul - AICc_9lag

lag.null9.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag9only)
R2.a9.nolag <- 1-var(residuals(lag.null9.nolag))/(var(model.response(model.frame(lag.null9.nolag))))
R2.a9 - R2.a9.nolag 
AICc_9lag_nolag <- AICc(lag.null9.nolag) #
AICc_9lag_nolag - AICc_9lag


# age 10

lag.null10 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag10only)
gam.check(lag.null10) #
summary(lag.null10)
plot(lag.null10) #
draw(lag.null10, select=1)
draw(lag.null10, select=2)
draw(lag.null10, select=3)
draw(lag.null10, select=4)

v10 <- visreg(lag.null10, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null10) #

AICc_10lag <- AICc(lag.null10) #
AICc_10lag 

R2.a10 <- 1-var(residuals(lag.null10))/(var(model.response(model.frame(lag.null10))))

lag.null10.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag10only)
R2.a10.nosst <- 1-var(residuals(lag.null10.nosst))/(var(model.response(model.frame(lag.null10.nosst))))
R2.a10 - R2.a10.nosst 
AICc_10lag_nosst <- AICc(lag.null10.nosst) #
AICc_10lag_nosst - AICc_10lag

lag.null10.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag10only)
R2.a10.nolatlong <- 1-var(residuals(lag.null10.nolatlong))/(var(model.response(model.frame(lag.null10.nolatlong))))
R2.a10 - R2.a10.nolatlong 
AICc_10lag_nolatlong <- AICc(lag.null10.nolatlong) #
AICc_10lag_nolatlong - AICc_10lag

lag.null10.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag10only)
R2.a10.nojul <- 1-var(residuals(lag.null10.nojul))/(var(model.response(model.frame(lag.null10.nojul))))
R2.a10 - R2.a10.nojul 
AICc_10lag_nojul <- AICc(lag.null10.nojul) #
AICc_10lag_nojul - AICc_10lag

lag.null10.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag10only)
R2.a10.nolag <- 1-var(residuals(lag.null10.nolag))/(var(model.response(model.frame(lag.null10.nolag))))
R2.a10 - R2.a10.nolag 
AICc_10lag_nolag <- AICc(lag.null10.nolag) #
AICc_10lag_nolag - AICc_10lag


# age 11

lag.null11 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag11only)
gam.check(lag.null11) #
summary(lag.null11)
plot(lag.null11) #
draw(lag.null11, select=1)
draw(lag.null11, select=2)
draw(lag.null11, select=3)
draw(lag.null11, select=4)

v11 <- visreg(lag.null11, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null11) #

AICc_11lag <- AICc(lag.null11) #
AICc_11lag  

R2.a11 <- 1-var(residuals(lag.null11))/(var(model.response(model.frame(lag.null11))))

lag.null11.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag11only)
R2.a11.nosst <- 1-var(residuals(lag.null11.nosst))/(var(model.response(model.frame(lag.null11.nosst))))
R2.a11 - R2.a11.nosst 
AICc_11lag_nosst  <- AICc(lag.null11.nosst ) #
AICc_11lag_nosst  - AICc_11lag

lag.null11.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag11only)
R2.a11.nolatlong <- 1-var(residuals(lag.null11.nolatlong))/(var(model.response(model.frame(lag.null11.nolatlong))))
R2.a11 - R2.a11.nolatlong 
AICc_11lag_nolatlong  <- AICc(lag.null11.nolatlong ) #
AICc_11lag_nolatlong  - AICc_11lag

lag.null11.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag11only)
R2.a11.nojul <- 1-var(residuals(lag.null11.nojul))/(var(model.response(model.frame(lag.null11.nojul))))
R2.a11 - R2.a11.nojul 
AICc_11lag_nojul <- AICc(lag.null11.nojul) #
AICc_11lag_nojul - AICc_11lag

lag.null11.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag11only)
R2.a11.nolag <- 1-var(residuals(lag.null11.nolag))/(var(model.response(model.frame(lag.null11.nolag))))
R2.a11 - R2.a11.nolag 
AICc_11lag_nolag <- AICc(lag.null11.nolag) #
AICc_11lag_nolag - AICc_11lag



# age 12

lag.null12 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag12only)
gam.check(lag.null12) #
summary(lag.null12)
plot(lag.null12) #
draw(lag.null12, select=1)
draw(lag.null12, select=2)
draw(lag.null12, select=3)
draw(lag.null12, select=4)

v12 <- visreg(lag.null12, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST")

AICc(lag.null12) #

AICc_12lag <- AICc(lag.null12) #
AICc_12lag   # 

R2.a12 <- 1-var(residuals(lag.null12))/(var(model.response(model.frame(lag.null12))))

lag.null12.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag12only)
R2.a12.nosst <- 1-var(residuals(lag.null12.nosst))/(var(model.response(model.frame(lag.null12.nosst))))
R2.a12 - R2.a12.nosst 
AICc_12lag_nosst <- AICc(lag.null12.nosst) #
AICc_12lag_nosst - AICc_12lag

lag.null12.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag12only)
R2.a12.nolatlong <- 1-var(residuals(lag.null12.nolatlong))/(var(model.response(model.frame(lag.null12.nolatlong))))
R2.a12 - R2.a12.nolatlong 
AICc_12lag_nolatlong <- AICc(lag.null12.nolatlong) #
AICc_12lag_nolatlong - AICc_12lag

lag.null12.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(prevage_lastyr_weight_anom, k=4), data=lag12only)
R2.a12.nojul <- 1-var(residuals(lag.null12.nojul))/(var(model.response(model.frame(lag.null12.nojul))))
R2.a12 - R2.a12.nojul 
AICc_12lag_nojul <- AICc(lag.null12.nojul) #
AICc_12lag_nojul - AICc_12lag

lag.null12.nolag <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag12only)
R2.a12.nolag <- 1-var(residuals(lag.null12.nolag))/(var(model.response(model.frame(lag.null12.nolag))))
R2.a12 - R2.a12.nolag 
AICc_12lag_nolag <- AICc(lag.null12.nolag) #
AICc_12lag_nolag - AICc_12lag





# age 13

lag.null13 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4) + s(prevage_lastyr_weight_anom, k=4), data=lag13only)
gam.check(lag.null13) #
summary(lag.null13)
plot(lag.null13) #
draw(lag.null13, select=1)
draw(lag.null13, select=2)
draw(lag.null13, select=3)
draw(lag.null13, select=4)

visreg(lag.null13, "sst.amj", scale="response",ylab="Scaled log (weight-at-age)", xlab="April-June SST")

AICc(lag.null13) #

AICc_13lag <- AICc(lag.null13) #
AICc_13lag # 


#plot each age models======

# vv2 <- visregList(v10, v9, v8, v7, v6, v5, v4, v3, v2,
                # #  v2, v3, v4, v5, v6, v7, v8, v9, v10,
                #   collapse=TRUE,
                #   labels=c("Age 10", "Age 9", "Age 8",
                #            "Age 7", "Age 6", "Age 5",
                #            "Age 4", "Age 3", "Age 2"))
                           
vv2 <- visregList(v8, v9, v10, 
                  v5, v6, v7, 
                  v2, v3, v4,
  collapse=TRUE,
  labels=c("Age 8","Age 9","Age 10", 
           "Age 5", "Age 6", "Age 7",
           "Age 2", "Age 3", "Age 4"))

#par(mfrow=c(4,3), mar = c(4,4,4,4))
plot(vv2,
     ylab="Scaled log (weight-at-age)",
     xlab="April-June SST")

#add age 11
vv2 <- visregList(v8, v9, v10, v11, 
                  v4, v5, v6, v7, 
                  v2, v3,
                  collapse=TRUE,
                  labels=c("Age 8","Age 9","Age 10", "Age 11",
                           "Age 4", "Age 5", "Age 6", "Age 7",
                           "Age 2", "Age 3"))

#par(mfrow=c(4,3), mar = c(4,4,4,4))
plot(vv2,
     ylab="Scaled log (weight-at-age)",
     xlab="April-June SST")


vv3 <- visregList(v1, 
  v2, v3, v4, v5, v6, v7, v8, v9, v10,
  collapse=FALSE
  ))
par(mfrow=c(4,3), mar = c(4,4,4,4))
plot(vv3)


#multi-panel latxlong
library(cowplot)
library(gratia)
library(visreg)
library(mgcViz)

cc1 <- draw(lag.null1, select=2)
cc2 <- draw(lag.null2, select=2)
cc3 <- draw(lag.null3, select=2)
cc4 <- draw(lag.null4, select=2)
cc5 <- draw(lag.null5, select=2)
cc6 <- draw(lag.null6, select=2)
cc7 <- draw(lag.null7, select=2)
cc8 <- draw(lag.null8, select=2)
cc9 <- draw(lag.null9, select=2)
cc10 <- draw(lag.null10, select=2)

plot_grid(cc1, cc2, cc3,
          cc4, cc5, cc6, cc7,
          cc8, cc9, cc10, 
          labels = c('Age 1', 'Age 2', 'Age 3', 'Age 4', 'Age 5', 'Age 6',
                     'Age 7', 'Age 8', 'Age 9', 'Age 10'), label_size = 12)

c1 <- getViz(lag.null1)
pc1 <- plot(sm(c1, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 


c2 <- getViz(lag.null2)
pc2 <- plot(sm(c2, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 


c3 <- getViz(lag.null3)
pc3 <- plot(sm(c3, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 


c4 <- getViz(lag.null4)
pc4 <- plot(sm(c4, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 


c5 <- getViz(lag.null5)
pc5 <- plot(sm(c5, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 



c6 <- getViz(lag.null6)
pc6 <- plot(sm(c6, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 


c7 <- getViz(lag.null7)
pc7 <- plot(sm(c7, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 



c8 <- getViz(lag.null8)
pc8 <- plot(sm(c8, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 



c9 <- getViz(lag.null9)
pc9 <- plot(sm(c9, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 



c10 <- getViz(lag.null10)
pc10 <- plot(sm(c10, 2)) + l_fitRaster() + l_fitContour() + 
  labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 
  # l_fitRaster(pTrans = function(.p) 0.5) + 
  # l_fitContour() + l_points() 

c11 <- getViz(lag.null11)
pc11 <- plot(sm(c11, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 


c12 <- getViz(lag.null12)
pc12 <- plot(sm(c12, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill=NA,color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = TRUE) 




gridPrint(pc1, pc2, pc3, pc4, pc5,
pc6, pc7, pc8, pc9, pc10, pc11, pc12, ncol = 3)

#map for fig 1=====

#all_analysis_dat required, see script 'NEBS_depth_temp_plot.R'
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 63), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE), size=0.1, data=all_analysis_dat[which(all_analysis_dat$region=="SEBS"),]) + theme_bw() + 
  theme( legend.position = c(0.87, 0.85), legend.key = element_blank(),
         legend.background=element_blank()) #+ #geom_path(aes(x_adj,y), data=contour.lines.adj$z20, col="dark blue") +
 # geom_path(aes(x_adj,y), data=contour.lines.adj$z50, col="#9ecae1") + geom_path(aes(x_adj,y), data=contour.lines.adj$z100, col="#3182bd") +
 #  geom_path(aes(x_adj,y), data=contour.lines.adj$z200, col="navy blue") #+ geom_path(aes(x_adj,y), data=contour.lines.adj$z1000, col="dark green") 







#let's look at models a little more 

library(tidymv)
library(visreg)

plot_smooths(lag.by9, series = sst.amj, comparison = era)

visreg(lag.by9, "sst.amj", "era", gg=TRUE, scale='response' )

#some maps======

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 63), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE, LATITUDE, colour=log_sc_weight), data=lagdat[which(lagdat$AGE<11),]) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~era*AGE) 


## fit brms models to attempt to calculate valid credible intervals -----------------------------

library(rstan)
library(brms)
library(bayesplot)
source("./scripts/stan_utils.R")

## setup - formula
# note that we're using t2 tensor products instead of te
brm_formula <- bf(sc.weight ~ s(sst.amj, k=4) + t2(LATITUDE, LONGITUDE) + s(julian, k = 4))


## fit: brms --------------------------------------
brm_1_2 <- brm(brm_formula,
                   data = filter(scale.dat, AGE %in% 1:2),
                   cores = 4, chains = 4, iter = 4000,
                   save_pars = save_pars(all = TRUE),
                   control = list(adapt_delta = 0.99, max_treedepth = 15))
brm_1_2  <- add_criterion(brm_1_2, c("loo", "bayes_R2"), moment_match = TRUE)
saveRDS(brm_1_2, file = "output/brm_1_2.rds")

brm_1_2 <- readRDS("./output/brm_1_2.rds")
check_hmc_diagnostics(brm_1_2$fit)
neff_lowest(brm_1_2$fit)
rhat_highest(brm_1_2$fit)
summary(brm_1_2)
bayes_R2(brm_1_2)
y <- scale.dat$sc.weight
yrep_brm_1_2  <- fitted(brm_1_2, scale = "response", summary = FALSE)
ppc_dens_overlay(y = y, yrep = yrep_brm_1_2[sample(nrow(yrep_brm_1_2), 25), ]) +
  xlim(0, 500) +
  ggtitle("brm_1_2")


# older age classes
brm_9_12 <- brm(brm_formula,
               data = filter(scale.dat, AGE %in% 9:12),
               cores = 4, chains = 4, iter = 3000,
               save_pars = save_pars(all = TRUE),
               control = list(adapt_delta = 0.99, max_treedepth = 12))
brm_9_12  <- add_criterion(brm_9_12, c("loo", "bayes_R2"), moment_match = TRUE)
saveRDS(brm_9_12, file = "output/brm_9_12.rds")

brm_9_12 <- readRDS("./output/brm_9_12.rds")
check_hmc_diagnostics(brm_9_12$fit)
neff_lowest(brm_9_12$fit)
rhat_highest(brm_9_12$fit)
summary(brm_9_12)
bayes_R2(brm_9_12)
plot(conditional_smooths(brm_9_12), ask = FALSE)
y <- scale.dat$sc.weight
yrep_brm_9_12  <- fitted(brm_9_12, scale = "response", summary = FALSE)
ppc_dens_overlay(y = y, yrep = yrep_brm_9_12[sample(nrow(yrep_brm_9_12), 25), ]) +
  xlim(0, 500) +
  ggtitle("brm_9_12")


## plot

## 95% CI
ce1s_1 <- conditional_effects(brm_9_12, effect = "sst.amj", re_formula = NA,
                              probs = c(0.025, 0.975))

dat_ce <- ce1s_1$sst.amj
dat_ce[["rug.anom"]] <- c(unique(scale.dat$sst.amj),
                          rep(NA, 100-length(unique(scale.dat$sst.amj))))
dat_ce$age.class <- "Age 9-12"

sst_9_12 <- ggplot(dat_ce) +
  aes(x = effect1__, y = estimate__) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey90") +
  geom_line(size = 1, color = "red3") +
  labs(x = "April-June SST", y = "Scaled weight") +
  theme_bw()+
  geom_rug(aes(x=rug.anom, y=NULL))
sst_9_12

# now the young fish
ce1s_1 <- conditional_effects(brm_1_2, effect = "sst.amj", re_formula = NA,
                              probs = c(0.025, 0.975))

young_dat_ce <- ce1s_1$sst.amj
young_dat_ce[["rug.anom"]] <- c(unique(scale.dat$sst.amj),
                          rep(NA, 100-length(unique(scale.dat$sst.amj))))
young_dat_ce$age.class <- "Age 1-2"

sst_1_2 <- ggplot(young_dat_ce) +
  aes(x = effect1__, y = estimate__) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey90") +
  geom_line(size = 1, color = "red3") +
  labs(x = "April-June SST", y = "Scaled weight") +
  theme_bw()+
  geom_rug(aes(x=rug.anom, y=NULL))

sst_1_2

both.plot <- rbind(dat_ce, young_dat_ce)

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw())

sst_both <- ggplot(both.plot) +
  aes(x = effect1__, y = estimate__, color = age.class, fill = age.class) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3, lty=0) +
  geom_line() +
  labs(x = "April-June SST", y = "Weight anomaly") +
  geom_rug(aes(x=rug.anom, y=NULL), color = "black") +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = cb[c(2,4)]) +
  scale_fill_manual(values = cb[c(2,4)])

sst_both

ggsave("./figs/ages_1-2_9-12_weight_vs_sst.png", width=6.5, height=4, units='in')

## follow-up hypothesis: are there reverse size anomalies between the EBS and NBS for these size classes?? -------------------
# re-load data to include NBS
dat <- read.csv(("./data/survey data/Litzow_pollock_02032021.csv"))

unique(dat$SURVEY) # EBS and NBS

dat$YEAR <- as.numeric(as.character((chron::years(dat$TOW_DATE))))

dat <- dat %>%
  filter(YEAR >= 1999) 

# get julian day
dat$Date <- chron::dates(as.character(dat$TOW_DATE))

dat$julian <- lubridate::yday(dat$Date)

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

map.plot

ggplot(filter(dat,  SURVEY=="NBS"), aes(AGE)) +
  geom_histogram(fill="grey", color="black", bins=15) +
  facet_wrap(~YEAR, scales="free_y")

# no ages for NBS??

## look at length histograms for these ages in the EBS
# first, age 1-2

ggplot(filter(dat, AGE %in% 1:2), aes(LENGTH)) +
  geom_histogram(color="black", fill="grey")

# compare to age 3
ggplot(filter(dat, AGE == 3), aes(LENGTH)) +
  geom_histogram(color="black", fill="grey")

# lots of overlap - limit 'young' group to <= 250 mm to avoid this

# now the 9-12s
ggplot(filter(dat, AGE %in% 9:12), aes(LENGTH)) +
  geom_histogram(color="black", fill="grey")

# compare with the 8s
ggplot(filter(dat, AGE == 8), aes(LENGTH)) +
  geom_histogram(color="black", fill="grey")

# and the 13-15s
ggplot(filter(dat, AGE %in% 13:15), aes(LENGTH)) +
  geom_histogram(color="black", fill="grey")

# again, plenty of overlap; limit "old" group to 550-750 mm

# we don't have ages, so can't scale by age - fit length:weight regressions instead!
dat <- dat %>%
  mutate(class = if_else(LENGTH %in% 100:250, "young",
                         if_else(LENGTH %in% 550:750, "old", NULL)))

mod1 <- gam(WEIGHT ~ s(LENGTH, k=6) + s(julian, k = 4) + SURVEY, data=filter(dat, class == "young", YEAR %in% c(2017, 2019)))
summary(mod1)

mod2 <- gam(WEIGHT ~ s(LENGTH, k=6, by = SURVEY) + s(julian, k = 4) + SURVEY, data=filter(dat, class == "young", YEAR %in% c(2017, 2019)))
summary(mod2)
plot(mod2, se=F, resid=T, pages=1, rug=F)

AICc(mod1, mod2) # slight support for mod2!
AICc(mod1)-AICc(mod2)

# now the larger age class
mod3 <- gam(WEIGHT ~ s(LENGTH, k=6) + s(julian, k = 4) + SURVEY, data=filter(dat, class == "old", YEAR %in% c(2017, 2019)))
summary(mod3)

mod4 <- gam(WEIGHT ~ s(LENGTH, k=6, by = SURVEY) + s(julian, k = 4) + SURVEY, data=filter(dat, class == "old", YEAR %in% c(2017, 2019)))
summary(mod4)
plot(mod4, se=F, resid=T, pages=1, rug=F)

AICc(mod3, mod4) # again, slight support for mod4!
AICc(mod3)-AICc(mod4)

# plot predicted values for each length comparison
newdat1 <- data.frame(LENGTH = rep(seq(from = 100, to = 250, length.out = 100), 2),
                      julian = mean(dat$julian),
                      SURVEY = rep(c("EBS", "NBS"), each = 100),
                      class = "young")

newdat1$predict <- predict(mod2, newdata = newdat1, type = "response")

newdat2 <- data.frame(LENGTH = rep(seq(from = 550, to = 750, length.out = 100), 2),
                      julian = mean(dat$julian),
                      SURVEY = rep(c("EBS", "NBS"), each = 100),
                      class = "old")

newdat2$predict <- predict(mod4, newdata = newdat2, type = "response")

plot.dat <- rbind(newdat1, newdat2)


ggplot(plot.dat, aes(LENGTH, predict, color = SURVEY)) +
  geom_line() +
  facet_wrap(~class, scales = "free", ncol=1)

# might be instructive to compare the entire length-weight relationship

range <- dat %>%
  filter(YEAR %in% c(2017, 2019)) %>%
  group_by(SURVEY) %>%
  summarise(min=min(LENGTH), max=max(LENGTH))
range
# so 100-750 is the range of overlap!

mod5 <- gam(WEIGHT ~ s(LENGTH, k=6) + s(julian, k = 4) + SURVEY, 
            data=filter(dat, LENGTH %in% 100:750, YEAR %in% c(2017, 2019)))


mod6 <- gam(WEIGHT ~ s(LENGTH, k=6, by = SURVEY) + s(julian, k = 4) + SURVEY, 
            data=filter(dat, LENGTH %in% 100:750, YEAR %in% c(2017, 2019)))
summary(mod6)
plot(mod6, se=T, resid=T, pages=1, rug=F)

AICc(mod5) - AICc(mod6)


newdat6 <- data.frame(LENGTH = rep(seq(from = 100, to = 750, length.out = 100), 2),
                      julian = mean(dat$julian),
                      SURVEY = rep(c("EBS", "NBS"), each = 100))

newdat6$predict <- predict(mod6, newdata = newdat6, type = "response")


ggplot(newdat6, aes(LENGTH, predict, color = SURVEY)) +
  geom_line() 

# differences look slight
# try fitting a brm model to estimate CIs for a more rigorous comparison

brm_formula <- bf(WEIGHT ~ s(LENGTH, k=6, by = SURVEY) + s(julian, k = 4))


## fit: brms --------------------------------------
brm_area <- brm(brm_formula,
               data = filter(dat, LENGTH %in% 100:750, YEAR %in% c(2017, 2019)),
               cores = 4, chains = 4, iter = 3000,
               save_pars = save_pars(all = TRUE),
               control = list(adapt_delta = 0.99, max_treedepth = 12))
brm_area  <- add_criterion(brm_area, c("loo", "bayes_R2"), moment_match = TRUE)
saveRDS(brm_area, file = "output/brm_area.rds")

brm_area <- readRDS("./output/brm_area.rds")
check_hmc_diagnostics(brm_area$fit)
neff_lowest(brm_area$fit)
rhat_highest(brm_area$fit)
summary(brm_area)
bayes_R2(brm_area)
plot(conditional_smooths(brm_area), ask = FALSE)
y <- dat$WEIGHT
yrep_brm_area  <- fitted(brm_area, scale = "response", summary = FALSE)
ppc_dens_overlay(y = y, yrep = yrep_brm_area[sample(nrow(yrep_brm_area), 25), ]) +
  xlim(0, 500) +
  ggtitle("brm_area")

# plot predicted weight-length regressions by area...

## 95% CI
ce1s_1 <- conditional_effects(brm_area, effect = "LENGTH:SURVEY", re_formula = NA,
                              probs = c(0.025, 0.975))

ce1s_1

ggsave("./figs/ebs_nbs_length_weight.png", width=6, height=4, units='in')

diff <- ce1s_1$LENGTH %>%
  select(effect1__, effect2__, estimate__) %>%
  pivot_wider(values_from = estimate__, names_from = effect2__) %>%
  mutate(percent_diff = 100*(NBS-EBS)/EBS)

ggplot(diff, aes(effect1__, percent_diff)) +
  geom_line() +
  labs(x = "Length", y = "Percent difference (NBS vs. EBS)")

ggsave("./figs/ebs_nbs_percent_diff_length_weight.png", width=6, height=4, units='in')


#sample size table----------------------------------------------

tab1 <- table(lag1$YEAR)
tab2 <- table(lag2$YEAR)
tab3 <- table(lag3only$YEAR)
tab4 <- table(lag4only$YEAR)
tab5 <- table(lag5only$YEAR)
tab6 <- table(lag6only$YEAR)
tab7 <- table(lag7only$YEAR)
tab8 <- table(lag8only$YEAR)
tab9 <- table(lag9only$YEAR)
tab10 <- table(lag10only$YEAR)
tab11 <- table(lag11only$YEAR)

all_age_table <- cbind(tab1, tab2, tab3, tab4, tab5,
                       tab6, tab7, tab8, tab9, tab10)

table(lag1$STATIONID)


