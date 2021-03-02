
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

amj.ts

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

#==
dat1_2 <- filter(logscale.dat, AGE %in% 1:2)

# age 1-2
log.null1 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat1_2)
gam.check(log.null1)

log.alt1 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat1_2)
gam.check(log.alt1)

log.by1 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat1_2)
gam.check(log.by1) #

summary(log.alt1)

AICc_1.2 <- AICc(log.null1, log.alt1) 
AICc_1.2 # null model is better
AICc(log.null1, log.alt1, log.by1) #by lower

#==
dat5_8 <- filter(logscale.dat, AGE %in% 5:8)

# age 5-8
log.null5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat5_8)
gam.check(log.null5) #not great

log.alt5 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat5_8)
gam.check(log.alt5)

log.by5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat5_8)
gam.check(log.by5)

summary(log.alt5)

AICc_5.8 <- AICc(log.null5, log.alt5) 
AICc_5.8 # null model is better
AICc(log.null5, log.alt5, log.by5) #by better

#==
dat9_12 <- filter(logscale.dat, AGE %in% 9:12)

# age 9-12
log.null9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat9_12)
gam.check(log.null9)

log.alt9 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat9_12)
gam.check(log.alt9)

log.by9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4), data=dat9_12)
gam.check(log.by9) #bad hessian

summary(log.alt9)

AICc_9.12 <- AICc(log.null9, log.alt9) 
AICc_9.12 # alt model is better; but nominal p-values are pretty high consider the # of obs in the model!
AICc(log.null9, log.alt9, log.by9) #by much lower AIC

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

bothmeans$weight_at_age_anom <- NA
bothmeans$weight_at_age_anom <- bothmeans$mean_annual_weight_at_age - bothmeans$mean_overall_weight_at_age 

bothmeans$lag_year <- NA
bothmeans$lag_year <- bothmeans$YEAR + 1

#wmeans$temp_1yr_lag <- wmeans$mean_annual_sstamj

mergemeans <- bothmeans[,c(2,5:6)]

lagdat <- left_join(logscale.dat, mergemeans, by = c("YEAR" = "lag_year", "AGE"="AGE"))

lag12 <- lagdat[which(lagdat$AGE<3),]
lag58 <- lagdat[which(lagdat$AGE<9 & lagdat$AGE>4),]
lag912 <- lagdat[which(lagdat$AGE<13 & lagdat$AGE>8),]


#models==


# age 1-2
lag.null1 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag12)
gam.check(lag.null1) #bad hessian

lag.alt1 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag12)
gam.check(lag.alt1)

lag.by1 <- gam(log_sc_weight ~ as.factor(AGE) +  s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag12)
gam.check(lag.by1) #bad hessian

summary(lag.alt1)
AICc(lag.null1, lag.alt1, log.null1, log.alt1) #yes improves model

AICc_1.2lag <- AICc(lag.null1, lag.alt1) #null still better
AICc_1.2lag # 

AICc(lag.null1, lag.alt1, log.null1, log.alt1, lag.by1) #by still better

# age 5-8
lag.null5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag58)
gam.check(lag.null5)

lag.alt5 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag58)
gam.check(lag.alt5)

lag.by5 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag58)
gam.check(lag.by5) #bad hessian

summary(lag.alt5)
AICc(lag.null5, lag.alt5, log.null5, log.alt5) #better with lagged weight anoms

AICc_5.8lag <- AICc(lag.null5, lag.alt5) 
AICc_5.8lag # null better

AICc(lag.null5, lag.alt5, log.null5, log.alt5, lag.by5) #by better in this case too

# age 9-12
lag.null9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag912)
gam.check(lag.null9) #bad hessian

lag.alt9 <- gam(log_sc_weight ~ as.factor(AGE) + sst.amj*era + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag912)
gam.check(lag.alt9) #bad hessian

lag.by9 <- gam(log_sc_weight ~ as.factor(AGE) + s(sst.amj, by=era, k=6) + te(LATITUDE, LONGITUDE) + s(julian, k = 4) + s(weight_at_age_anom), data=lag912)
gam.check(lag.by9) #bad hessian

summary(lag.alt9)
AICc(lag.null9, lag.alt9, log.null9, log.alt9) #better w lagged weight anom

AICc_9.12lag <- AICc(lag.null9, lag.alt9) 
AICc_9.12lag # null better

AICc(lag.null9, lag.alt9, log.null9, log.alt9, lag.by9) #by lowest



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
