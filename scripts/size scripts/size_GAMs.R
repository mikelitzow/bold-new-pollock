#========================================================================================================
# Size models
#
# By Krista, Jan 2021
#========================================================================================================
#Notes:
#========================================================================================================

library(gratia)
library(tidyverse)
library(mgcv)

#========================================================================================================
#read in data
dat2 <- read.csv(("./data/survey data/pollock_survey_specimen_data_confirmation.csv"))


dat2$CRUISE2 <- dat2$CRUISE
dat2 <- dat2 %>% separate(CRUISE2, c("YEAR", "Cruisenum"), sep=4)
dat2$YEAR <- as.numeric(dat2$YEAR)


ggplot(dat2, aes(YEAR, LENGTH)) + 
  geom_point()+  geom_smooth() + facet_wrap(~as.factor(AGE), scales = "free")

#means
yrly_means2 <- dat2 %>% group_by(YEAR, AGE) %>% summarise(mean_annual_size_global=mean(LENGTH, na.rm=TRUE), n=n())

ggplot(dat2, aes(LENGTH, WEIGHT, col=as.factor(AGE))) + geom_point() + facet_wrap(~YEAR)


dat2$cond_fact <- (100000*dat2$WEIGHT)/(dat2$LENGTH^3)

ggplot(dat2, aes(AGE, cond_fact, col=as.factor(AGE))) + geom_point() + facet_wrap(~YEAR)

ggplot(dat2[which(dat2$YEAR>1998),], aes(AGE, cond_fact, col=as.factor(AGE))) + geom_point() + facet_wrap(~YEAR)

ggplot(dat2[which(dat2$YEAR>1998),], aes(YEAR, cond_fact, col=as.factor(AGE))) + geom_point() + facet_wrap(~AGE, scales="free")

ggplot(dat2[which(dat2$YEAR>1998),], aes(YEAR, cond_fact, col=as.factor(AGE))) + geom_point() + 
  facet_wrap(~AGE, scales="free") + geom_smooth()

ggplot(dat2[which(dat2$YEAR>1998),], aes(YEAR, cond_fact)) + geom_point() + 
  geom_smooth()


#hmm need to double check this calc

#match size data to bottom temp anom and mean stat temp etc
table(dat2$YEAR, dat2$AGE)

table(dat2$AGE)

#use data previously cleaned 
#periods_analysis_dat is also loaded in trawl_biomass_GAM_explor.R
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)

length(dat2$SURVEY)

joindat <- left_join(dat2, periods_analysis_dat[,c(1:15,49:52)])

length(joindat$SURVEY)#same length, nice

#should either limit to age 10 or do age 10+. Stock assessment suggests most catch is under age 10

#GAMs===============================================================================

#exclude NBS stations

joindat$shelf <- NA
joindat$shelf[which(joindat$STRATUM==81 | 
                      joindat$STRATUM==70 |
                      joindat$STRATUM==71)] <- "NEBS"
joindat$shelf[which(joindat$STRATUM==10 | 
                      joindat$STRATUM==20)] <- "EBS_inner"
joindat$shelf[which(joindat$STRATUM==31 | 
                      joindat$STRATUM==32 | 
                      joindat$STRATUM==41 | 
                      joindat$STRATUM==42 | 
                      joindat$STRATUM==43 | 
                      joindat$STRATUM==82)] <- "EBS_middle"
joindat$shelf[which(joindat$STRATUM==50 | 
                      joindat$STRATUM==61 | 
                      joindat$STRATUM==62 | 
                      joindat$STRATUM==90)] <- "EBS_outer"

cond_analysis_dat <- joindat[which(joindat$YEAR>1981 & is.na(joindat$AGE)==FALSE),]
#about 10K with AGE NA removed
#seems like a whole lot of NA age are recent

ggplot(cond_analysis_dat[which(cond_analysis_dat$AGE<11),], aes(as.factor(AGE), cond_fact, col=period)) + geom_boxplot()

ggplot(cond_analysis_dat[which(cond_analysis_dat$AGE<11),], aes(as.factor(AGE), cond_fact, col=period)) + geom_boxplot() +
  facet_wrap(~STRATUM)

ggplot(cond_analysis_dat[which(cond_analysis_dat$AGE<11),], aes(YEAR, cond_fact, col=as.factor(AGE))) + geom_point() +
  facet_wrap(~STRATUM, scales="free") + geom_smooth()


ggplot(cond_analysis_dat[which(cond_analysis_dat$AGE<11& 
                                 temp_cond_dat$AGE>0 ),], aes(as.factor(STRATUM), cond_fact, col=as.factor(AGE))) + 
  geom_boxplot() + facet_wrap(~as.factor(AGE))

ggplot(cond_analysis_dat[which(cond_analysis_dat$AGE<11& 
                                 temp_cond_dat$AGE>0 ),], aes(LATITUDE, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth() + facet_wrap(~as.factor(AGE), scales="free")

ggplot(cond_analysis_dat[which(cond_analysis_dat$AGE<11& 
                                 temp_cond_dat$AGE>0 ),], aes(LONGITUDE, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth() + facet_wrap(~as.factor(AGE), scales="free")

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(south.sst.amj, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~as.factor(AGE), scales="free")

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(summer.bottom.temp, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~as.factor(AGE), scales="free")

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(YEAR, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~as.factor(AGE), scales="free")

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(YEAR, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="gam") + facet_wrap(~as.factor(AGE), scales="free")

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11 & 
                             temp_cond_dat$AGE>0 ),], aes(juliandate, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="gam", col="black") + facet_wrap(~as.factor(AGE), scales="free")


age1dat <- cond_analysis_dat[which(cond_analysis_dat$AGE==1),]

table(age1dat$YEAR, age1dat$HAUL)


#following Mike's models

mmod1 <- gam(cond_fact ~ s(YEAR) + s(bottemp_anom) + te(LATITUDE, LONGITUDE), data=cond_analysis_dat[which(cond_analysis_dat$AGE==1),])
summary(mmod1)
plot(mmod1)

mmod10 <- gam(cond_fact ~ s(YEAR) + s(bottemp_anom) + te(LATITUDE, LONGITUDE), data=cond_analysis_dat[which(cond_analysis_dat$AGE==10),])
summary(mmod10)
plot(mmod10)

#use annual climate data instead
clim.dat <- read.csv("data/climate data.csv")

temp_cond_dat <- left_join(cond_analysis_dat, clim.dat[,c(1:5,12)], by=c("YEAR"="year"))
length(cond_analysis_dat$YEAR)
length(temp_cond_dat$YEAR)

temp_cond_dat$DATETIME <- as.character(temp_cond_dat$DATETIME)
temp_cond_dat$DATE <- strsplit(temp_cond_dat$DATETIME, " ")
temp_cond_dat <- temp_cond_dat %>% separate(DATETIME, 
                                            into = c("DATE", "TIME"), sep = " ")

# get julian day

temp_cond_dat$DATE <- parse_date_time2(temp_cond_dat$DATE, "mdy", cutoff_2000 = 50)
temp_cond_dat$juliandate <- format(temp_cond_dat$DATE, "%j")
temp_cond_dat$juliandate <- as.numeric(temp_cond_dat$juliandate) 

#

bmod1 <- gam(cond_fact ~ s(YEAR) + s(summer.bottom.temp) + te(LATITUDE, LONGITUDE), data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
summary(bmod1)
plot(bmod1)

draw(bmod1, select = 3)

smod1 <- gam(cond_fact ~ s(YEAR) + s(south.sst.amj) + te(LATITUDE, LONGITUDE), data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
summary(smod1)
plot(smod1)

draw(smod1, select = 3)


jmod1 <- gam(cond_fact ~ s(YEAR) + s(south.sst.amj) + s(juliandate) + te(LATITUDE, LONGITUDE), data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
summary(jmod1)
plot(jmod1)

draw(jmod1, select = 1)
draw(jmod1, select = 2)
draw(jmod1, select = 3)
draw(jmod1, select = 4)


bmod10 <- gam(cond_fact ~ s(YEAR) + s(summer.bottom.temp) + te(LATITUDE, LONGITUDE), data=temp_cond_dat[which(temp_cond_dat$AGE==10),])
summary(bmod10)
plot(bmod10)

smod10 <- gam(cond_fact ~ s(YEAR) + s(south.sst.amj) + te(LATITUDE, LONGITUDE), data=temp_cond_dat[which(temp_cond_dat$AGE==10),])
summary(smod10)
plot(smod10)


world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=cond_fact), data=temp_cond_dat) +   
  scale_colour_distiller(palette = "Spectral")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
 # geom_hex(aes(LONGITUDE, LATITUDE, fill=cond_fact), data=temp_cond_dat) +   
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=cond_fact), bins = 30, fun = mean, data=temp_cond_dat) +   
  scale_fill_distiller(palette = "Spectral")
 # scale_colour_distiller(palette = "Spectral")


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
   stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=cond_fact), bins = 20, fun = mean, data=temp_cond_dat[which(temp_cond_dat$YEAR>2003),]) +   
  scale_fill_distiller(palette = "Spectral") + facet_wrap(~YEAR)


