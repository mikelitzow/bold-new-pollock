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
  geom_point() + geom_smooth(method="lm", col="black") + facet_wrap(~as.factor(AGE), scales="free", nrow=2)

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(south.sst.amj, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="gam", col="black") + facet_wrap(~as.factor(AGE), scales="free", nrow=2)


ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(summer.bottom.temp, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~as.factor(AGE), scales="free")

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(YEAR, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="lm", col="black") + facet_wrap(~as.factor(AGE), scales="free", nrow=2)

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11& 
                             temp_cond_dat$AGE>0 ),], aes(YEAR, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="gam", col="black") + facet_wrap(~as.factor(AGE), scales="free", nrow=2)

ggplot(temp_cond_dat[which(temp_cond_dat$AGE<11 & 
                             temp_cond_dat$AGE>0 ),], aes(juliandate, cond_fact, col=as.factor(AGE))) + 
  geom_point() + geom_smooth(method="gam", col="black") + facet_wrap(~as.factor(AGE), scales="free", nrow=2)


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

#limit to post 99
temp_cond_dat <- temp_cond_dat[which(temp_cond_dat$YEAR>1998),]

hist(temp_cond_dat$juliandate)

ggplot(temp_cond_dat, aes(juliandate)) + geom_histogram() + facet_wrap(~YEAR)

#

bmod1 <- gam(cond_fact ~ s(summer.bottom.temp) + te(LATITUDE, LONGITUDE), data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
summary(bmod1)
plot(bmod1)
gam.check(bmod1)

draw(bmod1, select = 2)

smod1 <- gam(cond_fact ~ s(south.sst.amj) + te(LATITUDE, LONGITUDE), data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
summary(smod1)
plot(smod1)
gam.check(smod1)

draw(smod1, select = 2)

ones <- temp_cond_dat[which(temp_cond_dat$AGE==1),]
ones.nona <- ones[which( is.na(ones$south.sst.amj)==FALSE &
                           is.na(ones$cond_fact)==FALSE  &
                           is.na(ones$LATITUDE)==FALSE &
                           is.na(ones$LONGITUDE)==FALSE &
                           is.na(ones$juliandate)==FALSE ),]

#limit to days with 20 or more samples
ones.nona <- ones.nona[which(ones.nona$juliandate>154 & 
                               ones.nona$juliandate<212 ),]

ggplot(ones.nona, aes(south.sst.amj, cond_fact)) + geom_point() + geom_smooth()

ggplot(ones.nona, aes(juliandate, cond_fact)) + geom_point() + geom_smooth()

ggplot(ones.nona, aes(LATITUDE, cond_fact)) + geom_point() + geom_smooth()

ggplot(ones.nona, aes(LONGITUDE, cond_fact)) + geom_point() + geom_smooth()

#seems some unusually high condition factors are leaving to heavy tail maybe?

jmod1 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=ones.nona)
summary(jmod1)
plot(jmod1)
gam.check(jmod1) #qqplot does not look good
#limiting k for sst deals with bad hessian

draw(jmod1, select = 1)
draw(jmod1, select = 2)
draw(jmod1, select = 3)

#autocor?
E <- residuals(jmod1, type="deviance")
I1 <- !is.na(ones.nona$cond_fact)
Efull <- vector(length=length(ones.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #not bad, some in year i - 1
plot(ones.nona$YEAR, Efull) #look pretty good

jmod_lintemp <- gam(cond_fact ~  south.sst.amj + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=ones.nona)
summary(jmod_lintemp)
gam.check(jmod_lintemp)
plot_model(jmod_lintemp, type="pred")

AIC(jmod1, jmod_lintemp)

#so looks good on temp autocor for age one BUT what about older ages?


#2======

twos <- temp_cond_dat[which(temp_cond_dat$AGE==2),]
twos.nona <- twos[which( is.na(twos$south.sst.amj)==FALSE &
                           is.na(twos$cond_fact)==FALSE  &
                           is.na(twos$LATITUDE)==FALSE &
                           is.na(twos$LONGITUDE)==FALSE &
                           is.na(twos$juliandate)==FALSE ),]

table(twos$juliandate)

jmod2 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=twos.nona)
summary(jmod2)
plot(jmod2)
gam.check(jmod2) #

draw(jmod2, select = 1)
draw(jmod2, select = 2)
draw(jmod2, select = 3)

#autocor?
E <- residuals(jmod2, type="deviance")
I1 <- !is.na(twos.nona$cond_fact)
Efull <- vector(length=length(twos.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #YES
plot(twos.nona$YEAR, Efull) #look pretty good


#3=====



threes <- temp_cond_dat[which(temp_cond_dat$AGE==3),]
threes.nona <- threes[which( is.na(threes$south.sst.amj)==FALSE &
                           is.na(threes$cond_fact)==FALSE  &
                           is.na(threes$LATITUDE)==FALSE &
                           is.na(threes$LONGITUDE)==FALSE &
                           is.na(threes$juliandate)==FALSE ),]

table(threes$juliandate)

jmod3 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=threes.nona)
summary(jmod3)
plot(jmod3)
gam.check(jmod3) #heavy tail

draw(jmod3, select = 1)
draw(jmod3, select = 2)
draw(jmod3, select = 3)

#autocor?
E <- residuals(jmod3, type="deviance")
I1 <- !is.na(threes.nona$cond_fact)
Efull <- vector(length=length(threes.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #maybe
plot(threes.nona$YEAR, Efull) #look pretty good




#4=====



fours <- temp_cond_dat[which(temp_cond_dat$AGE==4),]
fours.nona <- fours[which( is.na(fours$south.sst.amj)==FALSE &
                               is.na(fours$cond_fact)==FALSE  &
                               is.na(fours$LATITUDE)==FALSE &
                               is.na(fours$LONGITUDE)==FALSE &
                               is.na(fours$juliandate)==FALSE ),]

table(fours$juliandate)

jmod4 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=fours.nona)
summary(jmod4)
plot(jmod4)
gam.check(jmod4) #BAD HESSIAN

draw(jmod4, select = 1)
draw(jmod4, select = 2)
draw(jmod4, select = 3)

#autocor?
E <- residuals(jmod4, type="deviance")
I1 <- !is.na(fours.nona$cond_fact)
Efull <- vector(length=length(fours.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #yes
plot(fours.nona$YEAR, Efull) #



#5=====



fives <- temp_cond_dat[which(temp_cond_dat$AGE==5),]
fives.nona <- fives[which( is.na(fives$south.sst.amj)==FALSE &
                             is.na(fives$cond_fact)==FALSE  &
                             is.na(fives$LATITUDE)==FALSE &
                             is.na(fives$LONGITUDE)==FALSE &
                             is.na(fives$juliandate)==FALSE ),]

table(fives$juliandate)

jmod5 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=fives.nona)
summary(jmod5)
plot(jmod5)
gam.check(jmod5) #res v fit looks a little odd, so does qq

draw(jmod5, select = 1)
draw(jmod5, select = 2)
draw(jmod5, select = 3)

#autocor?
E <- residuals(jmod5, type="deviance")
I1 <- !is.na(fives.nona$cond_fact)
Efull <- vector(length=length(fives.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #maybe
plot(fives.nona$YEAR, Efull) # some very low residuals in 2013



#6=====



sixes <- temp_cond_dat[which(temp_cond_dat$AGE==6),]
sixes.nona <- sixes[which( is.na(sixes$south.sst.amj)==FALSE &
                             is.na(sixes$cond_fact)==FALSE  &
                             is.na(sixes$LATITUDE)==FALSE &
                             is.na(sixes$LONGITUDE)==FALSE &
                             is.na(sixes$juliandate)==FALSE ),]

table(sixes$juliandate)

jmod6 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=sixes.nona)
summary(jmod6)
plot(jmod6)
gam.check(jmod6) #

draw(jmod6, select = 1)
draw(jmod6, select = 2)
draw(jmod6, select = 3)

#autocor?
E <- residuals(jmod6, type="deviance")
I1 <- !is.na(sixes.nona$cond_fact)
Efull <- vector(length=length(sixes.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #seems ok
plot(sixes.nona$YEAR, Efull) # 




#7=====



sevens <- temp_cond_dat[which(temp_cond_dat$AGE==7),]
sevens.nona <- sevens[which( is.na(sevens$south.sst.amj)==FALSE &
                             is.na(sevens$cond_fact)==FALSE  &
                             is.na(sevens$LATITUDE)==FALSE &
                             is.na(sevens$LONGITUDE)==FALSE &
                             is.na(sevens$juliandate)==FALSE ),]

table(sevens$juliandate)

jmod7 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=sevens.nona)
summary(jmod7)
plot(jmod7)
gam.check(jmod7) #not bad actually

draw(jmod7, select = 1)
draw(jmod7, select = 2)
draw(jmod7, select = 3)

#autocor?
E <- residuals(jmod7, type="deviance")
I1 <- !is.na(sevens.nona$cond_fact)
Efull <- vector(length=length(sevens.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #seems ok
plot(sevens.nona$YEAR, Efull) # 




#8=====



eights <- temp_cond_dat[which(temp_cond_dat$AGE==8),]
eights.nona <- eights[which( is.na(eights$south.sst.amj)==FALSE &
                               is.na(eights$cond_fact)==FALSE  &
                               is.na(eights$LATITUDE)==FALSE &
                               is.na(eights$LONGITUDE)==FALSE &
                               is.na(eights$juliandate)==FALSE ),]

table(eights$juliandate)

jmod8 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=eights.nona)
summary(jmod8)
plot(jmod8)
gam.check(jmod8) #qq not great, one v low residual

draw(jmod8, select = 1)
draw(jmod8, select = 2)
draw(jmod8, select = 3)

#autocor?
E <- residuals(jmod8, type="deviance")
I1 <- !is.na(eights.nona$cond_fact)
Efull <- vector(length=length(eights.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #seems ok
plot(eights.nona$YEAR, Efull) # 



#9=====



nines <- temp_cond_dat[which(temp_cond_dat$AGE==9),]
nines.nona <- nines[which( is.na(nines$south.sst.amj)==FALSE &
                               is.na(nines$cond_fact)==FALSE  &
                               is.na(nines$LATITUDE)==FALSE &
                               is.na(nines$LONGITUDE)==FALSE &
                               is.na(nines$juliandate)==FALSE ),]

table(nines$juliandate)

jmod9 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=nines.nona)
summary(jmod9)
plot(jmod9)
gam.check(jmod9) #heavy tail, qq has big outlier

draw(jmod9, select = 1)
draw(jmod9, select = 2)
draw(jmod9, select = 3)

#autocor?
E <- residuals(jmod9, type="deviance")
I1 <- !is.na(nines.nona$cond_fact)
Efull <- vector(length=length(nines.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #seems ok
plot(nines.nona$YEAR, Efull) # 




#10=====



tens <- temp_cond_dat[which(temp_cond_dat$AGE==10),]
tens.nona <- tens[which( is.na(tens$south.sst.amj)==FALSE &
                             is.na(tens$cond_fact)==FALSE  &
                             is.na(tens$LATITUDE)==FALSE &
                             is.na(tens$LONGITUDE)==FALSE &
                             is.na(tens$juliandate)==FALSE ),]

table(tens$juliandate)

jmod10 <- gam(cond_fact ~  s(south.sst.amj, k=4) + s(juliandate) + te(LATITUDE, LONGITUDE), #data=temp_cond_dat[which(temp_cond_dat$AGE==1),])
             data=tens.nona)
summary(jmod10)
plot(jmod10)
gam.check(jmod10) #qq not super

draw(jmod10, select = 1)
draw(jmod10, select = 2)
draw(jmod10, select = 3)

#autocor?
E <- residuals(jmod10, type="deviance")
I1 <- !is.na(tens.nona$cond_fact)
Efull <- vector(length=length(tens.nona$cond_fact))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action=na.pass) #seems ok
plot(tens.nona$YEAR, Efull) # 





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


