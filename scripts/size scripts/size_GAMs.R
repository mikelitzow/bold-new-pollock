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


#hmm need to double check this calc

#match size data to bottom temp anom and mean stat temp etc


#GAMs===============================================================================

lin_cond1 <- gamm(cond_fact ~ bottemp_anom*period +
                       te(mean_station_bottemp, BOT_DEPTH), random=list(YEAR_factor=~1), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=dat2, method="REML")
gam.check(lin_cond1[[2]]) 
summary(lin_cond1[[1]]) #  
summary(lin_cond1[[2]]) #rsq 0.






