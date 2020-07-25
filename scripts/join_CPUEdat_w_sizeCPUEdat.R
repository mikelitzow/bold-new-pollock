#=======================================================================================================================================
# Join new size CPUE data to CPUE data
#
# Created by Krista, June 2020
#=======================================================================================================================================
# Notes:
#=======================================================================================================================================

library(ggplot2)
library(tidyverse)
library(corrplot)
library(mgcv)
library(nlme)
library(mgcViz)
library(tidyverse)
library(mapproj)
library(visreg)



sex.dat <- read.csv("data/survey data/poll_cpue_by_sex_cm.csv", stringsAsFactors = TRUE)

sex.dat$YEAR_factor <- as.factor(sex.dat$YEAR)
sex.dat$SEX <- as.factor(sex.dat$SEX)
sex.dat$LENGTH[which(sex.dat$LENGTH=="-9")]<-NA
sex.dat$SEX[which(sex.dat$SEX=="-9")]<-NA

indiv_pollock <- read.csv("data/survey data/poll_specimen_haul.csv", stringsAsFactors = TRUE)

indiv_pollock$SEX <- as.factor(indiv_pollock$SEX)
indiv_pollock$AGE <- as.factor(indiv_pollock$AGE)

indiv_pollock <- indiv_pollock %>%
  separate(CRUISE, 
           c("Year", "cruisenum"), 
           sep=cumsum(c(4,2)))
indiv_pollock$Year <- as.factor(indiv_pollock$Year)

#sel.trawl.dat is loaded in trawl_biomass_GAM_explor.R

head(sel.trawl.dat)
head(sex.dat)

trawljoin <- left_join(sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(1:6,9:18,20:21)], 
                      sex.dat)

#seems to catch all sex.dat rows except ~20K rows w missing STRATUM values
fuzziertestjoin <- left_join(sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(1:6,9:18,20:21)], 
                                 sex.dat, by = c("LATITUDE", "LONGITUDE", "YEAR", "VESSEL", "HAUL"))
length(fuzziertestjoin$YEAR) #no new rows
#are haul codes unique within a year???

View(sex.dat[is.na(sex.dat$STRATUM),])

#for now going to proceed - the rows that are not matching actually don't seem to have a good match
#in the trawl data, maybe removed before the dataset was made?

#test join to see if rows really don't match
secondtest <- left_join(sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(5,15:17)], 
                        sex.dat, by = c( "YEAR", "VESSEL", "HAUL"))
length(secondtest$YEAR)
length(sex.dat$YEAR)

antitest <- anti_join( sex.dat, sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(5,15:17)], 
                     by = c( "YEAR", "VESSEL", "HAUL"))

antitestfull <- anti_join( sex.dat, sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),], 
                        by = c( "YEAR", "VESSEL", "HAUL"))

antistratum <- anti_join( sex.dat, sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(5,15:17)], 
                       by = c( "YEAR", "STRATUM", "VESSEL", "HAUL"))

antistratum <- anti_join( sex.dat, sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(1:6,9:18,20:21)],
                          by = c( "YEAR", "STRATUM", "VESSEL", "HAUL")   )


#bin into size classes========================================================================================

hist(testjoin$LENGTH)

#binning following Thorson et al 2017 Fish Fisheries
#0-20cm = age 1
#21-30 = age 2
#31-40, 41-50, 50+

trawljoin$bin <- NA
trawljoin$bin[which(trawljoin$LENGTH<=200)]<- "0-200"
trawljoin$bin[which(trawljoin$LENGTH>200 & trawljoin$LENGTH<=300)]<- "200-300"
trawljoin$bin[which(trawljoin$LENGTH>300 & trawljoin$LENGTH<=400)]<- "300-400"
trawljoin$bin[which(trawljoin$LENGTH>400 & trawljoin$LENGTH<=500)]<- "400-500"
trawljoin$bin[which(trawljoin$LENGTH>500)]<- "500+"


binneddat <- trawljoin %>% group_by(YEAR, STATION, STRATUM, SCIENTIFIC, VESSEL, CRUISE, HAUL, bin) %>%
  summarize(bin_sum_WGTCPUE_LEN=sum(WGTCPUE_LENGTH, na.rm=TRUE), bin_sum_NUMCPUE_LEN=sum(NUMCPUE_LENGTH, na.rm=TRUE),
            n=n())
