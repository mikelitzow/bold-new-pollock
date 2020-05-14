#============================================================================================================================================
# GAM EXPLORATION - survey abundance data

#Created by Krista, May 2020
#============================================================================================================================================
#Notes:
#============================================================================================================================================
library(ggplot2)
library(tidyverse)
library(corrplot)
library(mgcv)



sel.trawl.dat <- read.csv("data/select_trawl_dat.csv", row.names = 1)

sel.trawl.dat$YEAR_factor <- as.factor(sel.trawl.dat$YEAR)

#going to need to get into a wider database I think to get other sps in



#select data======================================================

#we only want data pre2014
early_dat <- sel.trawl.dat[which(sel.trawl.dat$YEAR<2014),]
early_dat <- early_dat[,-c(9,11)] #drop columns that repeat info

#widen dataframe to have columns for each sps

# early_wide <- early_dat %>% pivot_wider(names_from=c(LATITUDE, LONGITUDE, STATION, STRATUM, YEAR, 
#                                                      DATETIME, VESSEL, CRUISE, HAUL), 
#                                         values_from=c(WTCPUE, NUMCPUE, logCPUE))

early_wide_test <- early_dat %>% pivot_wider(names_from=SCIENTIFIC, 
                                        values_from=c(WTCPUE, NUMCPUE, logCPUE))

#try w just pollock first

#proof of concept gams=====
#confirm whether these should be te() or t2() or s()
yr_int <- gam(logCPUE ~ YEAR_factor + s(LATITUDE, LONGITUDE, YEAR), 
                      data=early_wide_test[which(early_wide_test$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_int)
plot(yr_int)
vis.gam(yr_int, view=c("LATITUDE","LONGITUDE"))
#look at predict.gam

yr_botT_int <- gam(logCPUE ~ YEAR_factor + BOT_TEMP + s(LATITUDE, LONGITUDE, YEAR), 
              data=early_wide_test[which(early_wide_test$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_botT_int)
plot(yr_botT_int)
vis.gam(yr_botT_int, view=c("LATITUDE","LONGITUDE"))

yr_botTD_int <- gam(logCPUE ~ YEAR_factor + BOT_TEMP + BOT_DEPTH + s(LATITUDE, LONGITUDE, YEAR), 
                   data=early_wide_test[which(early_wide_test$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_botTD_int)
plot(yr_botTD_int)
vis.gam(yr_botTD_int, view=c("LATITUDE","LONGITUDE"))




