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


#going to need to get into a wider database I think to get other sps in
#and I need to only use early years!
#don't smooth year just factor

#try w just pollock first

#proof of concept gams=====

yr_int <- gam(logCPUE ~ YEAR + s(LATITUDE, LONGITUDE, YEAR), 
                      data=sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_int)
plot(yr_int)
vis.gam(yr_int, view=c("LATITUDE","LONGITUDE"))
#look at predict.gam

yr_botT_int <- gam(logCPUE ~ YEAR + BOT_TEMP + s(LATITUDE, LONGITUDE, YEAR), 
              data=sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_botT_int)
plot(yr_botT_int)
vis.gam(yr_botT_int, view=c("LATITUDE","LONGITUDE"))

yr_botTD_int <- gam(logCPUE ~ YEAR + BOT_TEMP + BOT_DEPTH + s(LATITUDE, LONGITUDE, YEAR), 
                   data=sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_botTD_int)
plot(yr_botTD_int)
vis.gam(yr_botTD_int, view=c("LATITUDE","LONGITUDE"))




