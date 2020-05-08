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


#going to need to get into a wider database I think
#try w just pollock first
onlysmoothers <- gam(logCPUE ~ s(YEAR) + ts(LATITUDE, LONGITUDE), 
            data=sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),])







