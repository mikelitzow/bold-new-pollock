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

sel.trawl.dat$YEAR <- as.ordered(sel.trawl.dat$YEAR) #set year as ordered factor











