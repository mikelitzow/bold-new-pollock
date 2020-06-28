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













