#============================================================================================================================================
# DFA on EBS recruitment data

#Created by Krista, April 2020
#Based on "BS_climate_DFA.R"
#============================================================================================================================================
#Notes:
#============================================================================================================================================
library(ggplot2)
library(reshape2)
library(dplyr)
library(pracma)
library(MARSS)
library(tidyr)
# devtools::install_github("kassambara/ggpubr")
library(ggpubr)
library(cowplot)
library(gridExtra)
library(broom)
library(lemon)
library(MuMIn)
library(lmtest)
library(cowplot)

#Get data plot data =======

# load environmental data
rec_dat <- read.csv("data/EBS.recruit.time.series.csv")

# plot TS for SI
do.rec.dat <- as.data.frame(scale(rec_dat[,-1])) # scale to plot on 1 axis
do.rec.dat$year <- rec_dat$year
plot.rec.dat <- gather(do.rec.dat, key=key, value=value, -year)


ggplot(plot.rec.dat, aes(x=year, y=value)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~key) +
  ylab("Standard anomaly") + xlab("") + theme_bw() + geom_hline(yintercept = 0)

















