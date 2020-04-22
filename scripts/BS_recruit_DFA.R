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

raw.rec.dat <- gather(rec_dat, key=species, value=recruitment, -year)

ggplot(raw.rec.dat, aes(x=year, y=recruitment)) + geom_point() + geom_line() +
  facet_wrap(~species, scales="free") +
  ylab("Recruitment") + xlab("") + theme_bw()

ggplot(raw.rec.dat, aes(x=year, y=recruitment)) + geom_point() + geom_smooth() +
  facet_wrap(~species, scales="free") +
  ylab("Recruitment") + xlab("") + theme_bw() 

#log transform====

log_rec <- as.data.frame(cbind(rec_dat$year, log(rec_dat$YLFN.R.age1.18.1a), log(rec_dat$TRBT.Age.0.R.16.1b),
                 log(rec_dat$FHSL.R.age0), log(rec_dat$Opilio.age0.recruits),
                 log(rec_dat$cod.age0.R), log(rec_dat$pollock.age0.R)))
#set col names
log_rec <- log_rec %>% 
  rename(
    year = V1,
    YLFN.R.age1.18.1a = V2,
    TRBT.Age.0.R.16.1b = V3,
    FHSL.R.age0 = V4,
    Opilio.age0.recruits = V5,
    cod.age0.R = V6,
    pollock.age0.R = V7
  )


log.rec.dat <- gather(log_rec, key=species, value=log_recruitment, -year)

ggplot(log.rec.dat, aes(x=year, y=log_recruitment)) + geom_point() + geom_line() +
  facet_wrap(~species, scales="free") +
  ylab("log(Recruitment)") + xlab("") + theme_bw()









