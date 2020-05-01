#============================================================================================================================================
# DATA EXPLORATION - survey abundance data

#Created by Krista, May 2020
#============================================================================================================================================
#Notes:
#============================================================================================================================================
library(ggplot2)
library(tidyverse)

#for now using trawl.data from 'trawl data processing.R' just to get a feel for the data
summary(trawl.data)

unique(trawl.data$COMMON)
unique(trawl.data$SCIENTIFIC) #WOW a lot of sps!!

pol.trawl.dat <- trawl.data[which(trawl.data$SCIENTIFIC=="Gadus chalcogrammus"),]

ggplot(pol.trawl.dat, aes(NUMCPUE)) + geom_histogram() + facet_wrap(~YEAR, scales="free")

table(pol.trawl.dat$STATION, pol.trawl.dat$YEAR)
length(unique(pol.trawl.dat$STATION))

ggplot(pol.trawl.dat, aes(YEAR, NUMCPUE, colour=STATION)) + geom_point() + theme(legend.position = "none")

ggplot(pol.trawl.dat, aes(YEAR, NUMCPUE, colour=STATION)) + geom_line() + theme(legend.position = "none")

ggplot(pol.trawl.dat, aes(YEAR, NUMCPUE, colour=as.factor(STRATUM))) + geom_line() + theme(legend.position = "none")

ggplot(pol.trawl.dat, aes(YEAR, NUMCPUE)) + geom_point() + theme(legend.position = "none") +
  geom_smooth() + facet_wrap(~STRATUM)

nonzero_pollock <- pol.trawl.dat[which(pol.trawl.dat$NUMCPUE>1),]
#are there -9999 in the data? Don't seem to be for pollock, maybe for other species???

ggplot(nonzero_pollock, aes(YEAR, NUMCPUE)) + geom_point() + theme(legend.position = "none") +
  geom_smooth() + facet_wrap(~STRATUM)
