#============================================================================================================================================
# DATA EXPLORATION - survey abundance data

#Created by Krista, May 2020
#============================================================================================================================================
#Notes:
#============================================================================================================================================
library(ggplot2)
library(tidyverse)
library(corrplot)

#for now using trawl.data from 'trawl data processing.R' just to get a feel for the data
summary(trawl.data)

unique(trawl.data$COMMON)
unique(trawl.data$SCIENTIFIC) #WOW a lot of sps!!

pol.trawl.dat <- trawl.data[which(trawl.data$SCIENTIFIC=="Gadus chalcogrammus"),]

ggplot(pol.trawl.dat, aes(WTCPUE)) + geom_histogram() + facet_wrap(~YEAR, scales="free")

table(pol.trawl.dat$STATION, pol.trawl.dat$YEAR)
length(unique(pol.trawl.dat$STATION))

ggplot(pol.trawl.dat, aes(YEAR, WTCPUE, colour=STATION)) + geom_point() + theme(legend.position = "none")

ggplot(pol.trawl.dat, aes(YEAR, WTCPUE, colour=STATION)) + geom_line() + theme(legend.position = "none")

ggplot(pol.trawl.dat, aes(YEAR, WTCPUE, colour=as.factor(STRATUM))) + geom_line() + theme(legend.position = "none")

ggplot(pol.trawl.dat, aes(YEAR, WTCPUE)) + geom_point() + theme(legend.position = "none") +
  geom_smooth() + facet_wrap(~STRATUM)


#are there -9999 in the data? Don't seem to be for pollock, maybe for other species???

pol.trawl.dat$logCPUE <- log(pol.trawl.dat$WTCPUE + 1)

ggplot(pol.trawl.dat, aes(YEAR, logCPUE)) + geom_point() + theme(legend.position = "none") +
  geom_smooth() + facet_wrap(~STRATUM)

ggplot(pol.trawl.dat, aes(logCPUE)) + geom_histogram() + facet_wrap(~YEAR, scales="free") #little better to my eye

pol.trawl.dat$fourthrootCPUE <- pol.trawl.dat$WTCPUE^(1/4)

ggplot(pol.trawl.dat, aes(fourthrootCPUE)) + geom_histogram() + facet_wrap(~YEAR, scales="free") #long tail

dat1 <- as.matrix(pol.trawl.dat[,c(7:8,12:19)])
cor1 <- cor(dat1, use="complete.obs")
corrplot(cor1)
