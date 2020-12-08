#==========================================================================================
# Adding in Cold Pool Extent (CPE)
#
# Krista, Dec 2020
#==========================================================================================
#Notes:
#==========================================================================================

library(mgcv)
library(ggplot2)
#remotes::install_github("gavinsimpson/gratia")
library(gratia)

#Get data plot data =======

# load environmental data
cdat <- read.csv("data/climate data.csv", row.names = 1)


# scale data
do.cdat <- as.data.frame(scale(cdat)) # scale to plot on 1 axis
plot.cdat <- gather(do.cdat)
plot.cdat$year <- 1951:2019

ggplot(plot.cdat, aes(x=year, y=value)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~key) +
  ylab("Standard anomaly") + xlab("") + theme_bw() + geom_hline(yintercept = 0)


#grab best model and data from previous scripts (allages_model_fitting.R)

#use data previously cleaned 
#periods_analysis_dat is also loaded in trawl_biomass_GAM_explor.R
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)

#model
mod <- readRDS(file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/lin-int_allages_model.RDS")

summary(mod)

gam.check(mod[[2]]) 
summary(mod[[1]]) #   44063.32 44161.09 -22018.66
summary(mod[[2]]) #rsq   0.366 

mod_CPE <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom*period +
                       te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=periods_analysis_dat, method="ML")

gam.check(mod_CPE[[2]]) 
summary(mod_CPE[[1]]) #  
summary(mod_CPE[[2]]) #rsq 












