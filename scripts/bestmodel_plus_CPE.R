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

#join clim data to data============

do.cdat$YEAR <- as.integer(rownames(do.cdat))

sub_CPE <- do.cdat[,c("YEAR", "summer.cold.pool.extent")]

cpedat <- left_join(periods_analysis_dat, sub_CPE)

#new model w CPE===================

mod_CPE <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom*period + summer.cold.pool.extent +
                       te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=cpedat[which(cpedat$STRATUM!=70 &
                                         cpedat$STRATUM!=71 &
                                         cpedat$STRATUM!=81),], method="ML")

gam.check(mod_CPE[[2]]) 
summary(mod_CPE[[1]]) #  38250.09 38353.64 -19111.05
summary(mod_CPE[[2]]) #rsq 0.384 




mod_CPE_noyr <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom*period + summer.cold.pool.extent +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), #random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=cpedat[which(cpedat$STRATUM!=70 &
                                    cpedat$STRATUM!=71 &
                                    cpedat$STRATUM!=81),], method="ML")

gam.check(mod_CPE_noyr[[2]]) 
summary(mod_CPE_noyr[[1]]) #  38248.18 38344.32 -19111.09
summary(mod_CPE_noyr[[2]]) #rsq 0.384


mod_CPE_only <- gamm(logCPUE_Gadus_chalcogrammus ~ summer.cold.pool.extent +
                       te(mean_station_bottemp, BOT_DEPTH, k=29), #random=list(YEAR_factor=~1), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=cpedat[which(cpedat$STRATUM!=70 &
                                         cpedat$STRATUM!=71 &
                                         cpedat$STRATUM!=81),], method="ML")

gam.check(mod_CPE_only[[2]]) 
summary(mod_CPE_only[[1]]) #  38343.83 38417.79 -19161.92
summary(mod_CPE_only[[2]]) #rsq 0.358 

draw(mod_CPE_only$gam, select = 1)
draw(mod_CPE_only$gam, select = 1, dist=0.05)
draw(mod_CPE_only$gam, select = 1, dist=0.01)


#uh oh this included NEBS stations
#will likely need to update elsewhere too



