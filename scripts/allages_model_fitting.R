#=============================================================================================
#model fitting for all ages model
#
#by Krista, Oct 2020
#=============================================================================================
#Notes: want to double check model fitting, since AIC may or may not (?) work with gamm()
#=============================================================================================

#use data previously cleaned 
#periods_analysis_dat is also loaded in trawl_biomass_GAM_explor.R
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)

#data exploration done in trawl_biomass_GAM_explor.R

#now I want to step through model selection process looking beyond AIC

repeatbase <- gamm(logCPUE_Gadus_chalcogrammus ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 data=periods_analysis_dat)
#is there any spatial correlation though?
plot(Variogram(repeatbase$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=periods_analysis_dat))
plot(Variogram(repeatbase$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=FALSE, data=periods_analysis_dat))
#actually looks fairly minor, but what about plots?

gam.check(repeatbase[[2]]) #does NOT like k
#qq a little curly on ends

check.gamViz(repeatbase[[2]])
















