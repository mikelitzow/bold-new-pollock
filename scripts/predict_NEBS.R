#====================================================================================================================================
# Predict into NEBS
#
#Krista, Nov 2020
#====================================================================================================================================
#Notes:
#====================================================================================================================================
#

#import the model fit for the model with a linear interaction
pmod <- read_rds("~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/lin-int_allages_model.RDS")
summary(pmod)

#set up data to 1) drop high mean stations temps and 2) set high mean station temps to highest observed in SEBS
# all_analysis_dat from NEBS_depth_temp_plot.R
#needs lat/long albers and period

#what is highest mean station bottemp in early?
maxmean_bottemp_early <- max(all_analysis_dat$mean_station_bottemp[which(all_analysis_dat$YEAR<2014)])
#11.7

pdat_limited <- all_analysis_dat[which(all_analysis_dat$mean_station_bottemp<maxmean_bottemp_early),]

#just cols used for prediction
pdat_sel <- pdat_limited[,c(21:23)]















