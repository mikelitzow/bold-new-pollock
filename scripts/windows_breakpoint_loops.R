#====================================================================================================================
#Looping through to test different windows and breakpoints
#
#by Krista, Oct-2020
#====================================================================================================================
#Notes:
#====================================================================================================================

#read in periods_analysis_data from trawl_biomass_GAM_explor but rename
wd <- getwd()
loops_dat <- read.csv(file=paste(wd,"/data/processed_periods_analysis_data.csv", sep=""), row.names = 1)
names(loops_dat)

yrs <- unique(loops_dat$YEAR)
n_yrs <- length(yrs)
possible_breaks <- yrs[which(yrs>(1982+5) & yrs<(2019-5))]


#starting with breakpoint
#will try all possible breakpoints with at least 6 yrs data on either side 
#(b/c don't want a smaller # yrs in either levels than the 2014 breakpoint)

breakpoint_vec <- vector(mode="numeric", length=length(possible_breaks))
AIC_split_vec <- vector(mode="numeric", length=length(possible_breaks))
AIC_drop_vec <- vector(mode="numeric", length=length(possible_breaks))


i<-1
for(i in 1:length(possible_breaks)){
  #set up data with new breakpoint
  print(possible_breaks[i])
  temp_break <- possible_breaks[i]
  temp_dat <- loops_dat
  temp_dat$split <- NA
  temp_dat$split[which(temp_dat$YEAR>(temp_break-1))] <- "post"
  temp_dat$split[which(temp_dat$YEAR<(temp_break))] <- "pre"
  
  temp_dat$split <- as.factor(temp_dat$split)
  
  #run model
  temp_mod <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom:split + ti(mean_station_bottemp, BOT_DEPTH) +
                     s(YEAR_factor, bs="re"), 
                   correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                   data=temp_dat)
  
  #save output
  breakpoint_vec[i] <- temp_break      
  AIC_split_vec[i] <- AIC(temp_mod[[1]])
  saveRDS(temp_mod, file=paste(wd,"/data/breakpoint_model", temp_break, ".csv", sep=""))
  
  
  #run drop model
  temp_drop <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom + ti(mean_station_bottemp, BOT_DEPTH) +
                     s(YEAR_factor, bs="re"), 
                   correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                   data=temp_dat)
  
  #save output
  AIC_drop_vec[i] <- AIC(temp_drop[[1]])
  saveRDS(temp_drop, file=paste(wd,"/data/breakpoint_drop_model", temp_break, ".csv", sep=""))
  
}

outputdf <- as.data.frame(cbind(breakpoint_vec, AIC_split_vec, AIC_drop_vec))

outputdf$difference_AIC <- outputdf$AIC_split_vec - outputdf$AIC_drop_vec


#next cut off data six yrs after breakpoint and try again



breakpoint_six_vec <- vector(mode="numeric", length=length(possible_breaks))
AIC_split_six_vec <- vector(mode="numeric", length=length(possible_breaks))
AIC_drop_six_vec <- vector(mode="numeric", length=length(possible_breaks))


i<-1
for(i in 1:length(possible_breaks)){
  #set up data with new breakpoint
  print(possible_breaks[i])
  temp_break <- possible_breaks[i]
  temp_dat <- loops_dat[which(loops_dat$YEAR<(temp_break+5)),]
  temp_dat$split <- NA
  temp_dat$split[which(temp_dat$YEAR>(temp_break-1))] <- "post"
  temp_dat$split[which(temp_dat$YEAR<(temp_break))] <- "pre"
  
  temp_dat$split <- as.factor(temp_dat$split)
  
  #run model
  temp_mod <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom:split + ti(mean_station_bottemp, BOT_DEPTH) +
                     s(YEAR_factor, bs="re"), 
                   correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                   data=temp_dat)
  
  #save output
  breakpoint_six_vec[i] <- temp_break      
  AIC_split_six_vec[i] <- AIC(temp_mod[[1]])
  saveRDS(temp_mod, file=paste(wd,"/data/breakpoint_six_model", temp_break, ".csv", sep=""))
  
  
  #run drop model
  temp_drop <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom + ti(mean_station_bottemp, BOT_DEPTH) +
                      s(YEAR_factor, bs="re"), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=temp_dat)
  
  #save output
  AIC_drop_six_vec[i] <- AIC(temp_drop[[1]])
  saveRDS(temp_drop, file=paste(wd,"/data/breakpoint_six_drop_model", temp_break, ".csv", sep=""))
  
}

outputdf_six <- as.data.frame(cbind(breakpoint_six_vec, AIC_split_six_vec, AIC_drop_six_vec))

outputdf_six$difference_AIC <- outputdf_six$AIC_split_six_vec - outputdf_six$AIC_drop_six_vec




