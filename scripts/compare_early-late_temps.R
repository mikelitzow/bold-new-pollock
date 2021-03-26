#=============================================================
#How do 2017-2019 temp means compare to earlier?
#
#Krista, Feb 2021
#=============================================================
#Notes:
#Exploring Franz's suggestion that NEBS temps might need to be
#corrected down because they were measured in such high years
#=============================================================

#use periods_analysis_dat
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)


#here is origincal calculation
#annual 'global' (w/in dataset) mean
earlymeans <- periods_analysis_dat[which(periods_analysis_dat$YEAR<2014),] %>% group_by(STATION) %>% summarize(early_mean_station_bottemp=mean(BOT_TEMP))
latemeans <- periods_analysis_dat[which(periods_analysis_dat$YEAR>=2014),] %>% group_by(STATION) %>% summarize(late_mean_station_bottemp=mean(BOT_TEMP))

allmeans1 <- left_join(periods_analysis_dat, earlymeans)
allmeans <- left_join(allmeans1, latemeans)

justmeans <- left_join(earlymeans, latemeans)
justmeans$diff_evl <- justmeans$early_mean_station_bottemp - justmeans$late_mean_station_bottemp

#allmeans$diff_all_late <- allmeans$mean_station_bottemp - allmeans$late_mean_station_bottemp

 # allmeans$diff_early_late <- allmeans$early_mean_station_bottemp - allmeans$late_mean_station_bottemp

  ggplot(allmeans, aes(diff_all_late, mean_station_bottemp)) + geom_point()
  #nearly ALL warmer
  
  ggplot(justmeans, aes(diff_evl)) + geom_histogram()
  #nearly ALL warmer
  
 diffmapdat <- left_join(justmeans, periods_analysis_dat[,c(1:4)])
  
  ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(-180, -155), ylim = c(54, 66), expand = TRUE) +
    stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=diff_evl), bins = 30, fun = mean, 
                    data= diffmapdat[which(diffmapdat$STRATUM!="70" &
                                             diffmapdat$STRATUM!="71" & 
                                             diffmapdat$STRATUM!="81" ),]) +   
    scale_fill_distiller(palette = "Spectral")
  
  early_sebs_mean <- mean(diffmapdat$early_mean_station_bottemp[which(diffmapdat$STRATUM!="70" &
                                   diffmapdat$STRATUM!="71" & 
                                   diffmapdat$STRATUM!="81" )])
  
  late_sebs_mean <- mean(diffmapdat$late_mean_station_bottemp[which(diffmapdat$STRATUM!="70" &
                                                                        diffmapdat$STRATUM!="71" & 
                                                                        diffmapdat$STRATUM!="81" )])
  
  late_sebs_mean - early_sebs_mean
  
  tempmod <- lm(late_mean_station_bottemp ~ mean_station_bottemp,
                data=allmeans[which(allmeans$STRATUM!="70" &
                                      allmeans$STRATUM!="71" & 
                                      allmeans$STRATUM!="81" ),])
summary(tempmod)
ggplot(allmeans[which(allmeans$STRATUM!="70" &
                        allmeans$STRATUM!="71" & 
                        allmeans$STRATUM!="81" ),], 
       aes(late_mean_station_bottemp, mean_station_bottemp, col=as.factor(STRATUM))) +
  geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)





