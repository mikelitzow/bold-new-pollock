#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#New simplified model structure on new dataset

#Krista, May 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(mgcv)
library(gamm4)

#load data
wd <- getwd()
northsouthdata_all <- read.csv(paste(wd,"/data/survey data/combined_cleaned_north-south_1982-2021_bot_trawl_data.csv",sep=""))

newsebs <- northsouthdata_all[which(northsouthdata_all$STRATUM!="70" &
                                      northsouthdata_all$STRATUM!="71" &
                                      northsouthdata_all$STRATUM!="81" &
                                      northsouthdata_all$STRATUM!="0" ),]

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=newsebs) + theme_bw() 

#limit to pollock data
sebs_pollock <- newsebs[which(newsebs$SID=="21740"),]

#add period
sebs_pollock$period <- NA
sebs_pollock$period[which(sebs_pollock$YEAR<2014)] <- "early"
sebs_pollock$period[which(sebs_pollock$YEAR>2013)] <- "late"
sebs_pollock$period <- as.factor(sebs_pollock$period)

#models------


startmod <- gamm4(logCPUE ~  s(BOT_DEPTH) +
               s(BOT_TEMP, by=period, bs="fs"),  random=~(1|YEAR/HAUL), 
             data=sebs_pollock)
summary(startmod[[1]]) 
summary(startmod[[2]]) 
anova(startmod[[2]])
plot(startmod[[2]])
#is there any spatial correlation though?
plot(Variogram(startmod$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=sebs_pollock))
plot(Variogram(startmod$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=FALSE, data=sebs_pollock))
#actually looks fairly minor, but what about plots?

gam.check(startmod[[2]]) 













