#====================================================================================================================================
# Predict into NEBS
#
#Krista, Nov 2020
#====================================================================================================================================
#Notes:
#====================================================================================================================================
#
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")


#import the model fit for the model with a linear interaction
pmod <- read_rds("~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/lin-int_allages_model.RDS")
summary(pmod)

#import the data
#use data previously cleaned 
#periods_analysis_dat is also loaded in trawl_biomass_GAM_explor.R
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)

pdat <- periods_analysis_dat

#----
#sort into NEBS and SEBS

pdat$region <- "SEBS"
pdat$region[which(pdat$STRATUM==81 | 
                    pdat$STRATUM==70 |
                    pdat$STRATUM==71)] <- "NEBS"


#add shelves 
#based on table 1 in Laurth et al 2019 NOAA Technical Memorandum NMFS-AFSC-396

pdat$shelf <- NA
pdat$shelf[which(pdat$STRATUM==81 | 
                               pdat$STRATUM==70 |
                               pdat$STRATUM==71)] <- "NEBS"
pdat$shelf[which(pdat$STRATUM==10 | 
                               pdat$STRATUM==20)] <- "EBS_inner"
pdat$shelf[which(pdat$STRATUM==31 | 
                               pdat$STRATUM==32 | 
                               pdat$STRATUM==41 | 
                               pdat$STRATUM==42 | 
                               pdat$STRATUM==43 | 
                               pdat$STRATUM==82)] <- "EBS_middle"
pdat$shelf[which(pdat$STRATUM==50 | 
                               pdat$STRATUM==61 | 
                               pdat$STRATUM==62 | 
                               pdat$STRATUM==90)] <- "EBS_outer"
#----

#set up data to 1) drop high mean stations temps and 2) set high mean station temps to highest observed in SEBS
# all_analysis_dat from NEBS_depth_temp_plot.R
#needs lat/long albers and period

#what is highest mean station bottemp in early?
maxmean_bottemp_early <- max(pdat$mean_station_bottemp[which(pdat$YEAR<2014 & pdat$region=="SEBS")])
#6.72

pdat_limited <- pdat[which(pdat$mean_station_bottemp<maxmean_bottemp_early),]

pdat_NEBS <- pdat_limited[which(pdat_limited$region=="NEBS"),]

#just cols used for prediction
#nebs_sel <- pdat_NEBS[,c(12, 21:23, 26:28)] old based on all_analysis_dat cols
nebs_sel <- pdat_NEBS[,c(7, 13, 14:15, 45, 50:54)]


NEBSpred1 <- predict(pmod, newdata = nebs_sel)    #not working

NEBSpred2 <- predict.gam(pmod$gam, newdata = nebs_sel)
length(NEBSpred2)
length(nebs_sel$BOT_DEPTH)

NEBSpredlme <- predict(pmod$lme, newdata = nebs_sel)    #doesn't work

NEBSpred3 <- predicted_samples(pmod, newdata = nebs_sel)

NEBSpredres <-  predict.gam(pmod$gam, newdata = nebs_sel, type="response") #same

smod <- read_rds("~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/smoothtemp_allages_model.RDS")
summary(smod)

smoothtemp_pred <- predict.gam(smod$gam, newdata = nebs_sel)


pdat_NEBS$predicted <- NEBSpred2
pdat_NEBS$smoothT_predicted <- smoothtemp_pred

pp1 <- ggplot(pdat_NEBS, aes(lat_albers, long_albers, col=predicted))
pp1 + geom_point()


world <- ne_countries(scale = "medium", returnclass = "sf")






p1 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 68), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=predicted), data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_color_distiller(palette = "Spectral")



library(cowplot)




p3 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(55, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=predicted), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_fill_distiller(palette = "Spectral")




p5 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=predicted), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR=="2010"),]) +   
  scale_fill_distiller(palette = "Spectral")

p6 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=predicted), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR=="2017"),]) +   
  scale_fill_distiller(palette = "Spectral")

p7 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=predicted), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR=="2018"),]) +   
  scale_fill_distiller(palette = "Spectral")

plot_grid(p5, p6, p7, labels=c("2010", "2017", "2018"), nrow=1)









#pivot longer so that can plot on same scale!

# plot_pred_dat <- pdat_NEBS[,c(1:3, 5, 20, 24:26, 29, 31)] %>% pivot_longer(!c(LATITUDE, LONGITUDE, STATION, YEAR, region, period, shelf), 
#                                                                        names_to="response_type", values_to="value")
plot_pred_dat <- pdat_NEBS[,c(1:3, 5, 45, 50, 53:56)] %>% pivot_longer(!c(LATITUDE, LONGITUDE, STATION, YEAR,  region, period, shelf),
                                                                           names_to="response_type", values_to="value")

# plot_pred_dat <- pdat_NEBS[,c(1:3, 5, 45, 50, 53:54, 56)] %>% pivot_longer(!c(LATITUDE, LONGITUDE, STATION, YEAR,  region, period, shelf), 
#                                                                        names_to="response_type", values_to="value")


View(plot_pred_dat)




ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 30, fun = mean, data=plot_pred_dat) + 
  facet_wrap(~response_type)  +
  scale_fill_distiller(palette = "Spectral")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 30, fun = mean, data=plot_pred_dat) + 
  facet_wrap(~interaction(response_type, YEAR), nrow=4)  +
  scale_fill_distiller(palette = "Spectral")


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 20, fun = mean, data=plot_pred_dat) + 
  facet_wrap(~interaction( YEAR, response_type), nrow=2)  +
  scale_fill_distiller(palette = "Spectral")

#hmm predicted always seems lower

#for presentation


plot_pred_dat2 <- plot_pred_dat

plot_pred_dat2$response_type2 <- plot_pred_dat2$response_type

plot_pred_dat2$response_type2[which(plot_pred_dat$response_type=="logCPUE_Gadus_chalcogrammus")] <- "Actual"
plot_pred_dat2$response_type2[which(plot_pred_dat$response_type=="predicted")] <- "Predicted"

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 20, fun = mean, data=plot_pred_dat2) + 
  facet_wrap(response_type2~YEAR, nrow=2)  +
  scale_fill_distiller(palette = "Spectral")





ggplot(pdat_NEBS, aes(predicted, logCPUE)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=YEAR_factor)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)
#OH this is interesting!!!!!

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=bottemp_anom)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0) +
  scale_colour_distiller(palette = "Spectral")

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=BOT_DEPTH)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)+
  scale_colour_distiller(palette = "Spectral")

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=mean_station_bottemp)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)+
  scale_colour_distiller(palette = "Spectral") #hmm maybe underestimates cold stations (blue band near top)

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=long_albers)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)+
  scale_colour_distiller(palette = "Spectral") #hmm maybe underestimates cold stations (blue band near top)

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=lat_albers)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)+
  scale_colour_distiller(palette = "Spectral") #hmm maybe underestimates cold stations (blue band near top)





#repeat with adjusted temps=======

#here subtract the difference in mean temps before vs after (1.38) for SEBS from NEBS stations

pdat$adjusted_bottom_temp <- pdat$mean_station_bottemp
#pdat$adjusted_bottom_temp[which(pdat$shelf=="NEBS")] <- pdat$adjusted_bottom_temp-1.38

pdat$adjusted_bottom_temp <- pdat$mean_station_bottemp
for(i in 1:length(pdat$mean_station_bottemp)){
  if (pdat$shelf[i]=="NEBS") {pdat$adjusted_bottom_temp[i] <- pdat$adjusted_bottom_temp[i] - 1.38} 
  #print(i)
}

pdat_NEBS_Ad <- pdat[which(pdat$region=="NEBS"),]

nebs_sel_adjusted <- pdat_NEBS_Ad[,c(7, 13, 14:15, 45, 50:55)]

NEBSpred3 <- predict.gam(pmod$gam, newdata = nebs_sel_adjusted)
length(NEBSpred3)
length(nebs_sel_adjusted$BOT_DEPTH) #same length

pdat_NEBS_Ad$predicted_adjusted <- NEBSpred3


#pivot longer so that can plot on same scale!

plot_ad_pred_dat <- pdat_NEBS_Ad[,c(1:3, 5, 45, 50, 53:54, 56)] %>% pivot_longer(!c(LATITUDE, LONGITUDE, STATION, YEAR,  region, period, shelf), 
                                                                           names_to="response_type", values_to="value")

View(plot_ad_pred_dat)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 20, fun = mean, data=plot_ad_pred_dat) + 
  facet_wrap(~interaction( YEAR, response_type), nrow=2)  +
  scale_fill_distiller(palette = "Spectral")



#for figures draft
#edit below
plot_ad_pred_dat2 <- plot_ad_pred_dat

plot_ad_pred_dat2$response_type2 <- plot_ad_pred_dat2$response_type

plot_ad_pred_dat2$response_type2[which(plot_ad_pred_dat$response_type=="logCPUE_Gadus_chalcogrammus")] <- "Actual"
plot_ad_pred_dat2$response_type2[which(plot_ad_pred_dat$response_type=="predicted_adjusted")] <- "Predicted"

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 20, fun = mean, data=plot_ad_pred_dat2) + 
  facet_wrap(response_type2~YEAR, nrow=2)  +
  scale_fill_distiller(palette = "Spectral") + theme_bw() +
  theme( legend.position = c(0.97, 0.25), legend.key = element_blank(),
         legend.background=element_blank(), legend.title = element_blank()) 




