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

pdat_NEBS <- pdat_limited[which(pdat_limited$region=="NEBS"),]

#just cols used for prediction
nebs_sel <- pdat_NEBS[,c(12, 21:23, 26:28)]


NEBSpred1 <- predict(pmod, newdata = nebs_sel)    #not working

NEBSpred2 <- predict.gam(pmod$gam, newdata = nebs_sel)
length(NEBSpred2)
length(nebs_sel$BOT_DEPTH)

NEBSpredlme <- predict(pmod$lme, newdata = nebs_sel)    #doesn't work

NEBSpred3 <- predicted_samples(pmod, newdata = nebs_sel)

NEBSpredres <-  predict.gam(pmod$gam, newdata = nebs_sel, type="response") #same

pdat_NEBS$predicted <- NEBSpred2

pp1 <- ggplot(pdat_NEBS, aes(lat_albers, long_albers, col=predicted))
pp1 + geom_point()

ppdiff <- ggplot(pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),], aes(lat_albers, long_albers, col=(predicted-logCPUE)/logCPUE))
ppdiff + geom_point()

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 68), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +  
  geom_point(aes(lat_albers, long_albers, col=(predicted-logCPUE)/logCPUE), data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_colour_gradient2()



ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 68), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=(predicted-logCPUE)/logCPUE), data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_colour_gradient2()

p1 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 68), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=predicted), data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_color_distiller(palette = "Spectral")


p2 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 68), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=logCPUE), data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_color_distiller(palette = "Spectral")

library(cowplot)
plot_grid(p1, p2)



p3 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(55, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=predicted), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_fill_distiller(palette = "Spectral")

p4 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(55, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=logCPUE), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) +   
  scale_fill_distiller(palette = "Spectral")

plot_grid(p3, p4, labels=c("predicted", "logCPUE"))


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


p8 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=logCPUE), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR=="2010"),]) +   
  scale_fill_distiller(palette = "Spectral")

p9 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=logCPUE), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR=="2017"),]) +   
  scale_fill_distiller(palette = "Spectral")

p10 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=logCPUE), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR=="2018"),]) +   
  scale_fill_distiller(palette = "Spectral")

plot_grid(p5, p6, p7, p8, p9, p10, nrow=2)




#pivot longer so that can plot on same scale!

plot_pred_dat <- pdat_NEBS[,c(1:3, 5, 20, 24:26, 29)] %>% pivot_longer(!c(LATITUDE, LONGITUDE, STATION, YEAR, region, period, shelf), 
                                                                       names_to="response_type", values_to="value")
View(plot_pred_dat)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 30, fun = mean, data=plot_pred_dat[which(plot_pred_dat$YEAR!="2019"),]) + 
  facet_wrap(~response_type)  +
  scale_fill_distiller(palette = "Spectral")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 30, fun = mean, data=plot_pred_dat[which(plot_pred_dat$YEAR!="2019"),]) + 
  facet_wrap(~interaction(response_type, YEAR), nrow=3)  +
  scale_fill_distiller(palette = "Spectral")


#hmm predicted always seems lower


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=(logCPUE-predicted), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) + 
  facet_wrap(~YEAR, nrow=3)  +
  scale_fill_distiller(palette = "Spectral")

pdat_NEBS$proportion_diff_predvcpue <- (pdat_NEBS$logCPUE - pdat_NEBS$predicted)/pdat_NEBS$logCPUE #hmm might not be ideal?

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=proportion_diff_predvcpue), bins = 30, fun = mean, data=pdat_NEBS[which(pdat_NEBS$YEAR!="2019"),]) + 
  facet_wrap(~YEAR, nrow=3)  +
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


