#====================================================================================================================================
# Predict into NEBS with new simplified model
#
#Krista, Jun 2022
#====================================================================================================================================
#Notes:
#====================================================================================================================================
#
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")
library(tidyverse)
library(mgcv)
library(cowplot)


#import the model fit for the model with a linear interaction
cmod1_noint <- read_rds("~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/new_no_int.RDS")
summary(cmod1_noint)

#import the data

wd <- getwd()
northsouthdata_all <- read.csv(paste(wd,"/data/survey data/combined_cleaned_north-south_1982-2021_bot_trawl_data.csv",sep=""))

#limit to pollock data
NS_pollock <- northsouthdata_all[which(northsouthdata_all$SID=="21740"),]

#add period
NS_pollock$period <- NA
NS_pollock$period[which(NS_pollock$YEAR<2014)] <- "early"
NS_pollock$period[which(NS_pollock$YEAR>2013)] <- "late"
NS_pollock$period <- as.factor(NS_pollock$period)


pdat <- NS_pollock

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

#predict-------


pdat_NEBS <- pdat[which(pdat$region=="NEBS"),]

nebs_sel<- pdat_NEBS[,c(5, 13, 14, 19, 20:26)]
names(nebs_sel)

NEBSpred3 <- predict.gam(cmod1_noint$gam, newdata = nebs_sel)
length(NEBSpred3)
length(nebs_sel$BOT_DEPTH) #same length

pdat_NEBS$predicted <- NEBSpred3


NEBSpred4 <- predict.gam(cmod1_noint$gam, newdata = nebs_sel, type="response")
#seems same, still neg


#get difference
pdat_NEBS$difference <- pdat_NEBS$predicted - pdat_NEBS$logCPUE

#get RMSE----
vanilla_mod_rsme <- sqrt(mean((pdat_NEBS$logCPUE - pdat_NEBS$predicted)^2, na.rm=TRUE))

pdat1982 <- pdat_NEBS[which(pdat_NEBS$YEAR=="1982"),]
pdat1985 <- pdat_NEBS[which(pdat_NEBS$YEAR=="1985"),]
pdat1988 <- pdat_NEBS[which(pdat_NEBS$YEAR=="1988"),]
pdat1991 <- pdat_NEBS[which(pdat_NEBS$YEAR=="1991"),]

pdat2010 <- pdat_NEBS[which(pdat_NEBS$YEAR=="2010"),]
pdat2017 <- pdat_NEBS[which(pdat_NEBS$YEAR=="2017"),]
pdat2018 <- pdat_NEBS[which(pdat_NEBS$YEAR=="2018"),]
pdat2019 <- pdat_NEBS[which(pdat_NEBS$YEAR=="2019"),]
pdat2021 <- pdat_NEBS[which(pdat_NEBS$YEAR=="2021"),]

vanilla_1982_rsme <- sqrt(mean((pdat1982$logCPUE - pdat1982$predicted)^2, na.rm=TRUE))
vanilla_1985_rsme <- sqrt(mean((pdat1985$logCPUE - pdat1985$predicted)^2, na.rm=TRUE))
vanilla_1988_rsme <- sqrt(mean((pdat1988$logCPUE - pdat1988$predicted)^2, na.rm=TRUE))
vanilla_1991_rsme <- sqrt(mean((pdat1991$logCPUE - pdat1991$predicted)^2, na.rm=TRUE))
vanilla_2010_rsme <- sqrt(mean((pdat2010$logCPUE - pdat2010$predicted)^2, na.rm=TRUE))
vanilla_2017_rsme <- sqrt(mean((pdat2017$logCPUE - pdat2017$predicted)^2, na.rm=TRUE))
vanilla_2018_rsme <- sqrt(mean((pdat2018$logCPUE - pdat2018$predicted)^2, na.rm=TRUE))
vanilla_2019_rsme <- sqrt(mean((pdat2019$logCPUE - pdat2019$predicted)^2, na.rm=TRUE))
vanilla_2021_rsme <- sqrt(mean((pdat2021$logCPUE - pdat2021$predicted)^2, na.rm=TRUE))

vanilla_1982_rsme
vanilla_1985_rsme
vanilla_1988_rsme
vanilla_1991_rsme
vanilla_2010_rsme
vanilla_2017_rsme
vanilla_2018_rsme 
vanilla_2019_rsme

#plot----
#pivot longer so that can plot on same scale!

plot_pred_dat <- pdat_NEBS[,c(1:3, 5,  19, 24:28)] %>% pivot_longer(!c(LATITUDE, LONGITUDE, STATION, YEAR,  region, period, shelf), 
                                                                                    names_to="response_type", values_to="value")



names(plot_pred_dat)
table(plot_pred_dat$response_type) #three coloumns, difference, logCPUE... and predicted


#plot----
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 20, fun = mean, data=plot_pred_dat) + 
  facet_wrap(~interaction( YEAR, response_type), nrow=3)  +
  scale_fill_distiller(palette = "Spectral")

#difference
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  stat_summary_2d(aes(LONGITUDE,LATITUDE,  z=value), bins = 20, fun = mean, data=plot_pred_dat[which(plot_pred_dat$response_type=="difference"),]) + 
  facet_wrap(~interaction( YEAR, response_type), nrow=3)  +
  scale_fill_distiller(palette = "Spectral")



#what about as points?

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=value), 
             data=plot_pred_dat) + 
  facet_wrap(response_type~YEAR, nrow=3)  +
  scale_colour_distiller(palette = "Spectral") + theme_bw() +
  theme( legend.position = c(0.97, 0.25), legend.key = element_blank(),
         legend.background=element_blank(), legend.title = element_blank()) 

#just difference
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=value), 
             data=plot_pred_dat[which(plot_pred_dat$response_type=="difference"),]) + 
  facet_wrap(response_type~YEAR, nrow=3)  +
  scale_colour_distiller(palette = "Spectral") + theme_bw() +
  theme( legend.position = c(0.97, 0.25), legend.key = element_blank(),
         legend.background=element_blank(), legend.title = element_blank()) 

#look at bottom temp
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=BOT_TEMP), 
             data=pdat_NEBS) + 
  facet_wrap(~YEAR, nrow=3)  +
  scale_colour_distiller(palette = "Spectral") + theme_bw() +
  theme( legend.position = c(0.97, 0.25), legend.key = element_blank(),
         legend.background=element_blank(), legend.title = element_blank()) 


#look at bottom temp across both nebs and sebs
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(58, 66), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  col=BOT_TEMP), 
             data=NS_pollock) + 
  facet_wrap(~YEAR)  +
  scale_colour_distiller(palette = "Spectral") + theme_bw() +
  theme( legend.position = c(0.97, 0.25), legend.key = element_blank(),
         legend.background=element_blank(), legend.title = element_blank()) 


#one-to-one plots========

ggplot(pdat_NEBS, aes(predicted, logCPUE)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=as.factor(YEAR))) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)
#OH this is interesting!!!!!

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=BOT_TEMP)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0) +
  scale_colour_distiller(palette = "Spectral")

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=BOT_DEPTH)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)+
  scale_colour_distiller(palette = "Spectral")

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=long_albers)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)+
  scale_colour_distiller(palette = "Spectral") #

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=lat_albers)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=0)+
  scale_colour_distiller(palette = "Spectral") #

ggplot(pdat_NEBS, aes(predicted, logCPUE, col=as.factor(YEAR))) + geom_point() + 
  geom_smooth(method="lm") + geom_abline(intercept=0) + theme_bw() + ylab("log(CPUE+1)") + xlab("Predicted log(CPUE+1)") +
 facet_wrap(~YEAR)
  # scale_color_manual(values=c("#b2df8a", "#66c2a5", "#fc8d62", "#8da0cb"))

#adding in random effects======================================

#following examples from gamm() documentation

refa <- ranef(cmod1_noint$lme,level=3) #extract random effect for year
rownames(refa) <- substr(rownames(refa),start=5,stop=8) #rename names

## make a prediction, with random effects zero...
p0 <- predict(b$gam,data.frame(x0=.3,x1=.6,x2=.98,x3=.77))
p0_21 <- predict(cmod1_noint$gam, newdata = nebs_sel[which(nebs_sel$YEAR=="2021"),])

## add in effect for fa = "2" and fb="2/4"...
p <- p0_21 + refa["2021",1] 

refa$year <- rownames(refa)

#and loop through the years that we have nebs data for
yrs <- unique(nebs_sel$YEAR)
output_df <- data.frame(matrix(ncol = 2, nrow = 0))
nms <- c("ptemp_wrand", "year")
colnames(output_df) <- nms
i <- 1
for(i in 1:length(yrs)){
  temp_yr <- yrs[i]
  yr_dat <- nebs_sel[which(nebs_sel$YEAR==temp_yr),]
  
  ## make a prediction, with random effects zero...
  ptemp <- predict(cmod1_noint$gam, newdata = nebs_sel[which(nebs_sel$YEAR==temp_yr),])
  
  ## add in effect for fa = "2" and fb="2/4"...
  ptemp_wrand <- ptemp + refa[which(refa$year==temp_yr),1] 
  
  dftemp <- as.data.frame(ptemp_wrand)
  dftemp$year <- temp_yr
  
  output_df <- rbind(output_df, dftemp)
}

#ok nice the loop output seems to be working
#need to double check
#then need to add in metadata to loop df so that it can be used to plot etc





