#try plotting depth * mean_station_bottemp for NEBS

#using joinboth from trawl_biomass_GAM_explor.R
#hmm still seems to remove NEBS
#use pol.trawl.dat instead

#to do this I am going to need to:
# 1 - get mean station temp for NEBS
# 2 - figure out which stations are NEBS!

#ggplot(joinboth, aes(BOT_DEPTH))

library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")



pol.trawl.dat$YEAR_factor <- as.factor(pol.trawl.dat$YEAR)
pol.trawl.dat$BOT_TEMP[which(pol.trawl.dat$BOT_TEMP=="-9999")]<-NA
pol.trawl.dat$SURF_TEMP[which(pol.trawl.dat$SURF_TEMP=="-9999")]<-NA
pol.trawl.dat$WTCPUE[which(pol.trawl.dat$WTCPUE=="-9999")]<-NA
pol.trawl.dat$NUMCPUE[which(pol.trawl.dat$NUMCPUE=="-9999")]<-NA

pol.trawl.dat$LATITUDE  <- as.numeric(pol.trawl.dat$LATITUDE)
pol.trawl.dat$LONGITUDE <- as.numeric(pol.trawl.dat$LONGITUDE)
pol.trawl.dat$YEAR <- as.numeric(pol.trawl.dat$YEAR )
pol.trawl.dat$WTCPUE <- as.numeric(pol.trawl.dat$WTCPUE)
pol.trawl.dat$NUMCPUE <- as.numeric(pol.trawl.dat$NUMCPUE)
pol.trawl.dat$BOT_DEPTH <- as.numeric(pol.trawl.dat$BOT_DEPTH )
pol.trawl.dat$BOT_TEMP <- as.numeric(pol.trawl.dat$BOT_TEMP)
pol.trawl.dat$SURF_TEMP <- as.numeric(pol.trawl.dat$SURF_TEMP)


pol.trawl.dat$logCPUE <- log(pol.trawl.dat$WTCPUE + 1)


world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE), data=pol.trawl.dat) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar")  

x <- mapproject(pol.trawl.dat$LONGITUDE, pol.trawl.dat$LATITUDE, "albers", param=c(55.9, 60.8))
pol.trawl.dat$long_albers <- x$x
pol.trawl.dat$lat_albers <- x$y



#do I need the section 'deal w missing data'?
#going to guess no so that it doesn't fill in NEBS stations for early period

#calculate anomalies and means=====================================================================

#annual 'global' (w/in dataset) mean
poltempmeans <- pol.trawl.dat %>% group_by(STATION) %>% summarize(mean_station_bottemp=mean(BOT_TEMP, na.rm=TRUE))
poltempmeans <- left_join(pol.trawl.dat, poltempmeans )

poltempmeans$bottemp_anom <- poltempmeans$BOT_TEMP - poltempmeans$mean_station_bottemp

all_analysis_dat <- poltempmeans





ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar")  





