#try plotting depth * mean_station_bottemp for NEBS

#using joinboth from trawl_biomass_GAM_explor.R
#hmm still seems to remove NEBS
#use pol.trawl.dat instead from data_explor_survey.R

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

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_text(aes(LONGITUDE, LATITUDE, 
                         label=STATION), data=all_analysis_dat)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_text(aes(LONGITUDE, LATITUDE, 
                label=STRATUM), data=all_analysis_dat)

post2010 <- all_analysis_dat[which(all_analysis_dat$YEAR>2009),]

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_text(aes(LONGITUDE, LATITUDE, 
                label=STATION), data=post2010[which(post2010$YEAR=="2010"|
                                                      post2010$YEAR=="2017"|
                                                      post2010$YEAR=="2018"|
                                                      post2010$YEAR=="2019"),]) + facet_wrap(~YEAR)


#sort into NEBS and SEBS

all_analysis_dat$region <- "SEBS"
all_analysis_dat$region[which(all_analysis_dat$STRATUM==81 | 
                                all_analysis_dat$STRATUM==70 |
                                all_analysis_dat$STRATUM==71)] <- "NEBS"

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                col=region), data=all_analysis_dat) 

#add shelves 
#based on table 1 in Laurth et al 2019 NOAA Technical Memorandum NMFS-AFSC-396

all_analysis_dat$shelf <- NA
all_analysis_dat$shelf[which(all_analysis_dat$STRATUM==81 | 
                                all_analysis_dat$STRATUM==70 |
                                all_analysis_dat$STRATUM==71)] <- "NEBS"
all_analysis_dat$shelf[which(all_analysis_dat$STRATUM==10 | 
                               all_analysis_dat$STRATUM==20)] <- "EBS_inner"
all_analysis_dat$shelf[which(all_analysis_dat$STRATUM==31 | 
                               all_analysis_dat$STRATUM==32 | 
                               all_analysis_dat$STRATUM==41 | 
                               all_analysis_dat$STRATUM==42 | 
                               all_analysis_dat$STRATUM==43 | 
                               all_analysis_dat$STRATUM==82)] <- "EBS_middle"
all_analysis_dat$shelf[which(all_analysis_dat$STRATUM==50 | 
                               all_analysis_dat$STRATUM==61 | 
                               all_analysis_dat$STRATUM==62 | 
                               all_analysis_dat$STRATUM==90)] <- "EBS_outer"

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=shelf), data=all_analysis_dat) 


#plot temps * depth=================

r1 <- ggplot(all_analysis_dat, aes(mean_station_bottemp, BOT_DEPTH))
r1 + geom_point() + facet_wrap(~region)


r2 <- ggplot(all_analysis_dat, aes(mean_station_bottemp, BOT_DEPTH, col=region))
r2 + geom_point()

r3 <- ggplot(all_analysis_dat, aes(mean_station_bottemp, BOT_DEPTH))
r3 + geom_point() + facet_wrap(~shelf)

r4 <- ggplot(all_analysis_dat, aes(mean_station_bottemp, BOT_DEPTH, col=shelf))
r4 + geom_point() 

