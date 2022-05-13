#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Load, clean, explore 2021 data and 1982-2019 online NEBS csv  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#found out that the publicly available 1982-2019 csv for the NEBS has more years
#and has 2018 stations (and 80s-90s stations?) automatically renamed to match standard stations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")
library(ggplot2)


#load data-------

wd <- getwd()
nebs2021dat <- read.csv(paste(wd,"/data/survey data/GAP_BottomTrawl_SurveyData_2021/NBS2021.csv",sep=""))
sebs2021dat <- read.csv(paste(wd,"/data/survey data/GAP_BottomTrawl_SurveyData_2021/EBS2021.csv",sep=""))

nebs82_19 <- read.csv(paste(wd,"/data/survey data/nbs1982_2019.csv",sep=""))

#all same column names which is nice

newdat <- rbind(nebs2021dat, sebs2021dat, nebs82_19)

#will limit to SID 21740 but not yet

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=newdat) + theme_bw() 

#look at each year
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=newdat) + theme_bw() + facet_wrap(~YEAR)

#look at only early year
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=newdat[which(newdat$YEAR<1992),]) + theme_bw() + facet_wrap(~YEAR)

#1985 and 1991 stations need stratum assigned, 1988 is missing 1 or 2 stratums
#Norton Sound stations are 0 in 1982, no stratum otherwise, assign?

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(60, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_text(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM),
                label=STATION), data=newdat[which(newdat$YEAR<1992),]) + theme_bw() + facet_wrap(~YEAR)


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(60, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_text(aes(LONGITUDE, LATITUDE, 
                col=as.factor(STRATUM),
                label=STATION), data=newdat[which(newdat$YEAR==2018),]) + theme_bw() + facet_wrap(~YEAR)






