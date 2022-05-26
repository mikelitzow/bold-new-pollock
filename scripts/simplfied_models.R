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

#fill in missing temps-----

tempmod82 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=sebs_pollock[which(sebs_pollock$YEAR==1982),] )
summary(tempmod82)
gam.check(tempmod82)
plot(tempmod82)

pre82 <- predict.gam(tempmod82, type="response")
df82 <- sebs_pollock[which(sebs_pollock$YEAR==1982),] 

missing82 <- df82[which(is.na(df82$BOT_TEMP)==TRUE),]
pred82 <- predict.gam(tempmod82, newdata=missing82, type="response")
missing82$BOT_TEMP <- pred82

tempmod83 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=sebs_pollock[which(sebs_pollock$YEAR==1983),] )
summary(tempmod83)
gam.check(tempmod83)
plot(tempmod83)

pre83 <- predict.gam(tempmod83, type="response")
df83 <- sebs_pollock[which(sebs_pollock$YEAR==1983),] 

missing83 <- df83[which(is.na(df83$BOT_TEMP)==TRUE),]
pred83 <- predict.gam(tempmod83, newdata=missing83, type="response")
missing83$BOT_TEMP <- pred83


tempmod84 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=sebs_pollock[which(sebs_pollock$YEAR==1984),] )
summary(tempmod84)
gam.check(tempmod84)
plot(tempmod84)

pre84 <- predict.gam(tempmod84, type="response")
df84 <- sebs_pollock[which(sebs_pollock$YEAR==1984),] 

missing84 <- df84[which(is.na(df84$BOT_TEMP)==TRUE),]
pred84 <- predict.gam(tempmod84, newdata=missing84, type="response")
missing84$BOT_TEMP <- pred84


tempmod85 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1985),] )
summary(tempmod85)
gam.check(tempmod85)
plot(tempmod85)

pre85 <- predict.gam(tempmod85, type="response")
df85 <- sebs_pollock[which(sebs_pollock$YEAR==1985),] 

missing85 <- df85[which(is.na(df85$BOT_TEMP)==TRUE),]
pred85 <- predict.gam(tempmod85, newdata=missing85, type="response")
missing85$BOT_TEMP <- pred85


tempmod86 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1986),] )
summary(tempmod86)
gam.check(tempmod86)
plot(tempmod86)

pre86 <- predict.gam(tempmod86, type="response")
df86 <- sebs_pollock[which(sebs_pollock$YEAR==1986),] 

missing86 <- df86[which(is.na(df86$BOT_TEMP)==TRUE),]
pred86 <- predict.gam(tempmod86, newdata=missing86, type="response")
missing86$BOT_TEMP <- pred86


tempmod87 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=sebs_pollock[which(sebs_pollock$YEAR==1987),] )
summary(tempmod87)
gam.check(tempmod87)
plot(tempmod87)

pre87 <- predict.gam(tempmod87, type="response")
df87 <- sebs_pollock[which(sebs_pollock$YEAR==1987),] 

missing87 <- df87[which(is.na(df87$BOT_TEMP)==TRUE),]
pred87 <- predict.gam(tempmod87, newdata=missing87, type="response")
missing87$BOT_TEMP <- pred87



tempmod88 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=sebs_pollock[which(sebs_pollock$YEAR==1988),] )
summary(tempmod88)
gam.check(tempmod88)
plot(tempmod88)

pre88 <- predict.gam(tempmod88, type="response")
df88 <- sebs_pollock[which(sebs_pollock$YEAR==1988),] 

missing88 <- df88[which(is.na(df88$BOT_TEMP)==TRUE),]
pred88 <- predict.gam(tempmod88, newdata=missing88, type="response")
missing88$BOT_TEMP <- pred88



tempmod89 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1989),] )
summary(tempmod89)
gam.check(tempmod89)
plot(tempmod89)

pre89 <- predict.gam(tempmod89, type="response")
df89 <- sebs_pollock[which(sebs_pollock$YEAR==1989),] 

missing89 <- df89[which(is.na(df89$BOT_TEMP)==TRUE),]
pred89 <- predict.gam(tempmod89, newdata=missing89, type="response")
missing89$BOT_TEMP <- pred89



tempmod90 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1990),] )
summary(tempmod90)
gam.check(tempmod90)
plot(tempmod90)

pre90 <- predict.gam(tempmod90, type="response")
df90 <- sebs_pollock[which(sebs_pollock$YEAR==1990),] 

missing90 <- df90[which(is.na(df90$BOT_TEMP)==TRUE),]
pred90 <- predict.gam(tempmod90, newdata=missing90, type="response")
missing90$BOT_TEMP <- pred90



tempmod91 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1991),]  )
summary(tempmod91)
gam.check(tempmod91)
plot(tempmod91)

pre91 <- predict.gam(tempmod91, type="response")
df91 <- sebs_pollock[which(sebs_pollock$YEAR==1991),] 

missing91 <- df91[which(is.na(df91$BOT_TEMP)==TRUE),]
pred91 <- predict.gam(tempmod91, newdata=missing91, type="response")
missing91$BOT_TEMP <- pred91



tempmod92 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1992),]  )
summary(tempmod92)
gam.check(tempmod92)
plot(tempmod92)

pre92 <- predict.gam(tempmod92, type="response")
df92 <- sebs_pollock[which(sebs_pollock$YEAR==1992),] 

missing92 <- df92[which(is.na(df92$BOT_TEMP)==TRUE),]
pred92 <- predict.gam(tempmod92, newdata=missing92, type="response")
missing92$BOT_TEMP <- pred92



tempmod93 <- gam(BOT_TEMP ~ s(BOT_DEPTH, k=17) + ti(long_albers, lat_albers, k=8), data=sebs_pollock[which(sebs_pollock$YEAR==1993),]  )
summary(tempmod93)
gam.check(tempmod93) #seems awfully high
plot(tempmod93) 

pre93 <- predict.gam(tempmod93, type="response")
df93 <- sebs_pollock[which(sebs_pollock$YEAR==1993),] 

missing93 <- df93[which(is.na(df93$BOT_TEMP)==TRUE),]
pred93 <- predict.gam(tempmod93, newdata=missing93, type="response")
missing93$BOT_TEMP <- pred93



tempmod94 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=7), data=sebs_pollock[which(sebs_pollock$YEAR==1994),]  )
summary(tempmod94)
gam.check(tempmod94)
plot(tempmod94)

pre94 <- predict.gam(tempmod94, type="response")
df94 <- sebs_pollock[which(sebs_pollock$YEAR==1994),] 

missing94 <- df94[which(is.na(df94$BOT_TEMP)==TRUE),]
pred94 <- predict.gam(tempmod94, newdata=missing94, type="response")
missing94$BOT_TEMP <- pred94



tempmod95 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1995),]  )
summary(tempmod95)
gam.check(tempmod95)
plot(tempmod95)

pre95 <- predict.gam(tempmod95, type="response")
df95 <- sebs_pollock[which(sebs_pollock$YEAR==1995),] 

missing95 <- df95[which(is.na(df95$BOT_TEMP)==TRUE),]
pred95 <- predict.gam(tempmod95, newdata=missing95, type="response")
missing95$BOT_TEMP <- pred95



tempmod96 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1996),]  )
summary(tempmod96)
gam.check(tempmod96)
plot(tempmod96)

pre96 <- predict.gam(tempmod96, type="response")
df96 <- sebs_pollock[which(sebs_pollock$YEAR==1996),] 

missing96 <- df96[which(is.na(df96$BOT_TEMP)==TRUE),]
pred96 <- predict.gam(tempmod96, newdata=missing96, type="response")
missing96$BOT_TEMP <- pred96



tempmod97 <- gam(BOT_TEMP ~ s(BOT_DEPTH, k=8) + ti(long_albers, lat_albers, k=11), data=sebs_pollock[which(sebs_pollock$YEAR==1997),]  )
summary(tempmod97)
gam.check(tempmod97)
plot(tempmod97)

pre97 <- predict.gam(tempmod97, type="response")
df97 <- sebs_pollock[which(sebs_pollock$YEAR==1997),] 

missing97 <- df97[which(is.na(df97$BOT_TEMP)==TRUE),]
pred97 <- predict.gam(tempmod97, newdata=missing97, type="response")
missing97$BOT_TEMP <- pred97



tempmod98 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1998),]  )
summary(tempmod98)
gam.check(tempmod98)
plot(tempmod98)

pre98 <- predict.gam(tempmod98, type="response")
df98 <- sebs_pollock[which(sebs_pollock$YEAR==1998),] 

missing98 <- df98[which(is.na(df98$BOT_TEMP)==TRUE),]
pred98 <- predict.gam(tempmod98, newdata=missing98, type="response")
missing98$BOT_TEMP <- pred98



tempmod99 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==1999),]  )
summary(tempmod99)
gam.check(tempmod99)
plot(tempmod99)

pre99 <- predict.gam(tempmod99, type="response")
df99 <- sebs_pollock[which(sebs_pollock$YEAR==1999),] 

missing99 <- df99[which(is.na(df99$BOT_TEMP)==TRUE),]
pred99 <- predict.gam(tempmod99, newdata=missing99, type="response")
missing99$BOT_TEMP <- pred99



tempmod00 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=7), data=sebs_pollock[which(sebs_pollock$YEAR==2000),]  )
summary(tempmod00)
gam.check(tempmod00)
plot(tempmod00)

pre00 <- predict.gam(tempmod00, type="response")
df00 <- sebs_pollock[which(sebs_pollock$YEAR==2000),] 

missing00 <- df00[which(is.na(df00$BOT_TEMP)==TRUE),]
pred00 <- predict.gam(tempmod00, newdata=missing00, type="response")
missing00$BOT_TEMP <- pred00



tempmod01 <- gam(BOT_TEMP ~ s(BOT_DEPTH, k=16) + ti(long_albers, lat_albers, k=11), data=sebs_pollock[which(sebs_pollock$YEAR==2001),]  )
summary(tempmod01)
gam.check(tempmod01) #still a little low but close enough
plot(tempmod01)

pre01 <- predict.gam(tempmod01, type="response")
df01 <- sebs_pollock[which(sebs_pollock$YEAR==2001),] 

missing01 <- df01[which(is.na(df01$BOT_TEMP)==TRUE),]
pred01 <- predict.gam(tempmod01, newdata=missing01, type="response")
missing01$BOT_TEMP <- pred01



tempmod02 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=sebs_pollock[which(sebs_pollock$YEAR==2002),]  )
summary(tempmod02)
gam.check(tempmod02)
plot(tempmod02)

pre02 <- predict.gam(tempmod02, type="response")
df02 <- sebs_pollock[which(sebs_pollock$YEAR==2002),] 

missing02 <- df02[which(is.na(df02$BOT_TEMP)==TRUE),]
pred02 <- predict.gam(tempmod02, newdata=missing02, type="response")
missing02$BOT_TEMP <- pred02



tempmod03 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=10), data=sebs_pollock[which(sebs_pollock$YEAR==2003),]  )
summary(tempmod03)
gam.check(tempmod03)
plot(tempmod03)

pre03 <- predict.gam(tempmod03, type="response")
df03 <- sebs_pollock[which(sebs_pollock$YEAR==2003),] 

missing03 <- df03[which(is.na(df03$BOT_TEMP)==TRUE),]
pred03 <- predict.gam(tempmod03, newdata=missing03, type="response")
missing03$BOT_TEMP <- pred03



tempmod04 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2004),]  )
summary(tempmod04)
gam.check(tempmod04)
plot(tempmod04)

pre04 <- predict.gam(tempmod04, type="response")
df04 <- sebs_pollock[which(sebs_pollock$YEAR==2004),] 

missing04 <- df04[which(is.na(df04$BOT_TEMP)==TRUE),]
pred04 <- predict.gam(tempmod04, newdata=missing04, type="response")
missing04$BOT_TEMP <- pred04



tempmod05 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2005),]  )
summary(tempmod05)
gam.check(tempmod05)
plot(tempmod05)

pre05 <- predict.gam(tempmod05, type="response")
df05 <- sebs_pollock[which(sebs_pollock$YEAR==2005),] 

missing05 <- df05[which(is.na(df05$BOT_TEMP)==TRUE),]
pred05 <- predict.gam(tempmod05, newdata=missing05, type="response")
missing05$BOT_TEMP <- pred05



tempmod06 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=10), data=sebs_pollock[which(sebs_pollock$YEAR==2006),]  )
summary(tempmod06)
gam.check(tempmod06)
plot(tempmod06)

pre06 <- predict.gam(tempmod06, type="response")
df06 <- sebs_pollock[which(sebs_pollock$YEAR==2006),] 

missing06 <- df06[which(is.na(df06$BOT_TEMP)==TRUE),]
pred06 <- predict.gam(tempmod06, newdata=missing06, type="response")
missing06$BOT_TEMP <- pred06



tempmod07 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2007),]  )
summary(tempmod07)
gam.check(tempmod07)
plot(tempmod07)

pre07 <- predict.gam(tempmod07, type="response")
df07 <- sebs_pollock[which(sebs_pollock$YEAR==2007),] 

missing07 <- df07[which(is.na(df07$BOT_TEMP)==TRUE),]
pred07 <- predict.gam(tempmod07, newdata=missing07, type="response")
missing07$BOT_TEMP <- pred07



tempmod08 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2008),]  )
summary(tempmod08)
gam.check(tempmod08)
plot(tempmod08)

pre08 <- predict.gam(tempmod08, type="response")
df08 <- sebs_pollock[which(sebs_pollock$YEAR==2008),] 

missing08 <- df08[which(is.na(df08$BOT_TEMP)==TRUE),]
pred08 <- predict.gam(tempmod08, newdata=missing08, type="response")
missing08$BOT_TEMP <- pred08



tempmod09 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=10), data=sebs_pollock[which(sebs_pollock$YEAR==2009),]  )
summary(tempmod09)
gam.check(tempmod09)
plot(tempmod09)

pre09 <- predict.gam(tempmod09, type="response")
df09 <- sebs_pollock[which(sebs_pollock$YEAR==2009),] 

missing09 <- df09[which(is.na(df09$BOT_TEMP)==TRUE),]
pred09 <- predict.gam(tempmod09, newdata=missing09, type="response")
missing09$BOT_TEMP <- pred09



tempmod10 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=10), data=sebs_pollock[which(sebs_pollock$YEAR==2010),]  )
summary(tempmod10)
gam.check(tempmod10)
plot(tempmod10)

pre10 <- predict.gam(tempmod10, type="response")
df10 <- sebs_pollock[which(sebs_pollock$YEAR==2010),] 

missing10 <- df10[which(is.na(df10$BOT_TEMP)==TRUE),]
pred10 <- predict.gam(tempmod10, newdata=missing10, type="response")
missing10$BOT_TEMP <- pred10



tempmod11 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=10), data=sebs_pollock[which(sebs_pollock$YEAR==2011),] )
summary(tempmod11)
gam.check(tempmod11)
plot(tempmod11)

pre11 <- predict.gam(tempmod11, type="response")
df11 <- sebs_pollock[which(sebs_pollock$YEAR==2011),] 

missing11 <- df11[which(is.na(df11$BOT_TEMP)==TRUE),]
pred11 <- predict.gam(tempmod11, newdata=missing11, type="response")
missing11$BOT_TEMP <- pred11



tempmod12 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=11), data=sebs_pollock[which(sebs_pollock$YEAR==2012),] )
summary(tempmod12)
gam.check(tempmod12)
plot(tempmod12)

pre12 <- predict.gam(tempmod12, type="response")
df12 <- sebs_pollock[which(sebs_pollock$YEAR==2012),] 

missing12 <- df12[which(is.na(df12$BOT_TEMP)==TRUE),]
pred12 <- predict.gam(tempmod12, newdata=missing12, type="response")
missing12$BOT_TEMP <- pred12



tempmod13 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2013),] )
summary(tempmod13)
gam.check(tempmod13)
plot(tempmod13)

pre13 <- predict.gam(tempmod13, type="response")
df13 <- sebs_pollock[which(sebs_pollock$YEAR==2013),] 

missing13 <- df13[which(is.na(df13$BOT_TEMP)==TRUE),]
pred13 <- predict.gam(tempmod13, newdata=missing13, type="response")
missing13$BOT_TEMP <- pred13



tempmod14 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2014),] )
summary(tempmod14)
gam.check(tempmod14)
plot(tempmod14)

pre14 <- predict.gam(tempmod14, type="response")
df14 <- sebs_pollock[which(sebs_pollock$YEAR==2014),] 

missing14 <- df14[which(is.na(df14$BOT_TEMP)==TRUE),]
pred14 <- predict.gam(tempmod14, newdata=missing14, type="response")
missing14$BOT_TEMP <- pred14



tempmod15 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=10), data=sebs_pollock[which(sebs_pollock$YEAR==2015),] )
summary(tempmod15)
gam.check(tempmod15)
plot(tempmod15)

pre15 <- predict.gam(tempmod15, type="response")
df15 <- sebs_pollock[which(sebs_pollock$YEAR==2015),] 

missing15 <- df15[which(is.na(df15$BOT_TEMP)==TRUE),]
pred15 <- predict.gam(tempmod15, newdata=missing15, type="response")
missing15$BOT_TEMP <- pred15



tempmod16 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2016),] )
summary(tempmod16)
gam.check(tempmod16)
plot(tempmod16)

pre16 <- predict.gam(tempmod16, type="response")
df16 <- sebs_pollock[which(sebs_pollock$YEAR==2016),] 

missing16 <- df16[which(is.na(df16$BOT_TEMP)==TRUE),]
pred16 <- predict.gam(tempmod16, newdata=missing16, type="response")
missing16$BOT_TEMP <- pred16



tempmod17 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2017),] )
summary(tempmod17)
gam.check(tempmod17)
plot(tempmod17)

pre17 <- predict.gam(tempmod17, type="response")
df17 <- sebs_pollock[which(sebs_pollock$YEAR==2017),] 

missing17 <- df17[which(is.na(df17$BOT_TEMP)==TRUE),]
pred17 <- predict.gam(tempmod17, newdata=missing17, type="response")
missing17$BOT_TEMP <- pred17



tempmod18 <- gam(BOT_TEMP ~ s(BOT_DEPTH, k=16) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2018),] )
summary(tempmod18)
gam.check(tempmod18) #still a little low on depth but close enough
plot(tempmod18)

pre18 <- predict.gam(tempmod18, type="response")
df18 <- sebs_pollock[which(sebs_pollock$YEAR==2018),] 

missing18 <- df18[which(is.na(df18$BOT_TEMP)==TRUE),]
pred18 <- predict.gam(tempmod18, newdata=missing18, type="response")
missing18$BOT_TEMP <- pred18



tempmod19 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=sebs_pollock[which(sebs_pollock$YEAR==2019),] )
summary(tempmod19)
gam.check(tempmod19)
plot(tempmod19)

pre19 <- predict.gam(tempmod19, type="response")
df19 <- sebs_pollock[which(sebs_pollock$YEAR==2019),] 

missing19 <- df19[which(is.na(df19$BOT_TEMP)==TRUE),]
pred19 <- predict.gam(tempmod19, newdata=missing19, type="response")
missing19$BOT_TEMP <- pred19



#now bind together

missingall <- rbind(missing82, missing83, missing84, missing85, missing86, missing87, missing88, missing89,
                    missing90, missing91, missing92, missing93, missing94, missing95,
                    missing96, missing97, missing98, missing99, missing00,
                    missing01, missing02, missing03, missing04, missing05,
                    missing06, missing07, missing08, missing09, missing10, 
                    missing11, missing12, missing13, missing14, missing15,
                    missing16, missing17, missing18, missing19)


nona <- sebs_pollock[which(is.na(sebs_pollock$BOT_TEMP)==FALSE),]

sebs_model_dat <- rbind(nona, missingall)



#models------


startmod <- gamm4(logCPUE ~  s(BOT_DEPTH) +
               s(BOT_TEMP, by=period, bs="fs"),  random=~(1|YEAR/HAUL), 
             data=sebs_pollock)
gam.check(startmod$gam)
summary(startmod[[1]]) 
summary(startmod[[2]]) 
anova(startmod[[2]])
plot(startmod[[2]])
#is there any spatial correlation though?
plot(Variogram(startmod$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=sebs_pollock))
plot(Variogram(startmod$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=FALSE, data=sebs_pollock))
#actually looks fairly minor, but what about plots?

c_dat <- sebs_pollock[which(is.finite(sebs_pollock$BOT_DEPTH)==TRUE&
                           is.finite(sebs_pollock$BOT_TEMP)==TRUE &
                           is.finite(sebs_pollock$logCPUE)==TRUE ),]
c_dat$r <- NA
c_dat$r <- resid(startmod[[2]])   # Extract residuals
sp:::bubble(c_dat[which(c_dat$YEAR==2010),], zcol="r")
ggplot(c_dat, aes(long_albers, lat_albers, col=r)) + geom_point() +
  scale_colour_gradient(high="red", low="green") + facet_wrap(~YEAR)
#looks spatially correlated

gam.check(startmod[[2]]) 

cmod1 <- gamm4(logCPUE ~  s(BOT_DEPTH) +
                    s(BOT_TEMP, by=period, bs="fs"),  random=~(1|YEAR/HAUL), correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=sebs_pollock)











