#=============================================================================================
#model fitting for all ages model
#
#by Krista, Oct 2020
#=============================================================================================
#Notes: want to double check model fitting
#=============================================================================================

library(mgcv)
library(ggplot2)

#remotes::install_github("gavinsimpson/gratia")
library(gratia)

#use data previously cleaned 
#periods_analysis_dat is also loaded in trawl_biomass_GAM_explor.R
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)

#data exploration done in trawl_biomass_GAM_explor.R

#now I want to step through model selection process looking beyond AIC

#fixed effects=====

repeatbase <- gamm(logCPUE_Gadus_chalcogrammus ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 data=periods_analysis_dat)
summary(repeatbase[[1]]) #50446.03 50521.24 -25213.01
summary(repeatbase[[2]]) #rsq 0.24
anova(repeatbase[[2]])
plot(repeatbase[[2]])
#is there any spatial correlation though?
plot(Variogram(repeatbase$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=periods_analysis_dat))
plot(Variogram(repeatbase$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=FALSE, data=periods_analysis_dat))
#actually looks fairly minor, but what about plots?

gam.check(repeatbase[[2]]) #does NOT like k
#qq a little curly on ends

visreg(repeatbase, "mean_station_bottemp", "BOT_DEPTH")

viz_rb <- getViz(repeatbase[[2]])
plot(sm(viz_rb , 1))
check.gamViz(viz_rb)

#compare to model w/o ti or w te

repeat_nospatial <- gamm(logCPUE_Gadus_chalcogrammus ~  #ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=periods_analysis_dat)
gam.check(repeat_nospatial[[2]]) #oo v bad
summary(repeat_nospatial[[1]]) #  53967.71 54020.36 -26976.85
summary(repeat_nospatial[[2]]) #rsq 0.03



#OK seems depth/avg temp very important!!

library(maps)
library(mapdata)
library(mapproj)
library(lattice)
require(ggplot2)
require(nlme)

repeat_te <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) +
                           s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                         data=periods_analysis_dat)
gam.check(repeat_te[[2]]) #
summary(repeat_te[[1]]) #  49056.92 49147.18 -24516.46 muhc better than ti
summary(repeat_te[[2]]) #rsq 0.314

plot(repeat_te[[2]])
visreg(repeat_te, "mean_station_bottemp", "BOT_DEPTH")

viz_te <- getViz(repeat_te[[2]])
plot(sm(viz_te , 1))
check.gamViz(viz_te)

te_dat <- periods_analysis_dat[which(is.finite(periods_analysis_dat$BOT_DEPTH)==TRUE&
                                       is.finite(periods_analysis_dat$mean_station_bottemp)==TRUE &
                                       is.finite(periods_analysis_dat$logCPUE_Gadus_chalcogrammus)==TRUE &
                                       is.finite(periods_analysis_dat$bottemp_anom)==TRUE),]
te_dat$r <- NA
te_dat$r <- resid(repeat_te[[2]])   # Extract residuals
j <- te_dat$YEAR == "2010"  # Extract 2010 data only
sp:::bubble(te_dat[which(te_dat$YEAR==2010),], zcol="r")
ggplot(te_dat, aes(long_albers, lat_albers, col=r)) + geom_point() +
  scale_colour_gradient(high="red", low="green") + facet_wrap(~YEAR)
#looks spatially correlated

r <- resid(repeat_te[[2]])[j]   # Extract residuals
# Compute pairwise distances among logations based on distances in 'x'
d <- dist(te_dat[j,c("lat_albers", "long_albers")])
d <- as.vector(d)   # Need to convert to a vector for 'Variogram' function

SemiVar <- Variogram(r, d)
head(SemiVar, 10)  
plot(SemiVar, xlim=c(0,0.1))
bins <- cut(SemiVar$dist, seq(0,0.1, by=0.005))  
plot(bins, SemiVar$variog)

v1 <- Variogram(repeat_te$lme, form=~long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=periods_analysis_dat)
plot(v1)

v2 <- Variogram(repeat_te$lme, form=~long_albers + lat_albers, nugget=TRUE, data=periods_analysis_dat)
plot(v2)

#can I fix low k on depth*meantemp?

repeat_tek1 <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH, k=30) +
                    s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                  data=periods_analysis_dat)
gam.check(repeat_tek1[[2]]) # fine k
summary(repeat_tek1[[1]]) #  48390.62 48480.88 -24183.31
summary(repeat_tek1[[2]]) #rsq 0.361

viz_tek1 <- getViz(repeat_tek1[[2]])
plot(sm(viz_tek1 , 1))
plot(sm(viz_tek1 , 2))
plot(sm(viz_tek1 , 3))
check.gamViz(viz_tek1)


repeat_tek2 <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH, k=33) +
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    data=periods_analysis_dat)
gam.check(repeat_tek2[[2]]) #bad k again?
summary(repeat_tek2[[1]]) #  48389.78 48480.04 -24182.89
summary(repeat_tek2[[2]]) #rsq 0.362

viz_tek2 <- getViz(repeat_tek2[[2]])
plot(sm(viz_tek2 , 1))
plot(sm(viz_tek2 , 2))
plot(sm(viz_tek2 , 3))
check.gamViz(viz_tek2)


repeat_tek3 <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    data=periods_analysis_dat)
gam.check(repeat_tek3[[2]]) # k fine
summary(repeat_tek3[[1]]) #  48394.3 48484.55 -24185.15
summary(repeat_tek3[[2]]) #rsq 0.361

viz_tek3 <- getViz(repeat_tek3[[2]])
plot(sm(viz_tek3 , 1))
plot(sm(viz_tek3 , 2))
plot(sm(viz_tek3 , 3))
check.gamViz(viz_tek3)


library(itsadug)
fvisgam(repeat_t2[[2]], view=c( "mean_station_bottemp", "BOT_DEPTH"))
pvisgam(repeat_t2[[2]], view=c( "mean_station_bottemp", "BOT_DEPTH"))

library(pammtools)
gamtek3 <- repeat_tek3$gam
gg_tensor(gamtek3) #not working

draw(gamtek3, select = 1)
draw(gamtek3, select = 1, dist=0.05)
draw(gamtek3, select = 1, dist=0.01)

appraise(repeat_tek3$gam)

repeat_t2 <- gamm(logCPUE_Gadus_chalcogrammus ~  t2(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                  data=periods_analysis_dat)
gam.check(repeat_t2[[2]])
gam.check(repeat_t2[[2]]) #
summary(repeat_t2[[1]]) #  49057.54 49155.32 -24515.77
summary(repeat_t2[[2]]) #rsq 0.313

plot(repeat_t2[[2]])
visreg(repeat_t2, "mean_station_bottemp", "BOT_DEPTH")

viz_t2 <- getViz(repeat_t2[[2]])
plot(sm(viz_t2 , 1))
check.gamViz(viz_t2)

repeat_t2F <- gamm(logCPUE_Gadus_chalcogrammus ~  t2(mean_station_bottemp, BOT_DEPTH, full=TRUE) +
                    s(bottemp_anom, by=as.factor(period), bs="fs", k=20), random=list(YEAR_factor=~1), 
                  data=periods_analysis_dat, control = lmeControl(msVerbose = TRUE))
gam.check(repeat_t2F[[2]])
gam.check(repeat_t2F[[2]]) #
summary(repeat_t2F[[1]]) #  49064.09 49176.91 -24517.04
summary(repeat_t2F[[2]]) #rsq 0.313

plot(repeat_t2F[[2]])

viz_t2f <- getViz(repeat_t2F[[2]])
plot(sm(viz_t2f , 1))
check.gamViz(viz_t2f)

ggplot(periods_analysis_dat, aes(mean_station_bottemp, BOT_DEPTH, colour=logCPUE_Gadus_chalcogrammus)) +
  geom_point(size=2)+
  scale_color_distiller(palette = "Spectral")

ggplot(periods_analysis_dat, aes(mean_station_bottemp, BOT_DEPTH, col=logCPUE_Gadus_chalcogrammus)) +
  stat_summary2d(aes(z = logCPUE_Gadus_chalcogrammus), bins = 30, fun = mean) + 
  scale_fill_distiller(palette = "Spectral")


ggplot(periods_analysis_dat, aes(LONGITUDE, LATITUDE, col=logCPUE_Gadus_chalcogrammus)) +
   #geom_tile()  #+ geom_point()
  #stat_bin2d( binwidth=c(0.5,0.5))
 # geom_density2d()
stat_summary_hex(aes(z = logCPUE_Gadus_chalcogrammus), bins = 20, fun = mean)
  # scale_colour_gradientn(colours = terrain.colors(10))


ggplot(periods_analysis_dat, aes(LONGITUDE, LATITUDE, col=logCPUE_Gadus_chalcogrammus)) +
  stat_summary2d(aes(z = logCPUE_Gadus_chalcogrammus), bins = 20, fun = mean) + 
  scale_fill_gradientn(colours=terrain.colors(13))

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 63), expand = TRUE)  +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +  
  stat_summary2d(aes(x=LONGITUDE, y=LATITUDE, z = logCPUE_Gadus_chalcogrammus), 
                 data=periods_analysis_dat, bins = 20, fun = mean)  + 
  scale_fill_distiller(palette = "Spectral") + facet_wrap(~period)


ggplot(periods_analysis_dat, aes(long_albers,lat_albers,  colour=logCPUE_Gadus_chalcogrammus)) +
  geom_point() + scale_color_gradientn(colours = terrain.colors(8))


world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 63), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  colour=logCPUE_Gadus_chalcogrammus), data=periods_analysis_dat) +   
  scale_colour_gradient2()


ggplot(periods_analysis_dat, aes(mean_station_bottemp, BOT_DEPTH)) + geom_point()


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 63), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  colour=mean_station_bottemp), data=periods_analysis_dat) +   
  scale_colour_gradient2()


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 63), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +  
  geom_point(aes(LONGITUDE,LATITUDE,  colour=BOT_DEPTH), data=periods_analysis_dat) +   
  scale_colour_gradient2()


repeat_onlydeptth <- gamm(logCPUE_Gadus_chalcogrammus ~  s( BOT_DEPTH)  + s(long_albers, lat_albers) +
                        s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                      data=periods_analysis_dat)
gam.check(repeat_onlydeptth[[2]]) #
summary(repeat_onlydeptth[[1]]) # 48567.34 48657.6 -24271.67 not as good as depth*meantemp w latxlong
summary(repeat_onlydeptth[[2]]) #rsq 0.34

plot(repeat_onlydeptth[[2]])
visreg(repeat_onlydeptth, "lat_albers", "long_albers")

viz_onlydeptth <- getViz(repeat_onlydeptth[[2]])
plot(sm(viz_onlydeptth , 1))
plot(sm(viz_onlydeptth , 2))
plot(sm(viz_onlydeptth , 3))
plot(sm(viz_onlydeptth , 4))
check.gamViz(viz_onlydeptth)


#correlation======

repeat_te_reml <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                  data=periods_analysis_dat, method="REML")
gam.check(repeat_te_reml[[2]]) #
summary(repeat_te_reml[[1]]) #   49050 49140.25 -24513
summary(repeat_te_reml[[2]]) #rsq 0.315

#which is better, lat x long or cor
repeat_cor1 <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_cor1[[2]]) 
summary(repeat_cor1[[1]]) #  44619.52 44724.81 -22295.76
summary(repeat_cor1[[2]]) # 0.299 

plot(repeat_cor1[[2]])
visreg(repeat_cor1, "mean_station_bottemp", "BOT_DEPTH")

viz_cor1 <- getViz(repeat_cor1[[2]])
plot(sm(viz_cor1 , 1)) #
plot(sm(viz_cor1 , 2))
check.gamViz(viz_cor1)

repeat_cor1.1 <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=FALSE),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_cor1.1[[2]]) 
summary(repeat_cor1.1[[1]]) # 44858.68 44956.46 -22416.34 better w nugget
summary(repeat_cor1.1[[2]]) # 0.304

#other cors

repeat_cors <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corSpher(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_cors[[2]]) 
summary(repeat_cors[[1]]) # 44752.72 44858.02 -22362.36
summary(repeat_cors[[2]])

repeat_corg <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_corg[[2]]) 
summary(repeat_corg[[1]]) # 44821.87 44927.16 -22396.93
summary(repeat_corg[[2]])

repeat_corr <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_corr[[2]]) 
summary(repeat_corr[[1]]) # 44651.23 44756.53 -22311.62
summary(repeat_corr[[2]])

viz_corr <- getViz(repeat_corr[[2]])
plot(sm(viz_corr , 1)) #
plot(sm(viz_corr , 2))
plot(sm(viz_corr , 3))
check.gamViz(viz_corr)


#best one, does it need nugget?

repeat_corrnonug <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=FALSE),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_corrnonug[[2]]) 
summary(repeat_corrnonug[[1]]) # 45099.91 45197.68 -22536.96 #not as good
summary(repeat_corrnonug[[2]])


repeat_corl <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corLin(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_corl[[2]]) 
summary(repeat_corl[[1]]) # doesn't converge
summary(repeat_corl[[2]])

repeat_corar1 <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corAR1(form=~ long_albers + lat_albers|YEAR_factor),
                    data=periods_analysis_dat, method="REML")
gam.check(repeat_corar1[[2]]) 
summary(repeat_corar1[[1]]) # 48943.16 49040.93 -24458.58
summary(repeat_corar1[[2]])



#once more w model with best k
repeat_tek3corR <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                      correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=periods_analysis_dat, method="REML")
gam.check(repeat_tek3corR[[2]]) 
summary(repeat_tek3corR[[1]]) #  44081.51 44186.8 -22026.75
summary(repeat_tek3corR[[2]]) #rsq 0.352

gamobj_tek3corR <- repeat_tek3corR$gam

draw(gamobj_tek3corR, select = 1)
draw(gamobj_tek3corR, select = 1, dist=0.05)
draw(gamobj_tek3corR, select = 1, dist=0.01)

appraise(repeat_tek3corR$gam)


anova(repeat_tek3corR[[2]])
plot(repeat_tek3corR[[2]])


#how does bottom depth only w cor but no lat*long compare?

repeat_corD <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_DEPTH) + 
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=periods_analysis_dat)
gam.check(repeat_corD[[2]]) 
summary(repeat_corD[[1]]) # 45075.8 45158.54 -22526.9 not as good without mean station temp
summary(repeat_corD[[2]]) # rsq 0.255 not as good without mean station temp

plot(repeat_corD[[2]])
visreg(repeat_corD, "mean_station_bottemp", "BOT_DEPTH")

viz_corD <- getViz(repeat_cor1[[2]])
plot(sm(viz_corD , 1)) #
plot(sm(viz_corD , 2))
check.gamViz(viz_corD)



#if I select using the model without the interaction w period do I get same answer??
#yes

baseti <- gamm(logCPUE_Gadus_chalcogrammus ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                   data=periods_analysis_dat)
summary(baseti[[1]]) #50551 50611.17 -25267.5
summary(baseti[[2]]) #rsq 0.25
anova(baseti[[2]])
plot(baseti[[2]])


base_te <- gamm(logCPUE_Gadus_chalcogrammus ~  te(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                  data=periods_analysis_dat)
gam.check(base_te[[2]]) #
summary(base_te[[1]]) #   49152.42 49227.63 -24566.21
summary(base_te[[2]]) #rsq 0.32

plot(base_te[[2]])
visreg(base_te, "mean_station_bottemp", "BOT_DEPTH")

viz_bte <- getViz(repeat_te[[2]])
plot(sm(viz_bte , 1))
check.gamViz(viz_bte)


base_t2 <- gamm(logCPUE_Gadus_chalcogrammus ~  t2(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                data=periods_analysis_dat)
gam.check(base_t2[[2]]) #
summary(base_t2[[1]]) #  49153.58 49236.32 -24565.79
summary(base_t2[[2]]) #rsq 0.319

plot(base_t2[[2]])
visreg(base_t2, "mean_station_bottemp", "BOT_DEPTH")

viz_bt2 <- getViz(repeat_t2[[2]])
plot(sm(viz_bt2 , 1))
check.gamViz(viz_bt2)