#stats on data binned by length

#using binmeta from join_CPUEdat_w_sizeCPUEdat.R

library(ggplot2)

ex1 <- ggplot(binmeta, aes( YEAR, bin_sum_WGTCPUE_LEN))
ex1 + geom_point() + facet_wrap(~bin, scales="free") #one super high value in 500+ bin
#log??

binmeta$log_sum_WGTCPUE_LEN <- log(binmeta$bin_sum_WGTCPUE_LEN)
binmeta2$log_sum_WGTCPUE_LEN <- log(binmeta2$bin_sum_WGTCPUE_LEN)
ex1.5 <- ggplot(binmeta, aes( YEAR, log_sum_WGTCPUE_LEN))
ex1.5 + geom_point() + facet_wrap(~bin, scales="free")

ex2 <- ggplot(binmeta, aes( BOT_DEPTH, bin_sum_WGTCPUE_LEN))
ex2 + geom_point() + facet_wrap(~bin, scales="free")

ex2.5 <- ggplot(binmeta, aes( BOT_DEPTH, log_sum_WGTCPUE_LEN))
ex2.5 + geom_point() + facet_wrap(~bin, scales="free")

ex3 <- ggplot(binmeta, aes( BOT_TEMP, bin_sum_WGTCPUE_LEN))
ex3 + geom_point() + facet_wrap(~bin, scales="free")

ex3.5 <- ggplot(binmeta, aes( BOT_TEMP, log_sum_WGTCPUE_LEN))
ex3.5 + geom_point() + facet_wrap(~bin, scales="free")

ex4 <- ggplot(binmeta, aes( long_albers, bin_sum_WGTCPUE_LEN))
ex4 + geom_point() + facet_wrap(~bin, scales="free")

ex4.5 <- ggplot(binmeta, aes( long_albers, log_sum_WGTCPUE_LEN))
ex4.5 + geom_point() + facet_wrap(~bin, scales="free")

ex5 <- ggplot(binmeta, aes( lat_albers, bin_sum_WGTCPUE_LEN))
ex5 + geom_point() + facet_wrap(~bin, scales="free")

ex5.5 <- ggplot(binmeta, aes( lat_albers, log_sum_WGTCPUE_LEN))
ex5.5 + geom_point() + facet_wrap(~bin, scales="free")

hist(binmeta$n) #bins with just one fish very common


binmeta$STATION <- as.factor(binmeta$STATION)
binmeta$VESSEL <- as.factor(binmeta$VESSEL)
binmeta$CRUISE <- as.factor(binmeta$CRUISE)
binmeta$HAUL <- as.factor(binmeta$HAUL)
binmeta$bin <- as.factor(binmeta$bin)

##exclusion criteria===============

station_bin <- binmeta %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #not going to work, repeated years b/c diff bins

joinstat <- left_join(binmeta, station_bin)

bin_analysis_dat <- joinstat[which(joinstat$n_yrs>5),] #is this too liberal?
table(joinstat$STATION, joinstat$bin)

z11 <- ggplot(bin_analysis_dat[which(bin_analysis_dat$YEAR==2000),], aes(LONGITUDE, LATITUDE, 
                                           colour=log_sum_WGTCPUE_LEN))
z11 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z11 <- ggplot(bin_analysis_dat, aes(LONGITUDE, LATITUDE, 
                                                                         colour=log_sum_WGTCPUE_LEN))
z11 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")


#need anoms and means for temps too

bmodbase <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 data=binmeta2)
plot(Variogram(tmodbase$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=periods_analysis_dat))
#is there any spatial correlation though?
plot(Variogram(tmodbase$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=periods_analysis_dat))


bgambase <- gam(logCPUE_Gadus_chalcogrammus ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                data=periods_analysis_dat)












