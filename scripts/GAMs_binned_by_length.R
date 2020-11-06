#stats on data binned by length

#using binmeta from join_CPUEdat_w_sizeCPUEdat.R
library(ggplot2)
library(sjPlot)

binmeta$log_sum_WGTCPUE_LEN <- log(binmeta$bin_sum_WGTCPUE_LEN)
binmeta2$log_sum_WGTCPUE_LEN <- log(binmeta2$bin_sum_WGTCPUE_LEN)


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


  
 
#Models without sex=======


binmeta$STATION <- as.factor(binmeta$STATION)
binmeta$VESSEL <- as.factor(binmeta$VESSEL)
binmeta$CRUISE <- as.factor(binmeta$CRUISE)
binmeta$HAUL <- as.factor(binmeta$HAUL)
binmeta$bin <- as.factor(binmeta$bin)

##exclusion criteria===============

binmeta$log_sum_WGTCPUE_LEN <- log(binmeta$bin_sum_WGTCPUE_LEN)
binmeta2$log_sum_WGTCPUE_LEN <- log(binmeta2$bin_sum_WGTCPUE_LEN)

station_bin <- binmeta %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #not going to work, repeated years b/c diff bins

joinstat <- left_join(binmeta, station_bin)

bin_analysis_dat <- joinstat[which(joinstat$n_yrs>5),] #is this too liberal?
table(joinstat$STATION, joinstat$bin)

binmeta2$bin <- as.factor(binmeta2$bin)

z11 <- ggplot(bin_analysis_dat[which(bin_analysis_dat$YEAR==2000),], aes(LONGITUDE, LATITUDE, 
                                                                         colour=log_sum_WGTCPUE_LEN))
z11 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z11 <- ggplot(bin_analysis_dat, aes(LONGITUDE, LATITUDE, 
                                    colour=log_sum_WGTCPUE_LEN))
z11 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

bin1dat <- binmeta2[which(binmeta2$bin=="0-200"),]
bin2dat <- binmeta2[which(binmeta2$bin=="200-300"),]
bin3dat <- binmeta2[which(binmeta2$bin=="300-400"),]
bin4dat <- binmeta2[which(binmeta2$bin=="400-500"),]
bin5dat <- binmeta2[which(binmeta2$bin=="500+"),]


ggplot(binmeta2, aes(bin, log_sum_WGTCPUE_LEN)) + geom_boxplot()

ggplot(binmeta2, aes(bottemp_anom, log_sum_WGTCPUE_LEN, colour=bin)) + geom_point() + geom_smooth()




#model selection======

binmeta2$bin <- as.factor(binmeta2$bin)


