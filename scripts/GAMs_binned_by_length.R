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

bin1dat <- binmeta2[which(binmeta2$bin=="0-200"),]
bin2dat <- binmeta2[which(binmeta2$bin=="200-300"),]
bin3dat <- binmeta2[which(binmeta2$bin=="300-400"),]
bin4dat <- binmeta2[which(binmeta2$bin=="400-500"),]
bin5dat <- binmeta2[which(binmeta2$bin=="500+"),]

bin1datM <- bin1dat[which(bin1dat$SEX==1),]
  bin1datF <- bin1dat[which(bin1dat$SEX==2),]
  bin1datUK <- bin1dat[which(bin1dat$SEX==3),]
  
  bin2datM <- bin2dat[which(bin2dat$SEX==1),]
  bin2datF <- bin2dat[which(bin2dat$SEX==2),]
  bin2datUK <- bin2dat[which(bin2dat$SEX==3),]
  
  bin3datM <- bin3dat[which(bin3dat$SEX==1),]
  bin3datF <- bin3dat[which(bin3dat$SEX==2),]
  bin3datUK <- bin3dat[which(bin3dat$SEX==3),]
  
  bin4datM <- bin4dat[which(bin4dat$SEX==1),]
  bin4datF <- bin4dat[which(bin4dat$SEX==2),]
  bin4datUK <- bin4dat[which(bin4dat$SEX==3),]
  
  bin5datM <- bin5dat[which(bin5dat$SEX==1),]
  bin5datF <- bin5dat[which(bin5dat$SEX==2),]
  bin5datUK <- bin5dat[which(bin5dat$SEX==3),]
  
  
  #skip to section 'models without sex'
  
  
  

#first bin======
  #MALE
bmodbase1M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 data=bin1datM)
plot(Variogram(bmodbase1M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin1datM))
#is there any spatial correlation though?
plot(Variogram(bmodbase1M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datM))


bgambase1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                data=bin1datM)

res3 <- residuals(bgambase1M, type = "pearson")
bdat <- bin1datM[is.na(bin1datM$log_sum_WGTCPUE_LEN)==FALSE,]
bdat$residual <- res3

z1 <- ggplot(bdat[which(bdat$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat[which(bdat$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat[which(bdat$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod1M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
              correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
              data=bin1datM)
summary(tmod1M[[1]])
summary(tmod1M[[2]]) #period:bottempanom is NOT non-linear!
plot(tmod1M[[2]])
plot(Variogram(tmod1M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datM))
#saveRDS(tmod1M, file=paste(wd,"/scripts/GAM_output/male1-gaus.rds",sep=""))

tmod1.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin1datM)
summary(tmod1.1M) #period:bottempanom IS non-linear so differs from above
plot(tmod1.1M)

table(bin1datM$YEAR)

tmod1RM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin1datM)
summary(tmod1RM[[1]])
summary(tmod1RM[[2]])
plot(tmod1RM[[2]])
plot(Variogram(tmod1RM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datM))
#saveRDS(tmod1RM, file=paste(wd,"/scripts/GAM_output/male1-ratio.rds",sep=""))

tmod1R.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datM)
summary(tmod1R.1M)
plot(tmod1R.1M)



tmod1EM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin1datM)
plot(Variogram(tmod1EM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datM))
#saveRDS(tmod1EM, file=paste(wd,"/scripts/GAM_output/male1-exp.rds",sep=""))

tmod1E.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datM)
summary(tmod1E.1M)
plot(tmod1E.1M)
visreg(tmod1E.1M, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod1E.1M, "bottemp_anom", "period", ylim=c(-10,5))
visreg(tmod1E.1M, "mean_station_bottemp", "BOT_DEPTH")

AIC(tmod1.1M, tmod1R.1M, tmod1E.1M) #all same, deviance explained all too, anova all same

tmod1E.1M_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin1datM)
summary(tmod1E.1M_lin)
plot(tmod1E.1M_lin)
visreg(tmod1E.1M_lin, "bottemp_anom", "period")

#and drop periods
tmod1E.1M_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin1datM)
summary(tmod1E.1M_drop)
plot(tmod1E.1M_drop)
visreg(tmod1E.1M_drop, "bottemp_anom")
AIC(tmod1E.1M, tmod1E.1M_lin, tmod1E.1M_drop) #gam w period interaction is best



#FEMALE
bmodbase1F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin1datF)
plot(Variogram(bmodbase1F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin1datF))
#is there any spatial correlation though?
plot(Variogram(bmodbase1F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datF))


bgambase1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin1datF)

res3F <- residuals(bgambase1F, type = "pearson")
bdatF <- bin1datF[is.na(bin1datF$log_sum_WGTCPUE_LEN)==FALSE,]
bdatF$residual <- res3F

z1 <- ggplot(bdatF[which(bdatF$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdatF[which(bdatF$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdatF[which(bdatF$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod1F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin1datF)
summary(tmod1F[[1]])
summary(tmod1F[[2]])
plot(tmod1F[[2]])
plot(Variogram(tmod1F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datF))
#saveRDS(tmod1F, file=paste(wd,"/scripts/GAM_output/female1-gaus.rds",sep=""))


tmod1.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datF)
summary(tmod1.1F)
plot(tmod1.1F)



tmod1RF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datF)
summary(tmod1RF[[1]])
summary(tmod1RF[[2]])
plot(tmod1RF[[2]])
plot(Variogram(tmod1RF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datF))
#saveRDS(tmod1RF, file=paste(wd,"/scripts/GAM_output/female1-ratio.rds",sep=""))

tmod1R.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin1datF)
summary(tmod1R.1F)
plot(tmod1R.1F)



tmod1EF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datF)
plot(Variogram(tmod1EF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datF))
#saveRDS(tmod1EF, file=paste(wd,"/scripts/GAM_output/female1-exp.rds",sep=""))

tmod1E.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin1datF)
summary(tmod1E.1F)
plot(tmod1E.1F)
visreg(tmod1E.1F, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod1E.1F, "bottemp_anom", "period", ylim=c(-10,5))
visreg(tmod1E.1F, "mean_station_bottemp", "BOT_DEPTH")

tmod1E.1F_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin1datF)
summary(tmod1E.1F_lin)
plot(tmod1E.1F_lin)
visreg(tmod1E.1F_lin, "bottemp_anom", "period")

#and drop periods
tmod1E.1F_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                         s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                       correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                       data=bin1datF)
summary(tmod1E.1F_drop)
plot(tmod1E.1F_drop)
visreg(tmod1E.1F_drop, "bottemp_anom")
AIC(tmod1E.1F, tmod1E.1F_lin, tmod1E.1F_drop) #GAM w period interaction is best




#UK
bmodbase1UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin1datUK)
plot(Variogram(bmodbase1UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin1datUK))
#is there any spatial correlation though?
plot(Variogram(bmodbase1UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datUK))


bgambase1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin1datUK)

res3UK <- residuals(bgambase1UK, type = "pearson")
bdatUK <- bin1datUK[is.na(bin1datUK$log_sum_WGTCPUE_LEN)==FALSE,]
bdatUK$residual <- res3UK

z1 <- ggplot(bdatUK[which(bdatUK$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdatUK[which(bdatUK$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdatUK[which(bdatUK$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod1UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin1datUK)
summary(tmod1UK[[1]])
summary(tmod1UK[[2]])
plot(tmod1UK[[2]])
plot(Variogram(tmod1UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datUK))
#saveRDS(tmod1UK, file=paste(wd,"/scripts/GAM_output/uk1-gaus.rds",sep=""))


tmod1.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datUK)
summary(tmod1.1UK)
plot(tmod1.1UK)



tmod1RUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datUK)
summary(tmod1RUK[[1]])
summary(tmod1RUK[[2]])
plot(tmod1RUK[[2]])
plot(Variogram(tmod1RUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datUK))
#saveRDS(tmod1RUK, file=paste(wd,"/scripts/GAM_output/uk1-ratio.rds",sep=""))

tmod1R.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin1datUK)
summary(tmod1R.1UK)
plot(tmod1R.1UK)



tmod1EUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin1datUK)
plot(Variogram(tmod1EUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin1datUK))
#saveRDS(tmod1EUK, file=paste(wd,"/scripts/GAM_output/uk1-exp.rds",sep=""))

tmod1E.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin1datUK)
summary(tmod1E.1UK)
plot(tmod1E.1UK)
visreg(tmod1E.1UK, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod1E.1UK, "mean_station_bottemp", "BOT_DEPTH")


tmod1E.1UK_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin1datUK)
summary(tmod1E.1UK_lin)
plot(tmod1E.1UK_lin)
visreg(tmod1E.1UK_lin, "bottemp_anom", "period")

#and drop periods
tmod1E.1UK_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                         s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                       correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                       data=bin1datUK)
summary(tmod1E.1UK_drop)
plot(tmod1E.1UK_drop)
visreg(tmod1E.1UK_drop, "bottemp_anom")
AIC(tmod1E.1UK, tmod1E.1UK_lin, tmod1E.1UK_drop) #gam w period interaction best

b2 <- getViz(tmod1E.1UK_drop)
plot(sm(b2, 1))




#second bin======
#MALE
bmodbase2M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin2datM)
plot(Variogram(bmodbase2M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin2datM))
#is there any spatial correlation though?
plot(Variogram(bmodbase2M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datM))


bgambase2M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin2datM)

res32M <- residuals(bgambase2M, type = "pearson")
bdat2M <- bin2datM[is.na(bin2datM$log_sum_WGTCPUE_LEN)==FALSE,]
bdat2M$residual <- res32M

z1 <- ggplot(bdat2M[which(bdat2M$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat2M[which(bdat2M$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat2M[which(bdat2M$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod2M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin2datM)
summary(tmod2M[[1]])
summary(tmod2M[[2]])
plot(tmod2M[[2]])
plot(Variogram(tmod2M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datM))
#saveRDS(tmod2M, file=paste(wd,"/scripts/GAM_output/male2-gaus.rds",sep=""))


tmod2.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2datM)
summary(tmod2.1M)
plot(tmod2.1M)



tmod2RM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2datM)
summary(tmod2RM[[1]])
summary(tmod2RM[[2]])
plot(tmod2RM[[2]])
plot(Variogram(tmod2RM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datM))
#saveRDS(tmod2RM, file=paste(wd,"/scripts/GAM_output/male2-ratio.rds",sep=""))

tmod2R.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2datM)
summary(tmod2R.1M)
plot(tmod2R.1M)



tmod2EM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2datM)
plot(Variogram(tmod2EM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datM))
#saveRDS(tmod2EM, file=paste(wd,"/scripts/GAM_output/male2-exp.rds",sep=""))

tmod2E.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2datM)
summary(tmod2E.1M)
plot(tmod2E.1M)
visreg(tmod2E.1M, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod2E.1M, "mean_station_bottemp", "BOT_DEPTH")


tmod2E.1M_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin2datM)
summary(tmod2E.1M_lin)
plot(tmod2E.1M_lin)
visreg(tmod2E.1M_lin, "bottemp_anom", "period")

#and drop periods
tmod2E.1M_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin2datM)
summary(tmod2E.1M_drop)
plot(tmod2E.1M_drop)
visreg(tmod2E.1M_drop, "bottemp_anom")
AIC(tmod2E.1M, tmod2E.1M_lin, tmod2E.1M_drop) #gam w period droppped is better by a little



#FEMALE
bmodbase2F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin2datF)
plot(Variogram(bmodbase2F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin2datF))
#is there any spatial correlation though?
plot(Variogram(bmodbase2F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datF))


bgambase2F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin2datF)

res2F <- residuals(bgambase2F, type = "pearson")
bdat2F <- bin2datF[is.na(bin2datF$log_sum_WGTCPUE_LEN)==FALSE,]
bdat2F$residual <- res2F

z1 <- ggplot(bdat2F[which(bdat2F$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat2F[which(bdat2F$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat2F[which(bdat2F$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod2F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin2datF)
summary(tmod2F[[1]])
summary(tmod2F[[2]])
plot(tmod2F[[2]])
plot(Variogram(tmod2F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datF))
#saveRDS(tmod2F, file=paste(wd,"/scripts/GAM_output/female2-gaus.rds",sep=""))


tmod2.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2datF)
summary(tmod2.1F)
plot(tmod2.1F)



tmod2RF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2datF)
summary(tmod2RF[[1]])
summary(tmod2RF[[2]])
plot(tmod2RF[[2]])
plot(Variogram(tmod2RF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datF))
#saveRDS(tmod2RF, file=paste(wd,"/scripts/GAM_output/female2-ratio.rds",sep=""))

tmod2R.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2datF)
summary(tmod2R.1F)
plot(tmod2R.1F)



tmod2EF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2datF)
plot(Variogram(tmod2EF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datF))
#saveRDS(tmod2EF, file=paste(wd,"/scripts/GAM_output/female2-exp.rds",sep=""))

tmod2E.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2datF)
summary(tmod2E.1F)
plot(tmod2E.1F)
visreg(tmod2E.1F, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod2E.1F, "mean_station_bottemp", "BOT_DEPTH")


tmod2E.1F_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin2datF)
summary(tmod2E.1F_lin)
plot(tmod2E.1F_lin)
visreg(tmod2E.1F_lin, "bottemp_anom", "period")

#and drop periods
tmod2E.1F_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin2datF)
summary(tmod2E.1F_drop)
plot(tmod2E.1F_drop)
visreg(tmod2E.1F_drop, "bottemp_anom")
AIC(tmod2E.1F, tmod2E.1F_lin, tmod2E.1F_drop) #gam w periods dropped better by a little



#UK
bmodbase2UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    data=bin2datUK)
plot(Variogram(bmodbase2UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin2datUK))
#is there any spatial correlation though?
plot(Variogram(bmodbase2UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datUK))


bgambase2UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                   data=bin2datUK)

res2UK <- residuals(bgambase2UK, type = "pearson")
bdat2UK <- bin2datUK[is.na(bin2datUK$log_sum_WGTCPUE_LEN)==FALSE,]
bdat2UK$residual <- res2UK

z1 <- ggplot(bdat2UK[which(bdat2UK$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat2UK[which(bdat2UK$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat2UK[which(bdat2UK$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod2UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2datUK)
summary(tmod2UK[[1]])
summary(tmod2UK[[2]])
plot(tmod2UK[[2]])
plot(Variogram(tmod2UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datUK))
#saveRDS(tmod2UK, file=paste(wd,"/scripts/GAM_output/uk2-gaus.rds",sep=""))


tmod2.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2datUK)
summary(tmod2.1UK)
plot(tmod2.1UK)



tmod2RUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2datUK)
summary(tmod2RUK[[1]])
summary(tmod2RUK[[2]])
plot(tmod2RUK[[2]])
plot(Variogram(tmod2RUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datUK))
#saveRDS(tmod2RUK, file=paste(wd,"/scripts/GAM_output/uk2-ratio.rds",sep=""))

tmod2R.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin2datUK)
summary(tmod2R.1UK)
plot(tmod2R.1UK)



tmod2EUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2datUK)
plot(Variogram(tmod2EUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin2datUK))
#saveRDS(tmod2EUK, file=paste(wd,"/scripts/GAM_output/uk2-exp.rds",sep=""))

tmod2E.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin2datUK)
summary(tmod2E.1UK)
plot(tmod2E.1UK)
visreg(tmod2E.1UK, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod2E.1UK, "mean_station_bottemp", "BOT_DEPTH")


tmod2E.1UK_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin2datUK)
summary(tmod2E.1UK_lin)
plot(tmod2E.1UK_lin)
visreg(tmod2E.1UK_lin, "bottemp_anom", "period")

#and drop periods
tmod2E.1UK_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin2datUK)
summary(tmod2E.1UK_drop)
plot(tmod2E.1UK_drop)
visreg(tmod2E.1UK_drop, "bottemp_anom")
AIC(tmod2E.1UK, tmod2E.1UK_lin, tmod2E.1UK_drop) #Linear and gam w dropped period are equivalent, no spatial surface?







#third bin======
#MALE
bmodbase3M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin3datM)
plot(Variogram(bmodbase3M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin3datM))
#is there any spatial correlation though?
plot(Variogram(bmodbase3M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datM))


bgambase3M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin3datM)

res33M <- residuals(bgambase3M, type = "pearson")
bdat3M <- bin3datM[is.na(bin3datM$log_sum_WGTCPUE_LEN)==FALSE,]
bdat3M$residual <- res33M

z1 <- ggplot(bdat3M[which(bdat3M$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat3M[which(bdat3M$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat3M[which(bdat3M$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod3M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin3datM)
summary(tmod3M[[1]])
summary(tmod3M[[2]])
plot(tmod3M[[2]])
plot(Variogram(tmod3M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datM))
#saveRDS(tmod3M, file=paste(wd,"/scripts/GAM_output/male3-gaus.rds",sep=""))


tmod3.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datM)
summary(tmod3.1M)
plot(tmod3.1M)



tmod3RM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datM)
summary(tmod3RM[[1]])
summary(tmod3RM[[2]])
plot(tmod3RM[[2]])
plot(Variogram(tmod3RM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datM))
#saveRDS(tmod3RM, file=paste(wd,"/scripts/GAM_output/male3-ratio.rds",sep=""))

tmod3R.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datM)
summary(tmod3R.1M)
plot(tmod3R.1M)



tmod3EM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datM)
plot(Variogram(tmod3EM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datM))
#saveRDS(tmod3EM, file=paste(wd,"/scripts/GAM_output/male3-exp.rds",sep=""))

tmod3E.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datM)
summary(tmod3E.1M)
plot(tmod3E.1M)
visreg(tmod3E.1M, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod3E.1M, "mean_station_bottemp", "BOT_DEPTH")
visreg(tmod3E.1M, "bottemp_anom", "period")

AIC(tmod3.1M, tmod3R.1M, tmod3E.1M) #all same
anova(tmod3.1M, tmod3R.1M, tmod3E.1M) #all same

#try the periods interaction as linear
tmod3EM_lin <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datM)
summary(tmod3EM_lin[[1]])
summary(tmod3EM_lin[[2]])
plot(tmod3EM_lin[[1]])
plot(tmod3EM_lin[[2]])

tmod3E.1M_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datM)
summary(tmod3E.1M_lin)
plot(tmod3E.1M_lin)
visreg(tmod3E.1M_lin, "bottemp_anom", "period")

#and drop periods
tmod3E.1M_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin3datM)
summary(tmod3E.1M_drop)
plot(tmod3E.1M_drop)
visreg(tmod3E.1M_drop, "bottemp_anom")
AIC(tmod3E.1M, tmod3E.1M_lin, tmod3E.1M_drop) #periods dropped best by a little




#FEMALE
bmodbase3F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin3datF)
plot(Variogram(bmodbase3F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin3datF))
#is there any spatial correlation though?
plot(Variogram(bmodbase3F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datF))


bgambase3F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin3datF)

res3F <- residuals(bgambase3F, type = "pearson")
bdat3F <- bin3datF[is.na(bin3datF$log_sum_WGTCPUE_LEN)==FALSE,]
bdat3F$residual <- res3F

z1 <- ggplot(bdat3F[which(bdat3F$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat3F[which(bdat3F$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat3F[which(bdat3F$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod3F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin3datF)
summary(tmod3F[[1]])
summary(tmod3F[[2]])
plot(tmod3F[[2]])
plot(Variogram(tmod3F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datF))
#saveRDS(tmod3F, file=paste(wd,"/scripts/GAM_output/female3-gaus.rds",sep=""))


tmod3.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datF)
summary(tmod3.1F)
plot(tmod3.1F)



tmod3RF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datF)
summary(tmod3RF[[1]])
summary(tmod3RF[[2]])
plot(tmod3RF[[2]])
plot(Variogram(tmod3RF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datF))
#saveRDS(tmod3RF, file=paste(wd,"/scripts/GAM_output/female3-ratio.rds",sep=""))

tmod3R.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datF)
summary(tmod3R.1F)
plot(tmod3R.1F)



tmod3EF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datF)
plot(Variogram(tmod3EF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datF))
#saveRDS(tmod3EF, file=paste(wd,"/scripts/GAM_output/female3-exp.rds",sep=""))

tmod3E.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datF)
summary(tmod3E.1F)
plot(tmod3E.1F)
visreg(tmod3E.1F, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod3E.1F, "mean_station_bottemp", "BOT_DEPTH")

tmod3E.1F_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin3datF)
summary(tmod3E.1F_lin)
plot(tmod3E.1F_lin)
visreg(tmod3E.1F_lin, "bottemp_anom", "period")

#and drop periods
tmod3E.1F_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin3datF)
summary(tmod3E.1F_drop)
plot(tmod3E.1F_drop)
visreg(tmod3E.1F_drop, "bottemp_anom")
AIC(tmod3E.1F, tmod3E.1F_lin, tmod3E.1F_drop) #gam w period dropped is best by a little







#UK
bmodbase3UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    data=bin3datUK)
plot(Variogram(bmodbase3UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin3datUK))
#is there any spatial correlation though?
plot(Variogram(bmodbase3UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datUK))


bgambase3UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                   data=bin3datUK)

res3UK <- residuals(bgambase3UK, type = "pearson")
bdat3UK <- bin3datUK[is.na(bin3datUK$log_sum_WGTCPUE_LEN)==FALSE,]
bdat3UK$residual <- res3UK

z1 <- ggplot(bdat3UK[which(bdat3UK$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat3UK[which(bdat3UK$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat3UK[which(bdat3UK$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod3UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3datUK)
summary(tmod3UK[[1]])
summary(tmod3UK[[2]])
plot(tmod3UK[[2]])
plot(Variogram(tmod3UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datUK))
#saveRDS(tmod3UK, file=paste(wd,"/scripts/GAM_output/uk3-gaus.rds",sep=""))


tmod3.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datUK)
summary(tmod3.1UK)
plot(tmod3.1UK)



tmod3RUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datUK)
summary(tmod3RUK[[1]])
summary(tmod3RUK[[2]])
plot(tmod3RUK[[2]])
plot(Variogram(tmod3RUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datUK))
#saveRDS(tmod3RUK, file=paste(wd,"/scripts/GAM_output/uk3-ratio.rds",sep=""))

tmod3R.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin3datUK)
summary(tmod3R.1UK)
plot(tmod3R.1UK)



tmod3EUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3datUK)
plot(Variogram(tmod3EUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin3datUK))
#saveRDS(tmod3EUK, file=paste(wd,"/scripts/GAM_output/uk3-exp.rds",sep=""))

tmod3E.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin3datUK)
summary(tmod3E.1UK)
plot(tmod3E.1UK)
visreg(tmod3E.1UK, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod3E.1UK, "mean_station_bottemp", "BOT_DEPTH")

AIC(tmod3E.1UK)






#fourth bin======
#MALE
bmodbase4M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin4datM)
plot(Variogram(bmodbase4M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin4datM))
#is there any spatial correlation though?
plot(Variogram(bmodbase4M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datM))


bgambase4M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin4datM)

res34M <- residuals(bgambase4M, type = "pearson")
bdat4M <- bin4datM[is.na(bin4datM$log_sum_WGTCPUE_LEN)==FALSE,]
bdat4M$residual <- res34M

z1 <- ggplot(bdat4M[which(bdat4M$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat4M[which(bdat4M$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat4M[which(bdat4M$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod4M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin4datM)
summary(tmod4M[[1]])
summary(tmod4M[[2]])
plot(tmod4M[[2]])
plot(Variogram(tmod4M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datM))
#saveRDS(tmod4M, file=paste(wd,"/scripts/GAM_output/male4-gaus.rds",sep=""))


tmod4.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4datM)
summary(tmod4.1M)
plot(tmod4.1M)



tmod4RM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4datM)
summary(tmod4RM[[1]])
summary(tmod4RM[[2]])
plot(tmod4RM[[2]])
plot(Variogram(tmod4RM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datM))
#saveRDS(tmod4RM, file=paste(wd,"/scripts/GAM_output/male4-ratio.rds",sep=""))

tmod4R.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4datM)
summary(tmod4R.1M)
plot(tmod4R.1M)



tmod4EM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4datM)
plot(Variogram(tmod4EM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datM))
#saveRDS(tmod4EM, file=paste(wd,"/scripts/GAM_output/male4-exp.rds",sep=""))

tmod4E.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4datM)
summary(tmod4E.1M)
plot(tmod4E.1M)
visreg(tmod4E.1M, "bottemp_anom", "period") #
visreg(tmod4E.1M, "bottemp_anom", "period", ylim=c(-5,10))
visreg(tmod4E.1M, "mean_station_bottemp", "BOT_DEPTH")


tmod4E.1M_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin4datM)
summary(tmod4E.1M_lin)
plot(tmod4E.1M_lin)
visreg(tmod4E.1M_lin, "bottemp_anom", "period")

#and drop periods
tmod4E.1M_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin4datM)
summary(tmod4E.1M_drop)
plot(tmod4E.1M_drop)
visreg(tmod4E.1M_drop, "bottemp_anom")
AIC(tmod4E.1M, tmod4E.1M_lin, tmod4E.1M_drop) #no period is best






#FEMALE
bmodbase4F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin4datF)
plot(Variogram(bmodbase4F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin4datF))
#is there any spatial correlation though?
plot(Variogram(bmodbase4F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datF))


bgambase4F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin4datF)

res4F <- residuals(bgambase4F, type = "pearson")
bdat4F <- bin4datF[is.na(bin4datF$log_sum_WGTCPUE_LEN)==FALSE,]
bdat4F$residual <- res4F

z1 <- ggplot(bdat4F[which(bdat4F$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat4F[which(bdat4F$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat4F[which(bdat4F$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod4F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin4datF)
summary(tmod4F[[1]])
summary(tmod4F[[2]])
plot(tmod4F[[2]])
plot(Variogram(tmod4F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datF))
#saveRDS(tmod4F, file=paste(wd,"/scripts/GAM_output/female4-gaus.rds",sep=""))


tmod4.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4datF)
summary(tmod4.1F)
plot(tmod4.1F)



tmod4RF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4datF)
summary(tmod4RF[[1]])
summary(tmod4RF[[2]])
plot(tmod4RF[[2]])
plot(Variogram(tmod4RF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datF))
#saveRDS(tmod4RF, file=paste(wd,"/scripts/GAM_output/female4-ratio.rds",sep=""))

tmod4R.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4datF)
summary(tmod4R.1F)
plot(tmod4R.1F)



tmod4EF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4datF)
plot(Variogram(tmod4EF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datF))
#saveRDS(tmod4EF, file=paste(wd,"/scripts/GAM_output/female4-exp.rds",sep=""))

tmod4E.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4datF)
summary(tmod4E.1F)
plot(tmod4E.1F)
visreg(tmod4E.1F, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod4E.1F, "bottemp_anom", "period", ylim=c(-10, 10))
visreg(tmod4E.1F, "mean_station_bottemp", "BOT_DEPTH")

tmod4E.1F_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin4datF)
summary(tmod4E.1F_lin)
plot(tmod4E.1F_lin)
visreg(tmod4E.1F_lin, "bottemp_anom", "period")


#and drop periods
tmod4E.1F_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin4datF)
summary(tmod4E.1F_drop)
plot(tmod4E.1F_drop)
visreg(tmod4E.1F_drop, "bottemp_anom")
AIC(tmod4E.1F, tmod4E.1F_lin, tmod4E.1F_drop) #gam w period interaction is best






#UK
bmodbase4UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    data=bin4datUK)
plot(Variogram(bmodbase4UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin4datUK))
#is there any spatial correlation though?
plot(Variogram(bmodbase4UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datUK))


bgambase4UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                   data=bin4datUK)

res4UK <- residuals(bgambase4UK, type = "pearson")
bdat4UK <- bin4datUK[is.na(bin4datUK$log_sum_WGTCPUE_LEN)==FALSE,]
bdat4UK$residual <- res4UK

z1 <- ggplot(bdat4UK[which(bdat4UK$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat4UK[which(bdat4UK$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat4UK[which(bdat4UK$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod4UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4datUK)
summary(tmod4UK[[1]])
summary(tmod4UK[[2]])
plot(tmod4UK[[2]])
plot(Variogram(tmod4UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datUK))
#saveRDS(tmod4UK, file=paste(wd,"/scripts/GAM_output/uk4-gaus.rds",sep=""))


tmod4.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4datUK)
summary(tmod4.1UK)
plot(tmod4.1UK)



tmod4RUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4datUK)
summary(tmod4RUK[[1]])
summary(tmod4RUK[[2]])
plot(tmod4RUK[[2]])
plot(Variogram(tmod4RUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datUK))
#saveRDS(tmod4RUK, file=paste(wd,"/scripts/GAM_output/uk4-ratio.rds",sep=""))

tmod4R.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin4datUK)
summary(tmod4R.1UK)
plot(tmod4R.1UK)



tmod4EUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4datUK)
plot(Variogram(tmod4EUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin4datUK))
#saveRDS(tmod4EUK, file=paste(wd,"/scripts/GAM_output/uk4-exp.rds",sep=""))

tmod4E.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin4datUK)
summary(tmod4E.1UK)
plot(tmod4E.1UK)
visreg(tmod4E.1UK, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod4E.1UK, "mean_station_bottemp", "BOT_DEPTH")














#fifth bin======
#MALE
bmodbase5M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin5datM)
plot(Variogram(bmodbase5M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin5datM))
#is there any spatial correlation though?
plot(Variogram(bmodbase5M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datM))


bgambase5M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin5datM)

res35M <- residuals(bgambase5M, type = "pearson")
bdat5M <- bin5datM[is.na(bin5datM$log_sum_WGTCPUE_LEN)==FALSE,]
bdat5M$residual <- res35M

z1 <- ggplot(bdat5M[which(bdat5M$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat5M[which(bdat5M$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat5M[which(bdat5M$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod5M <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin5datM)
summary(tmod5M[[1]])
summary(tmod5M[[2]])
plot(tmod5M[[2]])
plot(Variogram(tmod5M$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datM))
#saveRDS(tmod5M, file=paste(wd,"/scripts/GAM_output/male5-gaus.rds",sep=""))


tmod5.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5datM)
summary(tmod5.1M)
plot(tmod5.1M)



tmod5RM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5datM)
summary(tmod5RM[[1]])
summary(tmod5RM[[2]])
plot(tmod5RM[[2]])
plot(Variogram(tmod5RM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datM))
#saveRDS(tmod5RM, file=paste(wd,"/scripts/GAM_output/male5-ratio.rds",sep=""))

tmod5R.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datM)
summary(tmod5R.1M)
plot(tmod5R.1M)



tmod5EM <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5datM)
plot(Variogram(tmod5EM$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datM))
#saveRDS(tmod5EM, file=paste(wd,"/scripts/GAM_output/male5-exp.rds",sep=""))

tmod5E.1M <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datM)
summary(tmod5E.1M)
plot(tmod5E.1M)
visreg(tmod5E.1M, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod5E.1M, "mean_station_bottemp", "BOT_DEPTH")
big2 <- getViz(tmod5E.1M)
plot(sm(big2, 1))

tmod5E.1M_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin5datM)
summary(tmod5E.1M_lin)
plot(tmod5E.1M_lin)
visreg(tmod5E.1M_lin, "bottemp_anom", "period")
big3 <- getViz(tmod5E.1M_lin)
plot(sm(big3, 1))

#and drop periods
tmod5E.1M_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                        s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                      correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                      data=bin5datM)
summary(tmod5E.1M_drop)
plot(tmod5E.1M_drop)
visreg(tmod5E.1M_drop, "bottemp_anom")

big4 <- getViz(tmod5E.1M_drop)
plot(sm(big4, 1))

AIC(tmod5E.1M, tmod5E.1M_lin, tmod5E.1M_drop) #linear model with period interaction is better by just a little than gam w interaction




#FEMALE
bmodbase5F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                   data=bin5datF)
plot(Variogram(bmodbase5F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin5datF))
#is there any spatial correlation though?
plot(Variogram(bmodbase5F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datF))


bgambase5F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  data=bin5datF)

res5F <- residuals(bgambase5F, type = "pearson")
bdat5F <- bin5datF[is.na(bin5datF$log_sum_WGTCPUE_LEN)==FALSE,]
bdat5F$residual <- res5F

z1 <- ggplot(bdat5F[which(bdat5F$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat5F[which(bdat5F$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat5F[which(bdat5F$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod5F <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                 s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
               correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=bin5datF)
summary(tmod5F[[1]])
summary(tmod5F[[2]])
plot(tmod5F[[2]])
plot(Variogram(tmod5F$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datF))
#saveRDS(tmod5F, file=paste(wd,"/scripts/GAM_output/female5-gaus.rds",sep=""))


tmod5.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5datF)
summary(tmod5.1F)
plot(tmod5.1F)



tmod5RF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5datF)
summary(tmod5RF[[1]])
summary(tmod5RF[[2]])
plot(tmod5RF[[2]])
plot(Variogram(tmod5RF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datF))
#saveRDS(tmod5RF, file=paste(wd,"/scripts/GAM_output/female5-ratio.rds",sep=""))

tmod5R.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datF)
summary(tmod5R.1F)
plot(tmod5R.1F)



tmod5EF <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5datF)
plot(Variogram(tmod5EF$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datF))
#saveRDS(tmod5EF, file=paste(wd,"/scripts/GAM_output/female5-exp.rds",sep=""))

tmod5E.1F <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datF)
summary(tmod5E.1F)
plot(tmod5E.1F)
visreg(tmod5E.1F, "bottemp_anom", "period")
visreg(tmod5E.1F, "bottemp_anom", "period", ylim=c(-5,10)) #no data with low anom in late period??
visreg(tmod5E.1F, "mean_station_bottemp", "BOT_DEPTH")

tmod5E.1F_lin <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + ti(mean_station_bottemp, BOT_DEPTH) + s(YEAR_factor, bs="re"), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin5datF)
summary(tmod5E.1F_lin)
plot(tmod5E.1F_lin)
visreg(tmod5E.1F_lin, "bottemp_anom", "period")

#the nonlinear year isn't significant, try linear
tmod5E.1F_lin2 <- gam(log_sum_WGTCPUE_LEN ~ bottemp_anom:period + YEAR_factor + ti(mean_station_bottemp, BOT_DEPTH) , 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin5datF)
summary(tmod5E.1F_lin2)
plot(tmod5E.1F_lin2)
visreg(tmod5E.1F_lin2, "bottemp_anom", "period")
visreg(tmod5E.1F_lin2, "YEAR_factor") #no year effect

#and drop periods
tmod5E.1F_drop <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom) + s(YEAR_factor, bs="re"), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datF)
summary(tmod5E.1F_drop)
plot(tmod5E.1F_drop)
visreg(tmod5E.1F_drop, "bottemp_anom")
AIC(tmod5E.1F, tmod5E.1F_lin, tmod5E.1F_drop, tmod5E.1F_lin2) #periods model better by a bit than the other two

#UK
bmodbase5UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                      s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                    data=bin5datUK)
plot(Variogram(bmodbase5UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, 
               data=bin5datUK))
#is there any spatial correlation though?
plot(Variogram(bmodbase5UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datUK))


bgambase5UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                     s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                   data=bin5datUK)

res5UK <- residuals(bgambase5UK, type = "pearson")
bdat5UK <- bin5datUK[is.na(bin5datUK$log_sum_WGTCPUE_LEN)==FALSE,]
bdat5UK$residual <- res5UK

z1 <- ggplot(bdat5UK[which(bdat5UK$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat5UK[which(bdat5UK$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(bdat5UK[which(bdat5UK$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#yes looks like spatial cor!



tmod5UK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5datUK)
summary(tmod5UK[[1]])
summary(tmod5UK[[2]])
plot(tmod5UK[[2]])
plot(Variogram(tmod5UK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datUK))
#saveRDS(tmod5UK, file=paste(wd,"/scripts/GAM_output/uk5-gaus.rds",sep=""))


tmod5.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                 correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datUK)
summary(tmod5.1UK)
plot(tmod5.1UK)



tmod5RUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datUK)
summary(tmod5RUK[[1]])
summary(tmod5RUK[[2]])
plot(tmod5RUK[[2]])
plot(Variogram(tmod5RUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datUK))
#saveRDS(tmod5RUK, file=paste(wd,"/scripts/GAM_output/uk5-ratio.rds",sep=""))

tmod5R.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin5datUK)
summary(tmod5R.1UK)
plot(tmod5R.1UK)



tmod5EUK <- gamm(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                   s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5datUK)
plot(Variogram(tmod5EUK$lme, form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE, data=bin5datUK))
#saveRDS(tmod5EUK, file=paste(wd,"/scripts/GAM_output/uk5-exp.rds",sep=""))

tmod5E.1UK <- gam(log_sum_WGTCPUE_LEN ~  ti(mean_station_bottemp, BOT_DEPTH) +
                    s(bottemp_anom, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                  correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                  data=bin5datUK)
summary(tmod5E.1UK)
plot(tmod5E.1UK)
visreg(tmod5E.1UK, "bottemp_anom", "period") #no data with low anom in late period??
visreg(tmod5E.1UK, "mean_station_bottemp", "BOT_DEPTH")


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

#models


bigG <- gam(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, bin, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corGaus(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=binmeta2) #hessian not positive definite, k maybe too low
summary(bigG) 
plot(bigG)
visreg(bigG, "bottemp_anom", "bin")
visreg(bigG, "bottemp_anom", "period")

table(bin1datM$YEAR)

bigR <- gam(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
              s(bottemp_anom, bin, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
            correlation = corRatio(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
            data=binmeta2) #hessian not positive definite, k maybe too low
summary(bigR) 
plot(bigR)
visreg(bigR, "bottemp_anom", "bin")
visreg(bigR, "bottemp_anom", "period")


bigE <- gam(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
              s(bottemp_anom, bin, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
            cor = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
            data=binmeta2) #hessian not positive definite, k maybe too low
summary(bigE) 
plot(bigE)
visreg(bigE, "bottemp_anom", "bin")
visreg(bigE, "bottemp_anom", "period")

gam.check(bigE)

big_e <- getViz(bigE)
plot(sm(big_e, 1))
plot(sm(big_e, 2))
plot(sm(big_e, 3))

plot(bigE, select = 2)
plot(bigE, select = 3)


v1 <- visreg(bigE, 'bottemp_anom', by='bin', cond=list(period="early"), layout=c(5,1))
v2 <- visreg(bigE, 'bottemp_anom', by='bin', cond=list(period="late"), layout=c(5,1))

#replot with predicted values instead of partials?


#what does raw data look like?
e1 <- ggplot(binmeta2, aes(bottemp_anom,log_sum_WGTCPUE_LEN, col=bin))
e1 + geom_point() + facet_wrap(~interaction(period, bin)) + geom_smooth()

e1 <- ggplot(binmeta2, aes(bottemp_anom,log_sum_WGTCPUE_LEN))
e1 + geom_point() + facet_wrap(~interaction(bin, period), ncol=5) + geom_smooth()

e2 <- ggplot(binmeta2, aes(bottemp_anom,log_sum_WGTCPUE_LEN))
e2 + geom_point() + facet_wrap(~interaction(bin, period), ncol=5) + geom_smooth(method="lm")

AIC(bigG, bigR, bigE) #all same?

bigEL <- gam(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom:bin:period +ti(mean_station_bottemp, BOT_DEPTH) +
                 s(YEAR_factor, bs="re"), 
            correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
            data=binmeta2)
summary(bigEL) 
plot(bigEL)
visreg(bigEL, "bottemp_anom", "bin")
visreg(bigEL, "bottemp_anom", "period")
plot_model(bigEL, type="int")


bigEdrop <- gam(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
              s(bottemp_anom, bin, bs="fs") + s(YEAR_factor, bs="re"), 
            correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
            data=binmeta2)
summary(bigEdrop) 
plot(bigEdrop)
visreg(bigEdrop, "bottemp_anom", "bin")
visreg(bigEdrop, "bottemp_anom", "period")

big_1 <- getViz(bigEdrop)
plot(sm(big_1, 1))
plot(sm(big_1, 2))
plot(sm(big_1, 3))


bigELdrop <- gam(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom:bin +ti(mean_station_bottemp, BOT_DEPTH) +
               s(YEAR_factor, bs="re"), 
             correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
             data=binmeta2)
summary(bigELdrop) 
plot(bigELdrop)
visreg(bigELdrop, "bottemp_anom", "bin")
visreg(bigELdrop, "bottemp_anom", "period")

gam.check(bigELdrop) #good hessian



bigEdrop2 <- gam(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
                  s(bottemp_anom, by=bin, bs="fs") + s(YEAR_factor, bs="re"), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=binmeta2)
gam.check(bigEdrop2) #bad hessian, k likely too low

AIC(bigE, bigEL, bigEdrop, bigEdrop2, bigELdrop) #full by far the best

bigEL3way <- gam(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period +ti(mean_station_bottemp, BOT_DEPTH) +
               s(YEAR_factor, bs="re"), 
             correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
             data=binmeta2)
summary(bigEL3way) 
plot(bigEL3way)
visreg(bigEL3way, "bottemp_anom", "bin")
visreg(bigEL3way, "bottemp_anom", "period")
plot_model(bigEL3way, type="int")

gam.check(bigEL3way) #good hessian

AIC(bigE, bigEL, bigEdrop, bigEdrop2, bigELdrop, bigEL3way)


#does limiting k remove that spike down?
bigEk <- gam(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
              s(bottemp_anom, bin, by=as.factor(period), bs="fs", k=5) + s(YEAR_factor, bs="re"), 
            correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
            data=binmeta2)
summary(bigEk) 
plot(bigEk)
visreg(bigEk, "bottemp_anom", "bin")
visreg(bigEk, "bottemp_anom", "period")

big_ek <- getViz(bigEk)
plot(sm(big_ek, 1))
plot(sm(big_ek, 2))
plot(sm(big_ek, 3))

plot(bigEk, select = 2)
plot(bigEk, select = 3)


bigEk3 <- gam(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
               s(bottemp_anom, bin, by=as.factor(period), bs="fs", k=3) + s(YEAR_factor, bs="re"), 
             correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
             data=binmeta2)
summary(bigEk3) 
plot(bigEk3)
visreg(bigEk3, "bottemp_anom", "bin")
visreg(bigEk3, "bottemp_anom", "period")

big_ek3 <- getViz(bigEk3)
plot(sm(big_ek3, 1))
plot(sm(big_ek3, 2))
plot(sm(big_ek3, 3))

plot(bigEk3, select = 2)
plot(bigEk3, select = 3)

AIC(bigE, bigEk, bigEk3) #gets worse as k is decreased



#attempt two at big model selection======

binmeta2$bin <- as.factor(binmeta2$bin)

#start full model
full_mm <- gamm(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
              s(bottemp_anom, bin, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
          #  cor = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
            data=binmeta2, method="REML") #
summary(full_mm[[2]]) 
summary(full_mm[[1]]) 
plot(full_mm[[2]])
visreg(full_mm[[2]], "bottemp_anom", "bin")
visreg(full_mm, "bottemp_anom", "period")

#model creates a bunch of NaNs, are there NAs in period?
#no NAs so issue with interaction

gam.check(summary(full_mm[[2]]))

fullviz1 <- getViz(full_mm)
plot(sm(fullviz1 , 1))
plot(sm(fullviz1 , 2))
plot(sm(fullviz1 , 3))

plot(full_mm, select = 2)
plot(full_mm, select = 3)


v1 <- visreg(full_mm, 'bottemp_anom', by='bin', cond=list(period="early"), layout=c(5,1))
v2 <- visreg(full_mm, 'bottemp_anom', by='bin', cond=list(period="late"), layout=c(5,1))



red_2way_mm <- gamm(log_sum_WGTCPUE_LEN ~ bin + ti(mean_station_bottemp, BOT_DEPTH) +
                      s(bin, by=as.factor(period), bs="fs") +
                  s(bottemp_anom, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
                #  cor = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=binmeta2)
summary(red_2way_mm[[2]])
plot(red_2way_mm[[2]])



lin_mm <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                 ti(mean_station_bottemp, BOT_DEPTH),
                  random=list(YEAR_factor=~1), 
                #  cor = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=binmeta2, method="REML") #
summary(lin_mm[[2]]) 
summary(lin_mm[[1]]) 
# AIC        BIC      logLik
# 177722.2  177939.4 -88836.11
plot(lin_mm[[2]])
anova(lin_mm[[2]])


gam.check(summary(lin_mm[[2]]))

linviz1 <- getViz(lin_mm[[2]])
plot(sm(linviz1 , 1))

LE1 <- resid(lin_mm$lme, type="normalized")
LF1 <- fitted(lin_mm$lme)

plot(lin_mm$lme, resid(., type="n") ~ fitted(.), abline = 0, col=1)
plot(lin_mm$lme, resid(., type="n") ~ bottemp_anom, abline = 0, col=1)
plot(lin_mm$lme, resid(., type="n") ~ fitted(.)|bin, abline = 0, col=1, par.strip.text=list(cex=0.75))
plot(lin_mm$lme, resid(., type="n") ~ fitted(.)|period, abline = 0, col=1, par.strip.text=list(cex=0.75))


lin <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                 ti(mean_station_bottemp, BOT_DEPTH),
             #  random=list(YEAR_factor=~1), 
               #  cor = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
               data=binmeta2, method="REML") #
summary(lin[[2]]) 
summary(lin[[1]]) 
# AIC      BIC    logLik
# 178531 178739.6 -89241.52
plot(lin[[2]])
anova(lin[[2]])

anova(lin[[2]], lin_mm[[2]], test="F")

#comparing using AIC with random year is better than without
#no nosig linear terms to drop
#spatial correlation??

library(nlme)

reslinm <- residuals(lin_mm[[2]], type = "pearson")
var <- Variogram(reslinm ~ long_albers + lat_albers,  data=linmdat)    #what dist???
plot(var)
var <- Variogram(lin_mm$lme, form=~long_albers + lat_albers|YEAR_factor, robust=TRUE, smooth=FALSE)    #what dist???
plot(var)
#linmdat <- periods_analysis_dat[is.na(periods_analysis_dat$logCPUE_Gadus_chalcogrammus)==FALSE,]
linmdat <- binmeta2
linmdat$residual <- NA
linmdat$residual <- reslinm
z1 <- ggplot(linmdat[which(linmdat$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(linmdat[which(linmdat$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(linmdat[which(linmdat$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

#yes, certainly looks like spatial autocorrelation

#and fixed portion:
reslinlme <- residuals(lin_mm[[1]], type = "normalized")
#var <- Variogram(reslinm ~ long_albers + lat_albers, data=linmdat)    #what dist???
plot(var)
#linmdat <- periods_analysis_dat[is.na(periods_analysis_dat$logCPUE_Gadus_chalcogrammus)==FALSE,]
#linmdat <- binmeta2
linmdat$residual_lme_norm <- NA
linmdat$residual_lme_norm <- reslinlme
z1 <- ggplot(linmdat[which(linmdat$YEAR==2000),], aes(LONGITUDE, LATITUDE, colour=residual_lme_norm))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(linmdat[which(linmdat$YEAR==2010),], aes(LONGITUDE, LATITUDE, colour=residual_lme_norm))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z1 <- ggplot(linmdat[which(linmdat$YEAR==1990),], aes(LONGITUDE, LATITUDE, colour=residual_lme_norm))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

#also spatial autocorrelation maybe not as bad


#fit correlation structures

#how many repeated lat-longs within a given year?
yr_loc <- binmeta2 %>% group_by(long_albers, lat_albers, YEAR, STATION) %>% summarize(n=n())
#of course, this is the binned data so tonnes of repeats
#add a teeny tiny bit to each coordinate so they aren't identical w/in a year?

binmeta2$adj_long_albers <- NA
binmeta2$adj_lat_albers <- NA

#lat
binmeta2$adj_lat_albers[which(binmeta2$bin=="0-200")] <- binmeta2$lat_albers[which(binmeta2$bin=="0-200")] + 
                                                            0.0000001
binmeta2$adj_lat_albers[which(binmeta2$bin=="200-300")] <- binmeta2$lat_albers[which(binmeta2$bin=="200-300")] - 
  0.0000001
binmeta2$adj_lat_albers[which(binmeta2$bin=="300-400")] <- binmeta2$lat_albers[which(binmeta2$bin=="300-400")] + 
  0.0000002
binmeta2$adj_lat_albers[which(binmeta2$bin=="400-500")] <- binmeta2$lat_albers[which(binmeta2$bin=="400-500")] - 
  0.0000002
binmeta2$adj_lat_albers[which(binmeta2$bin=="500+")] <- binmeta2$lat_albers[which(binmeta2$bin=="500+")] + 
  0.0000003

#long

binmeta2$adj_long_albers[which(binmeta2$bin=="0-200")] <- binmeta2$long_albers[which(binmeta2$bin=="0-200")] + 
  0.00000001
binmeta2$adj_long_albers[which(binmeta2$bin=="200-300")] <- binmeta2$long_albers[which(binmeta2$bin=="200-300")] - 
  0.00000001
binmeta2$adj_long_albers[which(binmeta2$bin=="300-400")] <- binmeta2$long_albers[which(binmeta2$bin=="300-400")] + 
  0.00000002
binmeta2$adj_long_albers[which(binmeta2$bin=="400-500")] <- binmeta2$long_albers[which(binmeta2$bin=="400-500")] - 
  0.00000002
binmeta2$adj_long_albers[which(binmeta2$bin=="500+")] <- binmeta2$long_albers[which(binmeta2$bin=="500+")] + 
  0.00000003

#for correlation structure use adj_ coordinates, which are jittered just a little

lin_mm_gau <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                     ti(mean_station_bottemp, BOT_DEPTH),
                   random=list(YEAR_factor=~1), 
                   cor = corGaus(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),
                   data=binmeta2, method="REML") #
saveRDS(lin_mm_gau, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Gaus-cor_model.RDS")

lin_mm_gau <- readRDS(file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Gaus-cor_model.RDS")


summary(lin_mm_gau[[2]]) 
summary(lin_mm_gau[[1]]) 
# AIC      BIC    logLik
# 171405.6 171640.2 -85675.81

plot(lin_mm_gau[[2]])
anova(lin_mm_gau[[2]])


gam.check(summary(lin_mm_gau[[2]]))

linviz1g <- getViz(lin_mm_gau[[2]])
plot(sm(linviz1g , 1))



lin_mm_exp <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                 ti(mean_station_bottemp, BOT_DEPTH),
               random=list(YEAR_factor=~1), 
                 cor = corExp(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),   #what is nugget again?
               data=binmeta2, method="REML") #start Tues 9:44
saveRDS(lin_mm_exp, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Exp-cor_model.RDS")

#should cor be by year + bin??

lin_mm_exp <- readRDS(file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Exp-cor_model.RDS")

summary(lin_mm_exp[[2]]) 
summary(lin_mm_exp[[1]]) 

# AIC      BIC    logLik
# 167389.5 167624.1 -83667.76     #definite improvement

plot(lin_mm_exp[[2]])
anova(lin_mm_exp[[2]])
summary(lin_mm_exp[[2]])

library(car)
Anova(lin_mm_exp[[2]], type="III")

library(sjPlot)
library(sjmisc)

plot_model(lin_mm_exp[[2]], type="int")
plot_model(lin_mm_exp[[2]], type = "pred", terms = c("bottemp_anom", "bin", "period"))

library(broom)
lin_mm_exp[[1]] %>% augment() %>% 
  ggplot(., aes(x = bottemp_anom,
                y = log_sum_WGTCPUE_LEN,
                col = bin)) +
  geom_smooth(method="lm") +
  facet_grid(. ~ period) +
  theme_classic()


lin_mm_exp[[1]] %>% augment() %>% 
  ggplot(., aes(x = bottemp_anom,
                y = log_sum_WGTCPUE_LEN,
                col = bin)) +
  geom_line(aes(group=bin)) +
  facet_grid(. ~ period) +
  theme_classic()


#hmm plotting interaction looks different based on whether I use the gam or lme portion of the model

gam.check(summary(lin_mm_exp[[2]]))

linviz1e <- getViz(lin_mm_exp[[2]])
plot(sm(linviz1e , 1))

Pred.exp <- predict(lin_mm_exp$gam, binmeta2,
                 type = "response")
bin_w_pred <- binmeta2
bin_w_pred$predicted_exp <- Pred.exp

ggplot(bin_w_pred, aes(bottemp_anom, log_sum_WGTCPUE_LEN, col=period)) + geom_point() + 
  +   geom_smooth(predicted_exp, method="lm")

plot(bin_w_pred$bottemp_anom, bin_w_pred$log_sum_WGTCPUE_LEN)
lines(bin_w_pred$predicted_exp, col=interaction(bin_w_pred$bin, bin_w_pred$period))

ggplot(bin_w_pred, aes(long_albers, lat_albers, col=predicted_exp)) + geom_point() 


#what does the 3-way interaction look like in raw data?
e1 <- ggplot(binmeta2, aes(bottemp_anom, log_sum_WGTCPUE_LEN, colour=period))
e1 + geom_point(alpha=0.2) + geom_smooth(method="lm") + facet_wrap(~bin)



lin_mm_rat <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                     ti(mean_station_bottemp, BOT_DEPTH),
                   random=list(YEAR_factor=~1), 
                   cor = corRatio(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),
                   data=binmeta2, method="REML") #
saveRDS(lin_mm_rat, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Ratio-cor_model.RDS")

summary(lin_mm_rat[[2]]) 
summary(lin_mm_rat[[1]]) 

plot(lin_mm_rat[[2]])
anova(lin_mm_rat[[2]])


gam.check(summary(lin_mm_rat[[2]]))

linviz1r <- getViz(lin_mm_rat[[2]])
plot(sm(linviz1r , 1))



lin_mm_shr <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                     ti(mean_station_bottemp, BOT_DEPTH),
                   random=list(YEAR_factor=~1), 
                   cor = corSpher(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),
                   data=binmeta2, method="REML") #
saveRDS(lin_mm_shr, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_spher-cor_model.RDS")

summary(lin_mm_shr[[2]]) 
summary(lin_mm_shr[[1]]) 

plot(lin_mm_shr[[2]])
anova(lin_mm_shr[[2]])


gam.check(summary(lin_mm_shr[[2]]))

linviz1s <- getViz(lin_mm_shr[[2]])
plot(sm(linviz1s , 1))




AIC(lin_mm[[1]], lin_mm_gau[[1]], lin_mm_exp[[1]], lin_mm_rat[[1]], lin_mm_shr[[1]])
#exp is by far the best

#CPUE by bin spatially?======

s1 <- ggplot(binmeta2[which(binmeta2$bin=="500+"),], aes(lat_albers, long_albers, col=log_sum_WGTCPUE_LEN)) 
  s1 + geom_point(alpha=0.9) + facet_wrap(~YEAR) + scale_color_distiller(palette = "Spectral")

#model on only overlapping temp values======
  
min(binmeta2$bottemp_anom[which(binmeta2$period=="early")])  
  min(binmeta2$bottemp_anom[which(binmeta2$period=="late")])  
  
  max(binmeta2$bottemp_anom[which(binmeta2$period=="early")])  
  max(binmeta2$bottemp_anom[which(binmeta2$period=="late")])  
  
  ggplot(binmeta2, aes(period, bottemp_anom)) + geom_boxplot()
  
  tempsummary <- binmeta2 %>% group_by(period) %>%
    summarize(mean_Btempanom=mean(bottemp_anom, na.rm=TRUE),
              q_05=quantile(bottemp_anom, 0.05, na.rm=TRUE),
              q_95=quantile(bottemp_anom, 0.95, na.rm=TRUE))
  
#analyze only data that falls within the 5th and 95th quantile of both 
overlapdat <- binmeta2[which(binmeta2$bottemp_anom<1.56 & binmeta2$bottemp_anom>-0.595),]

e2 <- ggplot(overlapdat, aes(bottemp_anom, log_sum_WGTCPUE_LEN, colour=period))
e2 + geom_point(alpha=0.2) + geom_smooth(method="lm") + facet_wrap(~bin)


overlap_exp <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                     ti(mean_station_bottemp, BOT_DEPTH),
                   random=list(YEAR_factor=~1), 
                   cor = corExp(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),
                   data=overlapdat, method="REML") #start Tues 4:52pm
saveRDS(overlap_exp, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/overlap_Exp-cor_model.RDS")

summary(overlap_exp[[2]])
anova(overlap_exp[[2]])
plot(overlap_exp[[2]])

summary(overlap_exp[[1]])
anova(overlap_exp[[1]])

plot_model(overlap_exp[[2]], type="int")


overlap_exp[[1]] %>% augment() %>% 
  ggplot(., aes(x = bottemp_anom,
                y = log_sum_WGTCPUE_LEN,
                col = bin)) +
  geom_smooth(method="lm") +
  facet_grid(. ~ period) +
  theme_classic()

