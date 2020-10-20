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
                 cor = corExp(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),   
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




#drop period interactions from exp model=======

dropP_exp <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin  +
                     ti(mean_station_bottemp, BOT_DEPTH),
                   random=list(YEAR_factor=~1), 
                   cor = corExp(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),  
                   data=binmeta2, method="REML") #start Tues 9:44
saveRDS(dropP_exp, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Exp-cor_model_NOPERIOD.RDS")


summary(dropP_exp[[1]])
summary(dropP_exp[[2]])
anova(dropP_exp[[2]])

AIC(dropP_exp[[1]], lin_mm_exp[[1]]) #WITH period is better but diff # of obs??


#what about without a nugget?

nonug_exp <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*period  +
                     ti(mean_station_bottemp, BOT_DEPTH),
                   random=list(YEAR_factor=~1), 
                   cor = corExp(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=FALSE),  
                   data=binmeta2, method="REML") #start Fri 11:16am
saveRDS(nonug_exp, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Exp-cor_model_nonugget.RDS")

AIC(nonug_exp[[1]], lin_mm_exp[[1]]) #AIC is much worse without nugget




ggplot(binmeta2, aes(bottemp_anom, log_sum_WGTCPUE_LEN, col=bin)) + geom_point() + facet_wrap(~YEAR) + geom_smooth(method="lm")



#playing with breakpoint

dat_brkpnt <- binmeta2

dat_brkpnt$break2011 <- NA
dat_brkpnt$break2011[which(dat_brkpnt$YEAR<2011)]<-"prebreak"
dat_brkpnt$break2011[which(dat_brkpnt$YEAR>2010)]<-"postbreak"

dat_brkpnt$break2012 <- NA
dat_brkpnt$break2012[which(dat_brkpnt$YEAR<2012)]<-"prebreak"
dat_brkpnt$break2012[which(dat_brkpnt$YEAR>2011)]<-"postbreak"

dat_brkpnt$break2013 <- NA
dat_brkpnt$break2013[which(dat_brkpnt$YEAR<2013)]<-"prebreak"
dat_brkpnt$break2013[which(dat_brkpnt$YEAR>2012)]<-"postbreak"

dat_brkpnt$break2010 <- NA
dat_brkpnt$break2010[which(dat_brkpnt$YEAR<2010)]<-"prebreak"
dat_brkpnt$break2010[which(dat_brkpnt$YEAR>2009 )]<-"postbreak"



#2010
exp10 <- gamm(log_sum_WGTCPUE_LEN ~ bin + bottemp_anom*bin*break2010  +
                     ti(mean_station_bottemp, BOT_DEPTH),
                   random=list(YEAR_factor=~1), 
                   cor = corExp(form=~ adj_long_albers + adj_lat_albers|YEAR_factor, nugget=TRUE),  
                   data=dat_brkpnt, method="REML") #start Mon 4:15
saveRDS(exp10, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linear-mixed_Exp-cor_model_2010break.RDS")



