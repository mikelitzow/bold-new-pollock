#stats on data binned by length

#using binmeta from join_CPUEdat_w_sizeCPUEdat.R
library(ggplot2)
library(sjPlot)
library(tidyverse)
library(mgcv)
library(visreg)
library(mgcViz)

wd <- getwd()
binmeta_clean_anom<- read.csv(file=paste(wd,"/data/clean_binned_anom_data.csv", sep=""), row.names = 1)



binmeta_clean_anom$log_sum_WGTCPUE_LEN <- log(binmeta_clean_anom$sum_wgtCPUE_len + 1)


binmeta_clean_anom$STATION <- as.factor(binmeta_clean_anom$STATION)
binmeta_clean_anom$VESSEL <- as.factor(binmeta_clean_anom$VESSEL)
binmeta_clean_anom$CRUISE <- as.factor(binmeta_clean_anom$CRUISE)
binmeta_clean_anom$HAUL <- as.factor(binmeta_clean_anom$HAUL)
binmeta_clean_anom$bin <- as.factor(binmeta_clean_anom$bin)

##exclusion criteria===============

#how to exclude stations w few obs? Maybe n in a single bin? yeah do once split out


bin1dat <- binmeta_clean_anom[which(binmeta_clean_anom$bin=="0-200"),]
bin2dat <- binmeta_clean_anom[which(binmeta_clean_anom$bin=="200-300"),]
bin3dat <- binmeta_clean_anom[which(binmeta_clean_anom$bin=="300-400"),]
bin4dat <- binmeta_clean_anom[which(binmeta_clean_anom$bin=="400-500"),]
bin5dat <- binmeta_clean_anom[which(binmeta_clean_anom$bin=="500+"),]

#exclude by bin

#bin 1
station_bin1 <- bin1dat %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #

join1stat <- left_join(bin1dat, station_bin1)

bin1_analysis_dat <- join1stat[which(join1stat$n_yrs>5),] #none even close to 5


#bin 2
station_bin2 <- bin2dat %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #

join2stat <- left_join(bin2dat, station_bin2)

bin2_analysis_dat <- join2stat[which(join2stat$n_yrs>5),] #none even close to 5


#bin 3
station_bin3 <- bin3dat %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #

join3stat <- left_join(bin3dat, station_bin3)

bin3_analysis_dat <- join3stat[which(join3stat$n_yrs>5),] #none even close to 5


#bin 4
station_bin4 <- bin4dat %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #

join4stat <- left_join(bin4dat, station_bin4)

bin4_analysis_dat <- join4stat[which(join4stat$n_yrs>5),] #none even close to 5


#bin 5
station_bin5 <- bin5dat %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #

join5stat <- left_join(bin5dat, station_bin5)

bin5_analysis_dat <- join5stat[which(join5stat$n_yrs>5),] #none even close to 5





#models======

#bin 1
modbin1 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                       te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=bin1_analysis_dat, method="ML")
saveRDS(modbin1, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linint_bin1_model.RDS")
gam.check(modbin1[[2]]) 
summary(modbin1[[1]]) #  12325.48 12423.15 -6149.742
summary(modbin1[[2]]) #rsq 0.11
linint1_aic <- AIC(modbin1$lme)

gammodbin1 <- modbin1$gam

draw(gammodbin1, select = 1)
draw(gammodbin1, select = 1, dist=0.05)
draw(gammodbin1, select = 1, dist=0.01)

appraise(gammodbin1$gam)

anova(modbin1[[2]])
plot(modbin1[[2]])
visreg(modbin1$lme, "bottemp_anom", "period")
visreg(modbin1[[2]], "bottemp_anom", "period")

binviz1 <- getViz(modbin1$gam)
plot(sm(binviz1 , 1))

library(sjPlot)
library(sjmisc)

plot_model(modbin1[[2]], type="int") #conditioned on fixed effects
plot_model(modbin1[[2]], type="int", pred.type = "re") #conditioned on random effects
plot_model(modbin1[[2]], type="int", pred.type = "re",
           show.data = TRUE) #conditioned on random effects
plot_model(modbin1[[2]], type="int", pred.type = "re",
           show.values = TRUE) 
plot_model(modbin1[[2]], type="resid")

visreg(modbin1$gam, "bottemp_anom", by="period", data=bin1_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5))

#bin 1 no interaction
dropbin1 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                         s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                       correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                       data=bin1_analysis_dat, method="ML")
saveRDS(dropbin1, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/smoothtemp_bin1_model.RDS")
gam.check(dropbin1[[2]]) 
summary(dropbin1[[1]]) #   did not converge
summary(dropbin1[[2]]) #rsq 0.
smooth1_aic <- AIC(dropbin1$lme)

gamdropbin1 <- dropbin1$gam

draw(gamdropbin1, select = 1)
draw(gamdropbin1, select = 1, dist=0.05)
draw(gamdropbin1, select = 1, dist=0.01)
draw(gamdropbin1, select = 2)

appraise(dropdropbin1$gam)

anova(dropdropbin1[[2]])
plot(dropdropbin1[[2]])



#bin 1 linear no interaction
droplinbin1 <- gamm(log_sum_WGTCPUE_LEN ~  bottemp_anom + te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin1_analysis_dat, method="ML")
saveRDS(droplinbin1, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/null_bin1_model.RDS")
gam.check(droplinbin1[[2]]) 
summary(droplinbin1[[1]]) #   12321.99 12404.63 -6149.994
summary(droplinbin1[[2]]) #rsq 0.
null1_aic <- AIC(droplinbin1$lme)

gamdroplinbin1 <- droplinbin1$gam

draw(gamdroplinbin1, select = 1)
draw(gamdroplinbin1, select = 1, dist=0.05)
draw(gamdroplinbin1, select = 1, dist=0.01)
draw(gamdroplinbin1, select = 2)

appraise(droplinbin1$gam)

anova(droplinbin1[[2]])
plot(droplinbin1[[2]])

#delta aic bin 1

print(c(null1_aic, smooth1_aic, linint1_aic))
#null is lowest
delta_null1 <- 0
delta_smooth1 <- smooth1_aic - null1_aic
  delta_linint1 <- linint1_aic - null1_aic


#bin 1 akaike weights

sumbin1aic <- sum(exp(-0.5*delta_null1), exp(-0.5*delta_smooth1), exp(-0.5*delta_linint1)) 

aw_null1 <- exp(-0.5*delta_null1)/sumbin1aic
aw_smooth1 <- exp(-0.5*delta_smooth1)/sumbin1aic
aw_linint1 <- exp(-0.5*delta_linint1)/sumbin1aic



#bin 2

modbin2 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2_analysis_dat, method="ML")
saveRDS(modbin2, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linint_bin2_model.RDS")
gam.check(modbin2[[2]]) 
summary(modbin2[[1]]) #  19009.5 19107.16 -9491.75
summary(modbin2[[2]]) #rsq 0.176
linint2_aic <- AIC(modbin2$lme)

gammodbin2 <- modbin2$gam

draw(gammodbin2, select = 1)
draw(gammodbin2, select = 1, dist=0.05)
draw(gammodbin2, select = 1, dist=0.01)

appraise(gammodbin2$gam)

anova(modbin2[[2]])
plot(modbin2[[2]])
visreg(modbin2$lme, "bottemp_anom", "period")
visreg(modbin2[[2]], "bottemp_anom", "period")

binviz2 <- getViz(modbin2$gam)
plot(sm(binviz2 , 1))

library(sjPlot)
library(sjmisc)

plot_model(modbin2[[2]], type="int") #conditioned on fixed effects
plot_model(modbin2[[2]], type="int", pred.type = "re") #conditioned on random effects
plot_model(modbin2[[2]], type="int", pred.type = "re",
           show.data = TRUE) #conditioned on random effects
plot_model(modbin2[[2]], type="int", pred.type = "re",
           show.values = TRUE) 
plot_model(modbin2[[2]], type="resid")

visreg(modbin2$gam, "bottemp_anom", by="period", data=bin2_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5))


#bin 2 no interaction
dropbin2 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2_analysis_dat, method="ML")
saveRDS(dropbin2, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/smoothtemp_bin2_model.RDS")
gam.check(dropbin2[[2]]) 
summary(dropbin2[[1]]) #  19007.85 19098 -9491.926 
summary(dropbin2[[2]]) #rsq 0.176
smooth2_aic <- AIC(dropbin2$lme)

gamdropbin2 <- dropbin2$gam

draw(gamdropbin2, select = 1)
draw(gamdropbin2, select = 1, dist=0.05)
draw(gamdropbin2, select = 1, dist=0.01)
draw(gamdropbin2, select = 2)

appraise(dropdropbin2$gam)

anova(dropbin2[[2]])
plot(dropbin2[[2]])



#bin 2 linear no interaction
droplinbin2 <- gamm(log_sum_WGTCPUE_LEN ~  bottemp_anom + te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=bin2_analysis_dat, method="ML")
saveRDS(droplinbin2, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/null_bin2_model.RDS")
gam.check(droplinbin2[[2]]) 
summary(droplinbin2[[1]]) #  19005.85 19088.49 -9491.926
summary(droplinbin2[[2]]) #rsq 0.176
null2_aic <- AIC(droplinbin2$lme)

gamdroplinbin2 <- droplinbin2$gam

draw(gamdroplinbin2, select = 1)
draw(gamdroplinbin2, select = 1, dist=0.05)
draw(gamdroplinbin2, select = 1, dist=0.01)
draw(gamdroplinbin2, select = 2)

appraise(droplinbin2$gam)

anova(droplinbin2[[2]])
plot(droplinbin2[[2]])

#delta aic bin 2

print(c(null2_aic, smooth2_aic, linint2_aic))
#null is lowest
delta_null2 <- 0
delta_smooth2 <- smooth2_aic - null2_aic
delta_linint2 <- linint2_aic - null2_aic


#bin 2 akaike weights

sumbin2aic <- sum(exp(-0.5*delta_null2), exp(-0.5*delta_smooth2), exp(-0.5*delta_linint2)) 

aw_null2 <- exp(-0.5*delta_null2)/sumbin2aic
aw_smooth2 <- exp(-0.5*delta_smooth2)/sumbin2aic
aw_linint2 <- exp(-0.5*delta_linint2)/sumbin2aic




#bin3

modbin3 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3_analysis_dat, method="ML")
saveRDS(modbin3, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linint_bin3_model.RDS")
gam.check(modbin3[[2]]) 
summary(modbin3[[1]]) #  36183.35 36281.01 -18078.68
summary(modbin3[[2]]) #rsq 0.293
linint3_aic <- AIC(modbin3$lme)

gammodbin3 <- modbin3$gam

draw(gammodbin3, select = 1)
draw(gammodbin3, select = 1, dist=0.05)
draw(gammodbin3, select = 1, dist=0.01)

appraise(gammodbin3$gam)

anova(modbin3[[2]])
plot(modbin3[[2]])
visreg(modbin3$lme, "bottemp_anom", "period")
visreg(modbin3[[2]], "bottemp_anom", "period")

binviz3 <- getViz(modbin3$gam)
plot(sm(binviz3 , 1))

library(sjPlot)
library(sjmisc)

plot_model(modbin3[[2]], type="int") #conditioned on fixed effects
plot_model(modbin3[[2]], type="int", pred.type = "re") #conditioned on random effects
plot_model(modbin3[[2]], type="int", pred.type = "re",
           show.data = TRUE) #conditioned on random effects
plot_model(modbin3[[2]], type="int", pred.type = "re",
           show.values = TRUE) 
plot_model(modbin3[[2]], type="resid")

visreg(modbin3$gam, "bottemp_anom", by="period", data=bin3_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5))


#bin 3 no interaction
dropbin3 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3_analysis_dat, method="ML")
saveRDS(dropbin3, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/smoothtemp_bin3_model.RDS")
gam.check(dropbin3[[2]]) 
summary(dropbin3[[1]]) #   36183.54 36273.69 -18079.77
summary(dropbin3[[2]]) #rsq 0.291
smooth3_aic <- AIC(dropbin3$lme)

gamdropbin3 <- dropbin3$gam

draw(gamdropbin3, select = 1)
draw(gamdropbin3, select = 1, dist=0.05)
draw(gamdropbin3, select = 1, dist=0.01)
draw(gamdropbin3, select = 2)

appraise(dropdropbin3$gam)

anova(dropbin3[[2]])
plot(dropbin3[[2]])


#bin 3 linear no interaction
droplinbin3 <- gamm(log_sum_WGTCPUE_LEN ~  bottemp_anom + te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=bin3_analysis_dat, method="ML")
saveRDS(droplinbin3, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/null_bin3_model.RDS")
gam.check(droplinbin3[[2]]) 
summary(droplinbin3[[1]]) #  36181.54 36264.18 -18079.77
summary(droplinbin3[[2]]) #rsq 0.291
null3_aic <- AIC(droplinbin3$lme)

gamdroplinbin3 <- droplinbin3$gam

draw(gamdroplinbin3, select = 1)
draw(gamdroplinbin3, select = 1, dist=0.05)
draw(gamdroplinbin3, select = 1, dist=0.01)
draw(gamdroplinbin3, select = 2)

appraise(droplinbin3$gam)

anova(droplinbin3[[2]])
plot(droplinbin3[[2]])


#delta aic bin 3

print(c(null3_aic, smooth3_aic, linint3_aic))
#null is lowest
delta_null3 <- 0
delta_smooth3 <- smooth3_aic - null3_aic
delta_linint3 <- linint3_aic - null3_aic


#bin 3 akaike weights

sumbin3aic <- sum(exp(-0.5*delta_null3), exp(-0.5*delta_smooth3), exp(-0.5*delta_linint3)) 

aw_null3 <- exp(-0.5*delta_null3)/sumbin3aic
aw_smooth3 <- exp(-0.5*delta_smooth3)/sumbin3aic
aw_linint3 <- exp(-0.5*delta_linint3)/sumbin3aic





#bin 4

modbin4 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4_analysis_dat, method="ML")
saveRDS(modbin4, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linint_bin4_model.RDS")
gam.check(modbin4[[2]]) 
summary(modbin4[[1]]) #  43565.06 43662.72 -21769.53
summary(modbin4[[2]]) #rsq 0.445
linint4_aic <- AIC(modbin4$lme)

gammodbin4 <- modbin4$gam

draw(gammodbin4, select = 1)
draw(gammodbin4, select = 1, dist=0.05)
draw(gammodbin4, select = 1, dist=0.01)

appraise(gammodbin4$gam)

anova(modbin4[[2]])
plot(modbin4[[2]])
visreg(modbin4$lme, "bottemp_anom", "period")
visreg(modbin4[[2]], "bottemp_anom", "period")

binviz4 <- getViz(modbin4$gam)
plot(sm(binviz4 , 1))

library(sjPlot)
library(sjmisc)

plot_model(modbin4[[2]], type="int") #conditioned on fixed effects
plot_model(modbin4[[2]], type="int", pred.type = "re") #conditioned on random effects
plot_model(modbin4[[2]], type="int", pred.type = "re",
           show.data = TRUE) #conditioned on random effects
plot_model(modbin4[[2]], type="int", pred.type = "re",
           show.values = TRUE) 
plot_model(modbin4[[2]], type="resid")


visreg(modbin4$gam, "bottemp_anom", by="period", data=bin4_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5))


#bin 4 no interaction
dropbin4 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4_analysis_dat, method="ML")
saveRDS(dropbin4, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/smoothtemp_bin4_model.RDS")
gam.check(dropbin4[[2]]) 
summary(dropbin4[[1]]) #   43598.86 43689.01 -21787.43
summary(dropbin4[[2]]) #rsq 0.411
smooth4_aic <- AIC(dropbin4$lme)

gamdropbin1 <- dropbin1$gam

draw(gamdropbin4, select = 1)
draw(gamdropbin4, select = 1, dist=0.05)
draw(gamdropbin4, select = 1, dist=0.01)
draw(gamdropbin4, select = 2)

appraise(dropdropbin4$gam)

anova(dropbin4[[2]])
plot(dropbin4[[2]])


#bin 4 linear no interaction
droplinbin4 <- gamm(log_sum_WGTCPUE_LEN ~  bottemp_anom + te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=bin4_analysis_dat, method="ML")
saveRDS(droplinbin4, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/null_bin4_model.RDS")
gam.check(droplinbin4[[2]]) 
summary(droplinbin4[[1]]) #   43596.86 43679.5 -21787.43
summary(droplinbin4[[2]]) #rsq 0.411
null4_aic <- AIC(droplinbin4$lme)

gamdroplinbin4 <- droplinbin4$gam

draw(gamdroplinbin4, select = 1)
draw(gamdroplinbin4, select = 1, dist=0.05)
draw(gamdroplinbin4, select = 1, dist=0.01)
draw(gamdroplinbin4, select = 2)

appraise(droplinbin4$gam)

anova(droplinbin4[[2]])
plot(droplinbin4[[2]])


#delta aic bin 4

print(c(null4_aic, smooth4_aic, linint4_aic))
#lin int is lowest
delta_null4 <- null4_aic - linint4_aic
delta_smooth4 <- smooth4_aic - linint4_aic
delta_linint4 <- 0


#bin 4 akaike weights

sumbin4aic <- sum(exp(-0.5*delta_null4), exp(-0.5*delta_smooth4), exp(-0.5*delta_linint4)) 

aw_null4 <- exp(-0.5*delta_null4)/sumbin4aic
aw_smooth4 <- exp(-0.5*delta_smooth4)/sumbin4aic
aw_linint4 <- exp(-0.5*delta_linint4)/sumbin4aic





#bin 5 

modbin5 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5_analysis_dat, method="ML")
saveRDS(modbin5, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/linint_bin5_model.RDS")
droplinbin4
gam.check(modbin5[[2]]) 
summary(modbin5[[1]]) #  40562.73 40660.39 -20268.37
summary(modbin5[[2]]) #rsq 0.222
linint5_aic <- AIC(modbin5$lme)

gammodbin5 <- modbin5$gam

draw(gammodbin5, select = 1)
draw(gammodbin5, select = 1, dist=0.05)
draw(gammodbin5, select = 1, dist=0.01)

appraise(gammodbin5$gam)

anova(modbin5[[2]])
plot(modbin5[[2]])
visreg(modbin5$lme, "bottemp_anom", "period")
visreg(modbin5[[2]], "bottemp_anom", "period")

binviz5 <- getViz(modbin5$gam)
plot(sm(binviz5 , 1))

library(sjPlot)
library(sjmisc)

plot_model(modbin5[[2]], type="int") #conditioned on fixed effects
plot_model(modbin5[[2]], type="int", pred.type = "re") #conditioned on random effects
plot_model(modbin5[[2]], type="int", pred.type = "re",
           show.data = TRUE) #conditioned on random effects
plot_model(modbin5[[2]], type="int", pred.type = "re",
           show.values = TRUE) 
plot_model(modbin5[[2]], type="resid")

visreg(modbin5$gam, "bottemp_anom", by="period", data=bin5_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5))


#bin 5 no interaction
dropbin5 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5_analysis_dat, method="ML")
saveRDS(dropbin5, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/smoothtemp_bin5_model.RDS")
gam.check(dropbin5[[2]]) 
summary(dropbin5[[1]]) #   40565.94 40656.09 -20270.97
summary(dropbin5[[2]]) #rsq 0.218
smooth5_aic <- AIC(dropbin5$lme)

plot(dropbin5[[2]])

gamdropbin5 <- dropbin5$gam

draw(gamdropbin5, select = 1)
draw(gamdropbin5, select = 1, dist=0.05)
draw(gamdropbin5, select = 1, dist=0.01)
draw(gamdropbin5, select = 2)

appraise(dropdropbin5$gam)

anova(dropbin5[[2]])
plot(dropbin5[[2]])



#bin 5 linear no interaction
droplinbin5 <- gamm(log_sum_WGTCPUE_LEN ~  bottemp_anom + te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                    correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                    data=bin5_analysis_dat, method="ML")
saveRDS(droplinbin5, file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/null_bin5_model.RDS")
gam.check(droplinbin5[[2]]) 
summary(droplinbin5[[1]]) #   did not converge
summary(droplinbin5[[2]]) #rsq 0.
null5_aic <- AIC(droplinbin5$lme)

gamdroplinbin5 <- droplinbin5$gam

draw(gamdroplinbin5, select = 1)
draw(gamdroplinbin5, select = 1, dist=0.05)
draw(gamdroplinbin5, select = 1, dist=0.01)
draw(gamdroplinbin5, select = 2)

appraise(droplinbin5$gam)

anova(droplinbin5[[2]])
plot(droplinbin5[[2]])


#delta aic bin 5

print(c(null5_aic, smooth5_aic, linint5_aic))
#lin int is lowest
delta_null5 <- null5_aic - linint5_aic
delta_smooth5 <- smooth5_aic - linint5_aic
delta_linint5 <- 0


#bin 5 akaike weights

sumbin5aic <- sum(exp(-0.5*delta_null5), exp(-0.5*delta_smooth5), exp(-0.5*delta_linint5)) 

aw_null5 <- exp(-0.5*delta_null5)/sumbin5aic
aw_smooth5 <- exp(-0.5*delta_smooth5)/sumbin5aic
aw_linint5 <- exp(-0.5*delta_linint5)/sumbin5aic



#plot for manu

par(mfrow=c(2,3), mai=c(0.3,0.4,0.3,0.1)) 

visreg(modbin1$gam, "bottemp_anom", by="period", data=bin1_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5), legend=FALSE, xlab="",
       line=list(col=(c("#0083c9", "red"))),
       fill=list(col=(c("#0083c980", "#FF4E3780"))))

visreg(modbin2$gam, "bottemp_anom", by="period", data=bin2_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5), legend=FALSE, xlab="",
       line=list(col=(c("#0083c9", "red"))),
       fill=list(col=(c("#0083c980", "#FF4E3780"))))

visreg(modbin3$gam, "bottemp_anom", by="period", data=bin3_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5), legend=FALSE, xlab="",
       line=list(col=(c("#0083c9", "red"))),
       fill=list(col=(c("#0083c980", "#FF4E3780"))))

visreg(modbin4$gam, "bottemp_anom", by="period", data=bin4_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5), legend=FALSE, xlab="",
       line=list(col=(c("#0083c9", "red"))),
       fill=list(col=(c("#0083c980", "#FF4E3780"))))

visreg(modbin5$gam, "bottemp_anom", by="period", data=bin5_analysis_dat,
       overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5), legend=FALSE, xlab="",
       line=list(col=(c("#0083c9", "red"))),
       fill=list(col=(c("#0083c980", "#FF4E3780"))))

# visreg(modbin5$gam, "bottemp_anom", by="period", data=bin5_analysis_dat,
#        overlay=TRUE, partial=FALSE, rug=FALSE, ylim=c(-0.5,5), legend=FALSE, xlab="",
#        line=list(col=(c("#0083c9", "red"))),
#        fill=list(col=(c("#0083c980", "#FF4E3780"))))
