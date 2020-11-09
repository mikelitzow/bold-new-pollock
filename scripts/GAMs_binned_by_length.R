#stats on data binned by length

#using binmeta from join_CPUEdat_w_sizeCPUEdat.R
library(ggplot2)
library(sjPlot)

binmeta_clean$log_sum_WGTCPUE_LEN <- log(binmeta_clean$sum_wgtCPUE_len + 1)


binmeta_clean$STATION <- as.factor(binmeta_clean$STATION)
binmeta_clean$VESSEL <- as.factor(binmeta_clean$VESSEL)
binmeta_clean$CRUISE <- as.factor(binmeta_clean$CRUISE)
binmeta_clean$HAUL <- as.factor(binmeta_clean$HAUL)
binmeta_clean$bin <- as.factor(binmeta_clean$bin)

##exclusion criteria===============

#how to exclude stations w few obs? Maybe n in a single bin? yeah do once split out


bin1dat <- binmeta_clean[which(binmeta_clean$bin=="0-200"),]
bin2dat <- binmeta_clean[which(binmeta_clean$bin=="200-300"),]
bin3dat <- binmeta_clean[which(binmeta_clean$bin=="300-400"),]
bin4dat <- binmeta_clean[which(binmeta_clean$bin=="400-500"),]
bin5dat <- binmeta_clean[which(binmeta_clean$bin=="500+"),]

#exclude by bin

#bin 1
station_bin1 <- bin1dat %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #

join1stat <- left_join(bin1dat, station_bin1)

bin1_analysis_dat <- join2stat[which(join1stat$n_yrs>5),] #none even close to 5


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
gam.check(modbin1[[2]]) 
summary(modbin1[[1]]) #  
summary(modbin1[[2]]) #rsq 0.

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



#bin 2

modbin2 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin2_analysis_dat, method="ML")
gam.check(modbin2[[2]]) 
summary(modbin2[[1]]) #  
summary(modbin2[[2]]) #rsq 0.

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



#bin3

modbin3 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin3_analysis_dat, method="ML")
gam.check(modbin3[[2]]) 
summary(modbin3[[1]]) #  
summary(modbin3[[2]]) #rsq 0.

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



#bin 4

modbin4 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin4_analysis_dat, method="ML")
gam.check(modbin4[[2]]) 
summary(modbin4[[1]]) #  
summary(modbin4[[2]]) #rsq 0.

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




#bin 5 

modbin5 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5_analysis_dat, method="ML")
gam.check(modbin5[[2]]) 
summary(modbin5[[1]]) #  
summary(modbin5[[2]]) #rsq 0.

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

