#stats on data binned by length

#using binmeta from join_CPUEdat_w_sizeCPUEdat.R
library(ggplot2)
library(sjPlot)

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


#bin 1 no interaction
dropbin1 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                         s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                       correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                       data=bin1_analysis_dat, method="ML")
gam.check(dropbin1[[2]]) 
summary(dropbin1[[1]]) #   did not converge
summary(dropbin1[[2]]) #rsq 0.

gamdropbin1 <- dropbin1$gam

draw(gamdropbin1, select = 1)
draw(gamdropbin1, select = 1, dist=0.05)
draw(gamdropbin1, select = 1, dist=0.01)
draw(gamdropbin1, select = 2)

appraise(dropdropbin1$gam)

anova(dropdropbin1[[2]])
plot(dropdropbin1[[2]])





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


#bin 2 no interaction
dropbin2 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin2_analysis_dat, method="ML")
gam.check(dropbin2[[2]]) 
summary(dropbin2[[1]]) #   
summary(dropbin2[[2]]) #rsq 0.

gamdropbin2 <- dropbin2$gam

draw(gamdropbin2, select = 1)
draw(gamdropbin2, select = 1, dist=0.05)
draw(gamdropbin2, select = 1, dist=0.01)
draw(gamdropbin2, select = 2)

appraise(dropdropbin2$gam)

anova(dropdropbin2[[2]])
plot(dropdropbin2[[2]])



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


#bin 3 no interaction
dropbin3 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin3_analysis_dat, method="ML")
gam.check(dropbin3[[2]]) 
summary(dropbin3[[1]]) #   
summary(dropbin3[[2]]) #rsq 0.

gamdropbin3 <- dropbin3$gam

draw(gamdropbin3, select = 1)
draw(gamdropbin3, select = 1, dist=0.05)
draw(gamdropbin3, select = 1, dist=0.01)
draw(gamdropbin3, select = 2)

appraise(dropdropbin3$gam)

anova(dropdropbin3[[2]])
plot(dropdropbin3[[2]])





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



#bin 4 no interaction
dropbin4 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin4_analysis_dat, method="ML")
gam.check(dropbin4[[2]]) 
summary(dropbin4[[1]]) #   
summary(dropbin4[[2]]) #rsq 0.

gamdropbin1 <- dropbin1$gam

draw(gamdropbin4, select = 1)
draw(gamdropbin4, select = 1, dist=0.05)
draw(gamdropbin4, select = 1, dist=0.01)
draw(gamdropbin4, select = 2)

appraise(dropdropbin4$gam)

anova(dropdropbin4[[2]])
plot(dropdropbin4[[2]])




#bin 5 

modbin5 <- gamm(log_sum_WGTCPUE_LEN ~ bottemp_anom*period +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=bin5_analysis_dat, method="ML")
gam.check(modbin5[[2]]) 
summary(modbin5[[1]]) #  
summary(modbin5[[2]]) #rsq 0.222

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


#bin 5 no interaction
dropbin5 <- gamm(log_sum_WGTCPUE_LEN ~  te(mean_station_bottemp, BOT_DEPTH, k=29) +
                   s(bottemp_anom, k=4), random=list(YEAR_factor=~1), 
                 correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                 data=bin5_analysis_dat, method="ML")
gam.check(dropbin5[[2]]) 
summary(dropbin5[[1]]) #   
summary(dropbin5[[2]]) #rsq 0.

gamdropbin5 <- dropbin5$gam

draw(gamdropbin5, select = 1)
draw(gamdropbin5, select = 1, dist=0.05)
draw(gamdropbin5, select = 1, dist=0.01)
draw(gamdropbin5, select = 2)

appraise(dropdropbin5$gam)

anova(dropdropbin5[[2]])
plot(dropdropbin5[[2]])
