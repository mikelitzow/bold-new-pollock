#==========================================================================================
# Adding in Cold Pool Extent (CPE)
#
# Krista, Dec 2020
#==========================================================================================
#Notes:
#==========================================================================================

library(mgcv)
library(ggplot2)
#remotes::install_github("gavinsimpson/gratia")
library(gratia)
library(mgcViz)
library(cowplot)

#Get data plot data =======

# load environmental data
cdat <- read.csv("data/climate data.csv", row.names = 1)


# scale data
do.cdat <- as.data.frame(scale(cdat)) # scale to plot on 1 axis
plot.cdat <- gather(do.cdat)
plot.cdat$year <- 1951:2019

ggplot(plot.cdat, aes(x=year, y=value)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~key) +
  ylab("Standard anomaly") + xlab("") + theme_bw() + geom_hline(yintercept = 0)


#grab best model and data from previous scripts (allages_model_fitting.R)

#use data previously cleaned 
#periods_analysis_dat is also loaded in trawl_biomass_GAM_explor.R
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)

#model
mod <- readRDS(file="~/Dropbox/Work folder/Pollock Analyses/bold-new-pollock/scripts/lin-int_allages_model.RDS")

summary(mod)

gam.check(mod[[2]]) 
summary(mod[[1]]) #   44063.32 44161.09 -22018.66
summary(mod[[2]]) #rsq   0.366 

#join clim data to data============

do.cdat$YEAR <- as.integer(rownames(do.cdat))

sub_CPE <- do.cdat[,c("YEAR", "summer.cold.pool.extent")]

cpedat <- left_join(periods_analysis_dat, sub_CPE)

#new model w CPE===================

mod_CPE <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom*period + summer.cold.pool.extent +
                       te(mean_station_bottemp, BOT_DEPTH, k=29), random=list(YEAR_factor=~1), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=cpedat[which(cpedat$STRATUM!=70 &
                                         cpedat$STRATUM!=71 &
                                         cpedat$STRATUM!=81),], method="ML")

gam.check(mod_CPE[[2]]) 
summary(mod_CPE[[1]]) # 
summary(mod_CPE[[2]]) #rsq 0.382




mod_CPE_noyr <- gamm(logCPUE_Gadus_chalcogrammus ~ bottemp_anom*period + summer.cold.pool.extent +
                  te(mean_station_bottemp, BOT_DEPTH, k=29), #random=list(YEAR_factor=~1), 
                correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                data=cpedat[which(cpedat$STRATUM!=70 &
                                    cpedat$STRATUM!=71 &
                                    cpedat$STRATUM!=81),], method="ML")

gam.check(mod_CPE_noyr[[2]]) 
summary(mod_CPE_noyr[[1]]) #  
summary(mod_CPE_noyr[[2]]) #rsq 0.382

plot_model(lin_tek3corE[[2]], type="int") #conditioned on fixed effects
plot_model(lin_tek3corE[[2]], type="int", pred.type = "re") #conditioned on random effects
plot_model(lin_tek3corE[[2]], type="int", pred.type = "re",
           show.data = TRUE) #conditioned on random effects
plot_model(mod_CPE_noyr[[2]], type="int", pred.type = "re",
           show.values = TRUE) 
plot_model(mod_CPE_noyr[[2]], type="resid")


mod_CPE_only <- gamm(logCPUE_Gadus_chalcogrammus ~ summer.cold.pool.extent +
                       te(mean_station_bottemp, BOT_DEPTH, k=29), #random=list(YEAR_factor=~1), 
                     correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=cpedat[which(cpedat$STRATUM!=70 &
                                         cpedat$STRATUM!=71 &
                                         cpedat$STRATUM!=81),], method="ML")

gam.check(mod_CPE_only[[2]]) 
summary(mod_CPE_only[[1]]) #  
summary(mod_CPE_only[[2]]) #rsq 0.36

draw(mod_CPE_only$gam, select = 1)
draw(mod_CPE_only$gam, select = 1, dist=0.05)
draw(mod_CPE_only$gam, select = 1, dist=0.01)


#uh oh this included NEBS stations
#will likely need to update elsewhere too


#try a full on spatio-temporal version?
stmod <- gam(logCPUE_Gadus_chalcogrammus ~ s(long_albers, lat_albers, bottemp_anom, by=period) + 
                s(long_albers, lat_albers, summer.cold.pool.extent) +
                       s(BOT_DEPTH), #random=list(YEAR_factor=~1), 
                    # correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
                     data=cpedat[which(cpedat$STRATUM!=70 &
                                         cpedat$STRATUM!=71 &
                                         cpedat$STRATUM!=81),], method="ML")
summary(stmod)


v <- getViz(stmod)

# Plot slices
pl2 <- plotSlice(x = sm(v, 2), 
                fix = list("summer.cold.pool.extent" = seq(-2, 2, length.out = 5)))
pl2 + l_fitRaster() + l_fitContour() + l_points() + l_rug()


# Plot slices
pl <- plotSlice(x = sm(v, 1), 
                fix = list("bottemp_anom" = seq(-4, 4, length.out = 9), "period"=seq(1,2)))
pl + l_fitRaster() + l_fitContour() + l_points() + l_rug()
#period 1=early

p1 <- plot(sm(v, 1), fix = c("bottemp_anom"=0, "period"=1)) + l_fitRaster() + l_fitContour()
p2 <- plot(sm(v, 1), fix = c("bottemp_anom" = -4, "period"=1)) + l_fitRaster() + l_fitContour()
p3 <- plot(sm(v, 1), fix = c("bottemp_anom" = 4, "period"=1)) + l_fitRaster() + l_fitContour()
p4 <- plot(sm(v, 1), fix = c("bottemp_anom"=0, "period"=2)) + l_fitRaster() + l_fitContour()
p5 <- plot(sm(v, 1), fix = c("bottemp_anom" = -4, "period"=2)) + l_fitRaster() + l_fitContour()
p6 <- plot(sm(v, 1), fix = c("bottemp_anom" = 4, "period"=2)) + l_fitRaster() + l_fitContour()




stmod2 <- gam(logCPUE_Gadus_chalcogrammus ~ s(long_albers, lat_albers, bottemp_anom, YEAR) + 
               s(long_albers, lat_albers, summer.cold.pool.extent) +
               s(BOT_DEPTH), #random=list(YEAR_factor=~1), 
             # correlation = corExp(form=~ long_albers + lat_albers|YEAR_factor, nugget=TRUE),
             data=cpedat[which(cpedat$STRATUM!=70 &
                                 cpedat$STRATUM!=71 &
                                 cpedat$STRATUM!=81),], method="ML")
summary(stmod2)
