#============================================================================================================================================
# GAM EXPLORATION - survey abundance data

#Created by Krista, May 2020
#============================================================================================================================================
#Notes:
#============================================================================================================================================
library(ggplot2)
library(tidyverse)
library(corrplot)
library(mgcv)
library(nlme)
library(mgcViz)



sel.trawl.dat <- read.csv("data/select_trawl_dat.csv", row.names = 1)

sel.trawl.dat$YEAR_factor <- as.factor(sel.trawl.dat$YEAR)
sel.trawl.dat$BOT_TEMP[which(sel.trawl.dat$BOT_TEMP=="-9999")]<-NA
sel.trawl.dat$SURF_TEMP[which(sel.trawl.dat$SURF_TEMP=="-9999")]<-NA

#going to need to get into a wider database I think to get other sps in



#select data======================================================

#we only want data pre2014
early_dat <- sel.trawl.dat[which(sel.trawl.dat$YEAR<2014),]
early_dat <- early_dat[,-c(9,11)] #drop columns that repeat info



#widen dataframe to have columns for each sps

# early_wide <- early_dat %>% pivot_wider(names_from=c(LONGITUDE, LATITUDE, STATION, STRATUM, YEAR, 
#                                                      DATETIME, VESSEL, CRUISE, HAUL), 
#                                         values_from=c(WTCPUE, NUMCPUE, logCPUE))

early_wide <- early_dat %>% pivot_wider(names_from=SCIENTIFIC, 
                                        values_from=c(WTCPUE, NUMCPUE, logCPUE))
#eek this causes some bad column names!
early_wide <- early_wide %>% rename(WTCPUE_Chionoecetes_bairdi = "WTCPUE_Chionoecetes bairdi",            
  WTCPUE_Atheresthes_stomia = "WTCPUE_Atheresthes stomias",             
  WTCPUE_Hippoglossus_stenolepis = "WTCPUE_Hippoglossus stenolepis",
  WTCPUE_Limanda_aspera = "WTCPUE_Limanda aspera",         
  WTCPUE_Lepidopsetta_sp= "WTCPUE_Lepidopsetta sp.",               
  WTCPUE_Chionoecetes_opilio = "WTCPUE_Chionoecetes opilio",             
  WTCPUE_Gadus_macrocephalus = "WTCPUE_Gadus macrocephalus",            
  WTCPUE_Hippoglossoides_elassodon = "WTCPUE_Hippoglossoides elassodon",  
  WTCPUE_Pleuronectes_quadrituberculatus = "WTCPUE_Pleuronectes quadrituberculatus", 
  WTCPUE_Lepidopsetta_polyxystra = "WTCPUE_Lepidopsetta polyxystra", 
  NUMCPUE_Gadus_chalcogrammus = "NUMCPUE_Gadus chalcogrammus",        
  NUMCPUE_Chionoecetes_bairdi = "NUMCPUE_Chionoecetes bairdi",            
  NUMCPUE_Atheresthes_stomias = "NUMCPUE_Atheresthes stomias",           
  NUMCPUE_Hippoglossus_stenolepis = "NUMCPUE_Hippoglossus stenolepis",        
  NUMCPUE_Limanda_aspera = "NUMCPUE_Limanda aspera",                
  NUMCPUE_Lepidopsetta_sp = "NUMCPUE_Lepidopsetta sp.",               
  NUMCPUE_Chionoecetes_opilio = "NUMCPUE_Chionoecetes opilio",           
  NUMCPUE_Gadus_macrocephalus = "NUMCPUE_Gadus macrocephalus",            
  NUMCPUE_Hippoglossoides_elassodon = "NUMCPUE_Hippoglossoides elassodon",     
  NUMCPUE_Pleuronectes_quadrituberculatus = "NUMCPUE_Pleuronectes quadrituberculatus",
  NUMCPUE_Lepidopsetta_polyxystra = "NUMCPUE_Lepidopsetta polyxystra",
  logCPUE_Gadus_chalcogrammus = "logCPUE_Gadus chalcogrammus",        
  logCPUE_Chionoecetes_bairdi = "logCPUE_Chionoecetes bairdi",           
  logCPUE_Atheresthes_stomias = "logCPUE_Atheresthes stomias",            
  logCPUE_Hippoglossus_stenolepis = "logCPUE_Hippoglossus stenolepis", 
  logCPUE_Limanda_aspera = "logCPUE_Limanda aspera",        
  logCPUE_Lepidopsetta_sp = "logCPUE_Lepidopsetta sp.",              
  logCPUE_Chionoecetes_opilio = "logCPUE_Chionoecetes opilio",            
  logCPUE_Gadus_macrocephalus = "logCPUE_Gadus macrocephalus",           
  logCPUE_Hippoglossoides_elassodon = "logCPUE_Hippoglossoides elassodon",      
  logCPUE_Pleuronectes_quadrituberculatus = "logCPUE_Pleuronectes quadrituberculatus" ,
  logCPUE_Lepidopsetta_polyxystra = "logCPUE_Lepidopsetta polyxystra") 

#this results in a lot of NAs that should be zero, so I will enter zeros

early_wide[,14:46][is.na(early_wide[,14:46])] <- 0 #WAIT ZERO OR NO?

#exclusion criteria===============
library(tidyverse)
station_summary <- early_wide %>% group_by(STATION, LONGITUDE, LATITUDE) %>%
  summarize(n_yrs=n())

station_summary2 <- early_wide %>% group_by(STATION) %>%
  summarize(n_yrs=n())

joinearly <- left_join(early_wide, station_summary2)

analysis_dat <- joinearly[which(joinearly$n_yrs>5),]

#plot CPUE maps======

t1 <- ggplot(early_wide, aes(LONGITUDE, LATITUDE, col=logCPUE_Gadus_chalcogrammus))
t1 + geom_point() + facet_wrap(~YEAR) +
  scale_colour_gradient2(low="blue", high="red", guide="colorbar")

t2 <- ggplot(early_wide, aes(LONGITUDE, LATITUDE, fill=logCPUE_Gadus_chalcogrammus))
t2 + geom_tile() # + facet_wrap(~YEAR)

t3 <- ggplot(early_wide, aes(STATION))
t3 + geom_histogram(stat="count")



#a base model====
ti_base_mod <- gam(logCPUE_Gadus_chalcogrammus ~ s(YEAR) + s(LONGITUDE, LATITUDE) +
                     ti(LONGITUDE, LATITUDE, YEAR, d=c(2,1)), 
                   data=analysis_dat)
plot(ti_base_mod, scheme = 2)
gam.check(ti_base_mod)
ti_p <- getViz(ti_base_mod)

plot(sm(ti_p, 2))
print(plot(ti_p, allTerms = T), pages = 1)
plotRGL(sm(ti_p, 3), fix = c("YEAR" = 2000), residuals = TRUE)

par(mfrow=c(2,2))
plot(sm(ti_p, 3), fix = c("YEAR" = 2000))
plot(sm(ti_p, 3), fix = c("YEAR" = 2001))
plot(sm(ti_p, 3), fix = c("YEAR" = 2002))
plot(sm(ti_p, 3), fix = c("YEAR" = 2003))

plot(sm(ti_p, 3), fix = c("YEAR" = 1982))
plot(sm(ti_p, 3), fix = c("YEAR" = 2013))


#find 'global' best model======================================================================================



#start small------

small1 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP), 
               data=analysis_dat)
summary(small1 )
plot(small1 )
gam.check(small1 ) #k too small
small1k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP, k=10), 
              data=analysis_dat)
gam.check(small1k ) #GOOD
#saveRDS(small1k, "scripts/GAM_output/mod_output_yr_stemp.rds") 

small2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE), 
              data=analysis_dat)
summary(small2 ) #big increase in dev explained
plot(small2 )
gam.check(small2 ) #k too low
# small2k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                 t2(LONGITUDE, LATITUDE, k=23),
#               data=analysis_dat)
# gam.check(small2k) #GOOD
small2k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                 t2(LONGITUDE, LATITUDE, k=22), 
               data=analysis_dat)
gam.check(small2k2) #BETTER
plot(small2k2)
#saveRDS(small2k2, "scripts/GAM_output/mod_output_yr_stemp_t2lat-long.rds") 

small3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, by=factor(YEAR)), 
              data=analysis_dat)

summary(small3 )
plot(small3 )
gam.check(small3 ) #k too low, not pos defin

small3k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE, k=20) + t2(LONGITUDE, LATITUDE, by=factor(YEAR), k=20), 
              data=analysis_dat) # return to this, too big

sm3 <- getViz(small3)

plot(sm(sm3, 3), fix = c("YEAR" = 2000))
plot(sm(sm3, 3), fix = c("YEAR" = 2001))
plot(sm(sm3, 3), fix = c("YEAR" = 2002))
plot(sm(sm3, 3), fix = c("YEAR" = 2003))

plot(sm(sm3, 3), fix = c("YEAR" = 1982))
plot(sm(sm3, 3), fix = c("YEAR" = 2013))

small4 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, BOT_TEMP), 
              data=analysis_dat)

summary(small4 )
plot(small4 )
gam.check(small4 ) #k too low bad hessian
# small4k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                 t2(LONGITUDE, LATITUDE, k=20) + t2(LONGITUDE, LATITUDE, BOT_TEMP), 
#               data=analysis_dat)
# gam.check(small4k) #good k, bad hessian
# small4k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=15) + t2(LONGITUDE, LATITUDE, BOT_TEMP), 
#                data=analysis_dat)
# gam.check(small4k2) #k too low
# small4k3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                   t2(LONGITUDE, LATITUDE, k=17) + t2(LONGITUDE, LATITUDE, BOT_TEMP),
#                 data=analysis_dat)
# gam.check(small4k3) #k too low
small4k4 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                  t2(LONGITUDE, LATITUDE, k=18) + t2(LONGITUDE, LATITUDE, BOT_TEMP), 
                data=analysis_dat)
gam.check(small4k4) #GOOD
#saveRDS(small4k4, "scripts/GAM_output/mod_output_yr_stemp_t2lat-long_t2lat-long-temp.rds") 


small4.5 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                # t2(LONGITUDE, LATITUDE) + 
                  t2(LONGITUDE, LATITUDE, BOT_TEMP), 
              data=analysis_dat)
summary(small4.5 )
plot(small4.5 )
gam.check(small4.5 ) #k too low bad hessian
# small4.5k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                   # t2(LONGITUDE, LATITUDE) + 
#                   t2(LONGITUDE, LATITUDE, BOT_TEMP, k=15), 
#                 data=analysis_dat)
# gam.check(small4.5k) #bad hessian, good k
# small4.5k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                    # t2(LONGITUDE, LATITUDE) + 
#                    t2(LONGITUDE, LATITUDE, BOT_TEMP, k=10), 
#                  data=analysis_dat)
# gam.check(small4.5k2) #k too low
# small4.5k3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                     # t2(LONGITUDE, LATITUDE) + 
#                     t2(LONGITUDE, LATITUDE, BOT_TEMP, k=12), 
#                   data=analysis_dat)
# gam.check(small4.5k3) #bad hessian, good k
# small4.5k4 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                     # t2(LONGITUDE, LATITUDE) + 
#                     t2(LONGITUDE, LATITUDE, BOT_TEMP, k=11), 
#                   data=analysis_dat)
# gam.check(small4.5k4) #bad hessian, k too low
# small4.5k5 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                     # t2(LONGITUDE, LATITUDE) + 
#                     t2(LONGITUDE, LATITUDE, BOT_TEMP, k=15), 
#                   data=analysis_dat)
# gam.check(small4.5k5) #bad hessian
# small4.5k6 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                     # t2(LONGITUDE, LATITUDE) + 
#                     t2(LONGITUDE, LATITUDE, BOT_TEMP, k=13), 
#                   data=analysis_dat)
# gam.check(small4.5k6)#k too low
small4.5k7 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                    # t2(LONGITUDE, LATITUDE) + 
                    t2(LONGITUDE, LATITUDE, BOT_TEMP, k=14), 
                  data=analysis_dat)
gam.check(small4.5k7) #GOOD
#saveRDS(small4.5k7, "scripts/GAM_output/mod_output_yr_stemp_t2lat-long-temp.rds") 

small4.6 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                  # t2(LONGITUDE, LATITUDE) + 
                  t2(LONGITUDE, LATITUDE, BOT_TEMP), 
                data=analysis_dat)
summary(small4.6 )
plot(small4.6 )
gam.check(small4.6 ) #k is too low
# small4.6k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
#                   # t2(LONGITUDE, LATITUDE) + 
#                   t2(LONGITUDE, LATITUDE, BOT_TEMP, k=11), 
#                 data=analysis_dat)
# gam.check(small4.6k) #k too low and bad Hessian
# small4.6k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
#                    # t2(LONGITUDE, LATITUDE) + 
#                    t2(LONGITUDE, LATITUDE, BOT_TEMP, k=13), 
#                  data=analysis_dat)
# gam.check(small4.6k2) #k little too low
small4.6k3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                    # t2(LONGITUDE, LATITUDE) + 
                    t2(LONGITUDE, LATITUDE, BOT_TEMP, k=14), 
                  data=analysis_dat)
gam.check(small4.6k3)#good!
saveRDS(small4.6k3, "scripts/GAM_output/mod_output_yr_temp_t2lat-long-temp.rds") 


AIC(small1, small2, small3, small4) #small 3 by far best AIC, small 4 next best

E1 <- resid(small3, type="pearson")
F1 <- fitted(small3)
plot(F1, E1)

small5 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                #t2(LONGITUDE, LATITUDE, k=25) + 
                t2(LONGITUDE, LATITUDE, by=factor(YEAR)), 
              data=analysis_dat)

summary(small5 )
plot(small5 )
gam.check(small5 ) 


#small but ti====


small2ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LONGITUDE, LATITUDE), 
              data=analysis_dat)

summary(small2ti ) #
plot(small2ti )
gam.check(small2ti ) 
# small2tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                   ti(LONGITUDE, LATITUDE, k=15), 
#                 data=analysis_dat)
# gam.check(small2tik)#too low
small2tik2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                   ti(LONGITUDE, LATITUDE, k=17), 
                 data=analysis_dat)
gam.check(small2tik2) #GOOD
#saveRDS(small2tik2, "scripts/GAM_output/mod_output_yr_stemp_tilat-long.rds") OLD REPLACE

small3ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LONGITUDE, LATITUDE) + ti(LONGITUDE, LATITUDE, by=factor(YEAR)), 
              data=analysis_dat)

summary(small3ti )
plot(small3ti )
gam.check(small3ti ) 


small4ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LONGITUDE, LATITUDE) + ti(LONGITUDE, LATITUDE, BOT_TEMP), 
              data=analysis_dat)

summary(small4ti )
plot(small4ti )
gam.check(small4ti ) #k too low
small4tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                  ti(LONGITUDE, LATITUDE, k=17) + ti(LONGITUDE, LATITUDE, BOT_TEMP), 
                data=analysis_dat)
gam.check(small4tik) #GOOD
# small4tik2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                    ti(LONGITUDE, LATITUDE, k=15) + ti(LONGITUDE, LATITUDE, BOT_TEMP), 
#                  data=analysis_dat)
# gam.check(small4tik2) #k too low
#saveRDS(small4tik, "scripts/GAM_output/mod_output_yr_temp_tilat-long_tilat-long-temp.rds") OLD REPLACE

small4.5ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                  ti(LONGITUDE, LATITUDE, BOT_TEMP), 
                data=analysis_dat)

summary(small4.5ti )
plot(small4.5ti )
gam.check(small4.5ti )
small4.5tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                    ti(LONGITUDE, LATITUDE, BOT_TEMP, k=17), 
                  data=analysis_dat)
gam.check(small4.5tik) #GOOD
#saveRDS(small4.5tik, "scripts/GAM_output/mod_output_yr_stemp_tilat-long-temp.rds") OLD REPLACE

small4.6ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                  ti(LONGITUDE, LATITUDE, BOT_TEMP), 
                data=analysis_dat)

summary(small4.6ti )
plot(small4.6ti )
gam.check(small4.6ti ) #k is too low
small4.6tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                    ti(LONGITUDE, LATITUDE, BOT_TEMP, k=17), 
                  data=analysis_dat)
gam.check(small4.6tik) #GOOD BUT CRAZY SLOW
#saveRDS(small4.6tik, "scripts/GAM_output/mod_output_yr_temp_tilat-long-temp.rds") OLD REPLACE

small5ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LONGITUDE, LATITUDE, by=factor(YEAR)), 
              data=analysis_dat)

summary(small5ti )
plot(small5ti )
gam.check(small5ti ) 

AIC(small1, small2ti, small4.6ti, small3ti, small4ti, small4.5ti, small5ti,
    small2, small3, small4, small4.5, small4.6, small5)

#loop through species=======================================

#==============================================================================================================

#Using temperature not year======

plot(early_wide$YEAR, early_wide$BOT_TEMP)
plot(early_wide$YEAR, early_wide$SURF_TEMP)

plot( early_wide$BOT_TEMP, early_wide$logCPUE_Gadus_chalcogrammus)
plot( early_wide$BOT_DEPTH, early_wide$logCPUE_Gadus_chalcogrammus)
plot( early_wide$SURF_TEMP, early_wide$logCPUE_Gadus_chalcogrammus)


ggplot(early_wide, aes(YEAR, BOT_TEMP)) + geom_point()



#add climate variables========
# load environmental data
climdat <- read.csv("data/climate data.csv")

wide_join <- left_join(early_wide, climdat, by=c("YEAR" = "year"))

p2 <- ggplot(wide_join, aes(summer.cold.pool.extent, logCPUE_Gadus_chalcogrammus))
p2 + geom_smooth() + geom_point()

p3 <- ggplot(wide_join, aes(AO.jfm, logCPUE_Gadus_chalcogrammus))
p3 + geom_smooth() + geom_point()

p4 <- ggplot(wide_join, aes(south.sst.ndjfm, logCPUE_Gadus_chalcogrammus))
p4 + geom_smooth() + geom_point()

p5 <- ggplot(wide_join, aes(south.sst.amj, logCPUE_Gadus_chalcogrammus))
p5 + geom_smooth() + geom_point()

#will adding these other climate covariates work? Yes
ti_temp_mod6 <- gam(logCPUE_Gadus_chalcogrammus ~ s(summer.cold.pool.extent) + s(YEAR) + s(BOT_TEMP) + s(BOT_DEPTH) +
                      ti(LONGITUDE, LATITUDE) +
                      ti(LONGITUDE, LATITUDE, YEAR, d=c(2,1)), 
                    data=wide_join)
plot(ti_temp_mod6)
gam.check(ti_temp_mod6)
summary(ti_temp_mod6)



#now with other sps===========
#as covariates


#try w five species
#shared vs sps specific?
#picked somewhat haphazardly pollock, opilio, p cod, arrowtooth, yellowfin sole
five_dat <- early_dat[which(early_dat$SCIENTIFIC=="Gadus chalcogrammus"|early_dat$SCIENTIFIC=="Chionoecetes opilio"|
                              early_dat$SCIENTIFIC=="Gadus macrocephalus"|early_dat$SCIENTIFIC=="Atheresthes stomias"|
                              early_dat$SCIENTIFIC=="Limanda aspera"),]

p1 <- ggplot(five_dat, aes(YEAR, logCPUE))
p1+ geom_point() + geom_smooth() + facet_wrap(~SCIENTIFIC) 



#GAMS without year=====================================================================

#Mike asked me to also try these with no main effect of year

noy1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP), 
              data=analysis_dat) #k too low


noy2 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE), 
              data=analysis_dat) #k too low
# noy2k <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#               t2(LONGITUDE, LATITUDE, k=20), 
#             data=analysis_dat)
# gam.check(noy2k) #k too low
# noy2k2 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                t2(LONGITUDE, LATITUDE, k=23), 
#              data=analysis_dat)
# gam.check(noy2k2)#good k, bad hessian
noy2k3 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE, k=21), 
              data=analysis_dat)
gam.check(noy2k3) #GOOD
plot(noy2k3)
AIC(small2k_import, noy2k3) #w year still much better

AIC(small1, small2, noy1, noy2) #models with year better so far

noy3 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, by=factor(YEAR)), 
              data=analysis_dat)


noy4 <- gam(logCPUE_Gadus_chalcogrammus ~ s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, BOT_TEMP), 
              data=analysis_dat) #k too low


noy4.5 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                  t2(LONGITUDE, LATITUDE, BOT_TEMP), 
                data=analysis_dat) #k too low

noy4.6 <- gam(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
                  t2(LONGITUDE, LATITUDE, BOT_TEMP), 
                data=analysis_dat)
summary(noy4.6)
gam.check(noy4.6)#k too low

noy5 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE, by=factor(YEAR)), 
              data=analysis_dat) #bad hessian, k too low
noy5 <- bam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LONGITUDE, LATITUDE, by=factor(YEAR), k=20), 
            data=analysis_dat) #


#GAMs w year random===================================================================================
#random intercept, will deal with mean diff among years but allow out of sample prediction

ran1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP), random=list(YEAR_factor=~1),
            data=analysis_dat) #AIC is 44471.99 compared to 44364.46 for small1
summary(ran1)
summary(ran1[[1]])
summary(ran1[[2]])
plot(ran1[[2]])
gam.check(ran1[[2]]) #looks pretty bad

ran2 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LONGITUDE, LATITUDE), random=list(YEAR_factor=~1), 
            data=analysis_dat)
summary(ran2[[1]]) #AIC 40857.67
summary(ran2[[2]])
plot(ran2[[2]])
gam.check(ran2[[2]]) #k too low
# ran2k <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                t2(LONGITUDE, LATITUDE, k=17), random=list(YEAR_factor=~1), 
#              data=analysis_dat)
# gam.check(ran2k[[2]])#k too low
# ran2k2 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                 t2(LONGITUDE, LATITUDE, k=18), random=list(YEAR_factor=~1), 
#               data=analysis_dat)
# gam.check(ran2k2[[2]]) #k too low
# ran2k3 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=19), random=list(YEAR_factor=~1), 
#                data=analysis_dat)
# gam.check(ran2k3[[2]]) #k too low
# ran2k4 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=20), random=list(YEAR_factor=~1), 
#                data=analysis_dat)
# gam.check(ran2k4[[2]])#k too low
# ran2k5 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=21), random=list(YEAR_factor=~1), 
#                data=analysis_dat)
# gam.check(ran2k5[[2]]) #k too low
# ran2k6 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=22), random=list(YEAR_factor=~1), 
#                data=analysis_dat)
# gam.check(ran2k6[[2]]) #k too low
# ran2k7 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=23), random=list(YEAR_factor=~1), 
#                data=analysis_dat)
# gam.check(ran2k7[[2]]) #k too low
# ran2k8 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=24), random=list(YEAR_factor=~1), 
#                data=analysis_dat)
# gam.check(ran2k8[[2]]) #k too low
# ran2k9 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=25), random=list(YEAR_factor=~1), 
#                data=analysis_dat)
# gam.check(ran2k9[[2]]) #k too low
# ran2k10 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=30), random=list(YEAR_factor=~1), 
#                data=analysis_dat) #k too low
# gam.check(ran2k10[[2]])
# ran2k11 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                   t2(LONGITUDE, LATITUDE, k=35), random=list(YEAR_factor=~1), 
#                 data=analysis_dat)
# gam.check(ran2k11[[2]]) #k too low
# ran2k12 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                   t2(LONGITUDE, LATITUDE, k=40), random=list(YEAR_factor=~1), 
#                 data=analysis_dat)
# gam.check(ran2k12[[2]]) #k still too low!
# ran2k13 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                   t2(LONGITUDE, LATITUDE, k=45), random=list(YEAR_factor=~1), 
#                 data=analysis_dat)
# gam.check(ran2k13[[2]]) #still too low
ran2k14 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                  t2(LONGITUDE, LATITUDE, k=47), random=list(YEAR_factor=~1), 
                data=analysis_dat)
gam.check(ran2k14[[2]]) #still too low, hmmm might be something wrong
plot(ran2k14[[2]])
vis.gam(ran2k14[[2]], view=c("LATITUDE", "LONGITUDE"))
big2 <- getViz(ran2k14[[2]])
plot(sm(big2, 2))
AIC(small2k_import, noy2k3, ran2k14[[1]]) #seems worse than other models, not sure if this is whole model AIC

ran3 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, by=factor(YEAR)), random=list(YEAR_factor=~1), 
            data=analysis_dat) #singularity in backsolve error, looking into stack overflow suggests this is lack of main year effect?


ran4 <- gamm(logCPUE_Gadus_chalcogrammus ~ s(BOT_TEMP) +
              t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, BOT_TEMP), random=list(YEAR_factor=~1), 
            data=analysis_dat)
#singular convergence error

ran4.5 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LONGITUDE, LATITUDE, BOT_TEMP), random=list(YEAR_factor=~1), 
              data=analysis_dat)
summary(ran4.5)
summary(ran4.5[[1]])#AIC 40498.14
summary(ran4.5[[2]])
plot(ran4.5[[2]])
gam.check(ran4.5[[2]]) #k too low

ran4.6 <- gamm(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
                t2(LONGITUDE, LATITUDE, BOT_TEMP), random=list(YEAR_factor=~1), 
              data=analysis_dat)
#Singularity in backsolve

ran5 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LONGITUDE, LATITUDE, by=factor(YEAR)), random=list(YEAR_factor=~1), 
            data=analysis_dat) #reaches iteration limit without convergence

ran6 <- gamm(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
               t2(LONGITUDE, LATITUDE, by=factor(YEAR)), random=list(YEAR_factor=~1), 
             data=analysis_dat) #ktoo low
ran6 <- gamm(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
               t2(LONGITUDE, LATITUDE, by=factor(YEAR), k=18), random=list(YEAR_factor=~1), 
             data=analysis_dat) #ktoo low


#random year with gam()=======
#if you have R version >4.0
# install.packages(install.packages("https://jacolienvanrij.com/Rpackages/itsadug/package/itsadug_2.4.tar.gz", repos=NULL))

#if you have R version <4.0
# require(devtools)
# install_version("itsadug", version = "2.3", repos = "http://cran.us.r-project.org")
library(itsadug)

rangam1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) + s(YEAR_factor, bs="re"),
             data=analysis_dat) 
summary(rangam1)
plot_smooth(rangam1, view="BOT_TEMP", plot_all = "YEAR_factor") #pretty

plot(rangam1)
gam.check(rangam1) #GOOD


rangam2 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
               t2(LONGITUDE, LATITUDE)+ s(YEAR_factor, bs="re"), 
             data=analysis_dat)
summary(rangam2) #
plot(rangam2)
gam.check(rangam2) #k too low
plot_smooth(rangam2, view="BOT_TEMP", plot_all = "YEAR_factor") 
plot_smooth(rangam2, view="LATITUDE", plot_all = "YEAR_factor") 
plot_smooth(rangam2, view="LONGITUDE", plot_all = "YEAR_factor")
r2 <- getViz(rangam2)
plot(sm(r2, 2), residuals = TRUE)
plot(sm(r2, 1), fix = c("YEAR" = 2000), residuals = TRUE)
print(plot(r2, allTerms = T), pages = 1)


#RERUN THE NEXT THREE!!
rangam3 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
               t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, by=factor(YEAR_factor))+ s(YEAR, bs="re"), 
             data=analysis_dat) 
gam.check(rangam3)#bad hessian, k too low

rangam4 <- gam(logCPUE_Gadus_chalcogrammus ~ s(BOT_TEMP) +
               t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, BOT_TEMP)+ s(YEAR_factor, bs="re"), 
             data=analysis_dat)
gam.check(rangam4)#k too low


rangam4.5 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                 t2(LONGITUDE, LATITUDE, BOT_TEMP)+ s(YEAR_factor, bs="re"), 
               data=analysis_dat)
gam.check(rangam4.5) #k too low bad hessian

rangam4.6 <- gam(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
                 t2(LONGITUDE, LATITUDE, BOT_TEMP)+ s(YEAR_factor, bs="re"), 
               data=analysis_dat)
gam.check(rangam4.6) # k too low




rangam5 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
               t2(LONGITUDE, LATITUDE, by=factor(YEAR))+ s(YEAR_factor, bs="re"), 
             data=analysis_dat) 
gam.check(rangam5) #bad hessian, k too low

rangam6 <- gam(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
               t2(LONGITUDE, LATITUDE, by=factor(YEAR))+ s(YEAR_factor, bs="re"), 
             data=analysis_dat)
summary(rangam6)
gam.check(rangam6) #bad hessian, low k except for yr
plot(rangam6)
rangam6k <- gam(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
               t2(LONGITUDE, LATITUDE, by=factor(YEAR), k=18)+ s(YEAR_factor, bs="re"), 
             data=analysis_dat)




