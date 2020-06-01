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

# early_wide <- early_dat %>% pivot_wider(names_from=c(LATITUDE, LONGITUDE, STATION, STRATUM, YEAR, 
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

#a base model====
ti_base_mod <- gam(logCPUE_Gadus_chalcogrammus ~ s(YEAR) + s(LATITUDE, LONGITUDE) +
                     ti(LATITUDE, LONGITUDE, YEAR, d=c(2,1)), 
                   data=early_wide)
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
               data=early_wide)
summary(small1 )
plot(small1 )
gam.check(small1 ) #good on k, heteros?
#saveRDS(small1, "scripts/GAM_output/mod_output_yr_stemp.rds")

small2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE), 
              data=early_wide)
summary(small2 ) #big increase in dev explained
plot(small2 )
gam.check(small2 ) #k too low
# small2k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                 t2(LATITUDE, LONGITUDE, k=25), 
#               data=early_wide)
# gam.check(small2k) #good k, bad Hessian
small2k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                 t2(LATITUDE, LONGITUDE, k=20), 
               data=early_wide)
gam.check(small2k2) #GOOD
plot(small2k2)
# small2k3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                   t2(LATITUDE, LONGITUDE, k=15), 
#                 data=early_wide)
# gam.check(small2k3) #k too low
#saveRDS(small2k2, "scripts/GAM_output/mod_output_yr_stemp_t2lat-long.rds")

small3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE) + t2(LATITUDE, LONGITUDE, by=factor(YEAR)), 
              data=early_wide)

summary(small3 )
plot(small3 )
gam.check(small3 ) #k too low, not pos defin

small3k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE, k=20) + t2(LATITUDE, LONGITUDE, by=factor(YEAR), k=20), 
              data=early_wide) # return to this, too big

sm3 <- getViz(small3)

plot(sm(sm3, 3), fix = c("YEAR" = 2000))
plot(sm(sm3, 3), fix = c("YEAR" = 2001))
plot(sm(sm3, 3), fix = c("YEAR" = 2002))
plot(sm(sm3, 3), fix = c("YEAR" = 2003))

plot(sm(sm3, 3), fix = c("YEAR" = 1982))
plot(sm(sm3, 3), fix = c("YEAR" = 2013))

small4 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE) + t2(LATITUDE, LONGITUDE, BOT_TEMP), 
              data=early_wide)

summary(small4 )
plot(small4 )
gam.check(small4 ) #k too low
# small4k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                 t2(LATITUDE, LONGITUDE, k=20) + t2(LATITUDE, LONGITUDE, BOT_TEMP), 
#               data=early_wide)
# gam.check(small4k) #good k, bad hessian
# small4k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                  t2(LATITUDE, LONGITUDE, k=15) + t2(LATITUDE, LONGITUDE, BOT_TEMP), 
#                data=early_wide)
# gam.check(small4k2) #k too low
# small4k3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                   t2(LATITUDE, LONGITUDE, k=17) + t2(LATITUDE, LONGITUDE, BOT_TEMP), 
#                 data=early_wide)
# gam.check(small4k3) #k too low
small4k4 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                  t2(LATITUDE, LONGITUDE, k=18) + t2(LATITUDE, LONGITUDE, BOT_TEMP), 
                data=early_wide)
gam.check(small4k4) #GOOOOD!
#saveRDS(small4k4, "scripts/GAM_output/mod_output_yr_stemp_t2lat-long_t2lat-long-temp.rds")


small4.5 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                # t2(LATITUDE, LONGITUDE) + 
                  t2(LATITUDE, LONGITUDE, BOT_TEMP), 
              data=early_wide)
summary(small4.5 )
plot(small4.5 )
gam.check(small4.5 ) #k too low
# small4.5k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                   # t2(LATITUDE, LONGITUDE) + 
#                   t2(LATITUDE, LONGITUDE, BOT_TEMP, k=15), 
#                 data=early_wide)
# gam.check(small4.5k) #bad hessian, good k
# small4.5k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                    # t2(LATITUDE, LONGITUDE) + 
#                    t2(LATITUDE, LONGITUDE, BOT_TEMP, k=10), 
#                  data=early_wide)
# gam.check(small4.5k2) #k too low
# small4.5k3 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                     # t2(LATITUDE, LONGITUDE) + 
#                     t2(LATITUDE, LONGITUDE, BOT_TEMP, k=12), 
#                   data=early_wide)
# gam.check(small4.5k3) #bad hessian, good k
small4.5k4 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                    # t2(LATITUDE, LONGITUDE) + 
                    t2(LATITUDE, LONGITUDE, BOT_TEMP, k=11), 
                  data=early_wide)
gam.check(small4.5k4) #GOOD
#saveRDS(small4.5k, "scripts/GAM_output/mod_output_yr_stemp_t2lat-long-temp.rds")

small4.6 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                  # t2(LATITUDE, LONGITUDE) + 
                  t2(LATITUDE, LONGITUDE, BOT_TEMP), 
                data=early_wide)
summary(small4.6 )
plot(small4.6 )
gam.check(small4.6 ) #k is too low
# small4.6k <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
#                   # t2(LATITUDE, LONGITUDE) + 
#                   t2(LATITUDE, LONGITUDE, BOT_TEMP, k=11), 
#                 data=early_wide)
# gam.check(small4.6k) #k too low and bad Hessian
small4.6k2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                   # t2(LATITUDE, LONGITUDE) + 
                   t2(LATITUDE, LONGITUDE, BOT_TEMP, k=13), 
                 data=early_wide)
gam.check(small4.6k2) #GOOD
#saveRDS(small4.6k2, "scripts/GAM_output/mod_output_yr_temp_t2lat-long-temp.rds")


AIC(small1, small2, small3, small4) #small 3 by far best AIC, small 4 next best

E1 <- resid(small3, type="pearson")
F1 <- fitted(small3)
plot(F1, E1)

small5 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                #t2(LATITUDE, LONGITUDE, k=25) + 
                t2(LATITUDE, LONGITUDE, by=factor(YEAR)), 
              data=early_wide)

summary(small5 )
plot(small5 )
gam.check(small5 ) 


#small but ti====


small2ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LATITUDE, LONGITUDE), 
              data=early_wide)

summary(small2ti ) #
plot(small2ti )
gam.check(small2ti ) 
# small2tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                   ti(LATITUDE, LONGITUDE, k=15), 
#                 data=early_wide)
# gam.check(small2tik)#too low
small2tik2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                   ti(LATITUDE, LONGITUDE, k=17), 
                 data=early_wide)
gam.check(small2tik2) #GOOD
#saveRDS(small2tik2, "scripts/GAM_output/mod_output_yr_stemp_tilat-long.rds")

small3ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LATITUDE, LONGITUDE) + ti(LATITUDE, LONGITUDE, by=factor(YEAR)), 
              data=early_wide)

summary(small3ti )
plot(small3ti )
gam.check(small3ti ) 


small4ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LATITUDE, LONGITUDE) + ti(LATITUDE, LONGITUDE, BOT_TEMP), 
              data=early_wide)

summary(small4ti )
plot(small4ti )
gam.check(small4ti ) #k too low
small4tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                  ti(LATITUDE, LONGITUDE, k=17) + ti(LATITUDE, LONGITUDE, BOT_TEMP), 
                data=early_wide)
gam.check(small4tik) #GOOD
# small4tik2 <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
#                    ti(LATITUDE, LONGITUDE, k=15) + ti(LATITUDE, LONGITUDE, BOT_TEMP), 
#                  data=early_wide)
# gam.check(small4tik2) #k too low
#saveRDS(small4tik, "scripts/GAM_output/mod_output_yr_temp_tilat-long_tilat-long-temp.rds")

small4.5ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                  ti(LATITUDE, LONGITUDE, BOT_TEMP), 
                data=early_wide)

summary(small4.5ti )
plot(small4.5ti )
gam.check(small4.5ti )
small4.5tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                    ti(LATITUDE, LONGITUDE, BOT_TEMP, k=17), 
                  data=early_wide)
gam.check(small4.5tik) #GOOD
#saveRDS(small4.5tik, "scripts/GAM_output/mod_output_yr_stemp_tilat-long-temp.rds")

small4.6ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                  ti(LATITUDE, LONGITUDE, BOT_TEMP), 
                data=early_wide)

summary(small4.6ti )
plot(small4.6ti )
gam.check(small4.6ti ) #k is too low
small4.6tik <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + BOT_TEMP +
                    ti(LATITUDE, LONGITUDE, BOT_TEMP, k=17), 
                  data=early_wide)
gam.check(small4.6tik) #GOOD BUT CRAZY SLOW
#saveRDS(small4.6tik, "scripts/GAM_output/mod_output_yr_temp_tilat-long-temp.rds")

small5ti <- gam(logCPUE_Gadus_chalcogrammus ~ factor(YEAR) + s(BOT_TEMP) +
                ti(LATITUDE, LONGITUDE, by=factor(YEAR)), 
              data=early_wide)

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
                      ti(LATITUDE, LONGITUDE) +
                      ti(LATITUDE, LONGITUDE, YEAR, d=c(2,1)), 
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
              data=early_wide) #k too low


noy2 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE), 
              data=early_wide)

AIC(small1, small2, noy1, noy2) #models with year better so far

noy3 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE) + t2(LATITUDE, LONGITUDE, by=factor(YEAR)), 
              data=early_wide)


noy4 <- gam(logCPUE_Gadus_chalcogrammus ~ s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE) + t2(LATITUDE, LONGITUDE, BOT_TEMP), 
              data=early_wide)


noy4.5 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                  t2(LATITUDE, LONGITUDE, BOT_TEMP), 
                data=early_wide)

noy4.6 <- gam(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
                  t2(LATITUDE, LONGITUDE, BOT_TEMP), 
                data=early_wide)

noy5 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE, by=factor(YEAR)), 
              data=early_wide) #bad hessian, k too low
noy5 <- bam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LATITUDE, LONGITUDE, by=factor(YEAR), k=20), 
            data=early_wide) #


#GAMs w year random===================================================================================
#random intercept, will deal with mean diff among years but allow out of sample prediction

ran1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP), random=list(YEAR_factor=~1),
            data=early_wide) #AIC is 44471.99 compared to 44364.46 for small1
summary(ran1)
summary(ran1[[1]])
summary(ran1[[2]])
plot(ran1[[2]])
gam.check(ran1[[2]]) #looks pretty bad

ran2 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LATITUDE, LONGITUDE), random=list(YEAR_factor=~1), 
            data=early_wide)
summary(ran2[[1]]) #AIC 40857.67
summary(ran2[[2]])
plot(ran2[[2]])
gam.check(ran2[[2]]) #k too low

ran3 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LATITUDE, LONGITUDE) + t2(LATITUDE, LONGITUDE, by=factor(YEAR)), random=list(YEAR_factor=~1), 
            data=early_wide)


ran4 <- gamm(logCPUE_Gadus_chalcogrammus ~ s(BOT_TEMP) +
              t2(LATITUDE, LONGITUDE) + t2(LATITUDE, LONGITUDE, BOT_TEMP), random=list(YEAR_factor=~1), 
            data=early_wide)


ran4.5 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                t2(LATITUDE, LONGITUDE, BOT_TEMP), random=list(YEAR_factor=~1), 
              data=early_wide)

noy4.6 <- gamm(logCPUE_Gadus_chalcogrammus ~  BOT_TEMP +
                t2(LATITUDE, LONGITUDE, BOT_TEMP), random=list(YEAR_factor=~1), 
              data=early_wide)

ran5 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
              t2(LATITUDE, LONGITUDE, by=factor(YEAR)), random=list(YEAR_factor=~1), 
            data=early_wide) 



