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
sel.trawl.dat$WTCPUE[which(sel.trawl.dat$WTCPUE=="-9999")]<-NA
sel.trawl.dat$NUMCPUE[which(sel.trawl.dat$NUMCPUE=="-9999")]<-NA

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
  WTCPUE_Gadus_chalcogrammus = "WTCPUE_Gadus chalcogrammus", 
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




#find 'global' best model======================================================================================




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

# rangam2k <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                  t2(LONGITUDE, LATITUDE, k=20)+ s(YEAR_factor, bs="re"), 
#                data=analysis_dat)
# gam.check(rangam2k) #k too low bad hessian
# rangam2k2 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                   t2(LONGITUDE, LATITUDE, k=21)+ s(YEAR_factor, bs="re"), 
#                 data=analysis_dat)
# gam.check(rangam2k2) #k ok bad hessian
# rangam2k3 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
#                    t2(LONGITUDE, LATITUDE, k=22)+ s(YEAR_factor, bs="re"), 
#                  data=analysis_dat)
# gam.check(rangam2k3) #still bad hessian
# rangam2k4 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=15) +
#                    t2(LONGITUDE, LATITUDE)+ s(YEAR_factor, bs="re"), 
#                  data=analysis_dat)
# gam.check(rangam2k4) #lat long k too low
# rangam2k5 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=10) +
#                    t2(LONGITUDE, LATITUDE)+ s(YEAR_factor, bs="re"), 
#                  data=analysis_dat)
# gam.check(rangam2k5)#lat long k too low
# rangam2k6 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=8) +
#                    t2(LONGITUDE, LATITUDE)+ s(YEAR_factor, bs="re"), 
#                  data=analysis_dat)
# gam.check(rangam2k6)#bad hessian
# rangam2k7 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=9) +
#                    t2(LONGITUDE, LATITUDE)+ s(YEAR_factor, bs="re"), 
#                  data=analysis_dat)
# gam.check(rangam2k7)#bad hessian
rangam2k8 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=9) +
                   t2(LONGITUDE, LATITUDE, k=25)+ s(YEAR_factor, bs="re"), 
                 data=analysis_dat) #about 1 hr run time
gam.check(rangam2k8) #GOOD
rangam2k9 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=10) +
                   t2(LONGITUDE, LATITUDE, k=20)+ s(YEAR_factor, bs="re"), 
                 data=analysis_dat) 
gam.check(rangam2k9) #GOOD
summary(rangam2k8)
plot_smooth(rangam2k8, view="BOT_TEMP", plot_all = "YEAR_factor") 
plot_smooth(rangam2k8, view="LATITUDE", plot_all = "YEAR_factor") 
plot_smooth(rangam2k8, view="LONGITUDE", plot_all = "YEAR_factor")
r2k8 <- getViz(rangam2k8)
plot(sm(r2k8, 2), residuals = TRUE)
plot(sm(r2k8, 1), fix = c("YEAR" = 2000), residuals = TRUE)
print(plot(r2k8, allTerms = T), pages = 1)

acf(residuals(rangam2k8))
pacf(residuals(rangam2k8))

res1 <- residuals(rangam2k8, type = "pearson")
 var <- variogram(res1 ~ LONGITUDE + LATITUDE, data=analysis_dat)    
 plot(var)
 
 resapend <- analysis_dat[which(is.na(analysis_dat$BOT_TEMP)==FALSE),] #missing bottom temps leads to difference 
 #in length residuals v original dataset
 resapend$residual <- res1
 z1 <- ggplot(resapend, aes(LONGITUDE, LATITUDE, colour=residual))
z1 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
#definitely looks like there is spatial patterns in residuals!

z2 <- ggplot(resapend, aes(LONGITUDE, LATITUDE, colour=BOT_DEPTH))
z2 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")

z3 <- ggplot(resapend, aes(LONGITUDE, LATITUDE, colour=BOT_TEMP))
z3 + geom_point() +   scale_colour_gradient2(low="blue", high="red", guide="colorbar")
 
library(geoR)  

coords <- matrix(0, length(resapend$logCPUE_Gadus_chalcogrammus), 2)
coords[,1] <- resapend$LONGITUDE
coords[,2] <- resapend$LATITUDE

gb <- list(data=resapend$logCPUE_Gadus_chalcogrammus, cords=coords)
plot(variog(gb))

Variogram(rangam2k8, form =~ LONGITUDE + LATITUDE, data=analysis_dat, nugget=TRUE, maxDist=200000)


#compared to no random?
noyr2k8 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=9) +
                   t2(LONGITUDE, LATITUDE, k=25), 
                 data=analysis_dat)
AIC(rangam2k8,noyr2k8) #yes better with random yr effect


AIC(rangam1, rangam2k8)

#RERUN THE NEXT THREE!!
rangam3 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
               t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, by=factor(YEAR_factor))+ s(YEAR, bs="re"), 
             data=analysis_dat) 
gam.check(rangam3)#bad hessian, k too low
rangam3k <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                 t2(LONGITUDE, LATITUDE, k=17) + t2(LONGITUDE, LATITUDE, by=factor(YEAR_factor))+ s(YEAR, bs="re"), 
               data=analysis_dat) 
gam.check(rangam3k) #k fine, bad hessian
rangam3k2 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                  t2(LONGITUDE, LATITUDE, k=15) + t2(LONGITUDE, LATITUDE, by=factor(YEAR_factor))+ s(YEAR, bs="re"), 
                data=analysis_dat) 
gam.check(rangam3k2) #k too low, bad hessian
rangam3k3 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                   t2(LONGITUDE, LATITUDE, k=16) + t2(LONGITUDE, LATITUDE, by=factor(YEAR_factor))+ s(YEAR, bs="re"), 
                 data=analysis_dat) 
gam.check(rangam3k3) #k too low bad hessian

rangam3k4 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                   t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, by=factor(YEAR_factor), k=25)+ s(YEAR, bs="re"), 
                 data=analysis_dat) 
gam.check(rangam3k4) #memory issue
rangam3k5 <- bam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                   t2(LONGITUDE, LATITUDE) + t2(LONGITUDE, LATITUDE, by=factor(YEAR_factor), k=20)+ s(YEAR, bs="re"), 
                 data=analysis_dat) 
gam.check(rangam3k5)

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


#what about with poisson?

poi1 <- gam(WTCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) +
                   t2(LONGITUDE, LATITUDE)+ s(YEAR_factor, bs="re"), familiy="poisson",
                 data=analysis_dat) 

