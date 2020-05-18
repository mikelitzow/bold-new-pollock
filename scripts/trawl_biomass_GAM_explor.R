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

#try w just pollock first

#proof of concept gams=====
#confirm whether these should be te() or t2() or s()
yr_int <- gam(logCPUE ~ YEAR_factor + s(LATITUDE, LONGITUDE, YEAR), 
                      data=sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_int)
plot(yr_int)
vis.gam(yr_int, view=c("LATITUDE","LONGITUDE"))
#look at predict.gam

yr_botT_int <- gam(logCPUE ~ YEAR_factor + BOT_TEMP + s(LATITUDE, LONGITUDE, YEAR), 
              data=sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_botT_int)
plot(yr_botT_int)
vis.gam(yr_botT_int, view=c("LATITUDE","LONGITUDE"))

yr_botTD_int <- gam(logCPUE ~ YEAR_factor + BOT_TEMP + BOT_DEPTH + s(LATITUDE, LONGITUDE, YEAR), 
                   data=sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),])
summary(yr_botTD_int)
plot(yr_botTD_int)
vis.gam(yr_botTD_int, view=c("LATITUDE","LONGITUDE"))


#should likely be a tensor product

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




#now with other sps===========
#as covariates
spscovar1 <- gam(logCPUE_Gadus_chalcogrammus ~ YEAR_factor + 
logCPUE_Chionoecetes_bairdi + logCPUE_Atheresthes_stomias +           
                logCPUE_Hippoglossus_stenolepis + logCPUE_Limanda_aspera +             
                 logCPUE_Lepidopsetta_sp + logCPUE_Chionoecetes_opilio +            
                 logCPUE_Gadus_macrocephalus + logCPUE_Hippoglossoides_elassodon +    
                 logCPUE_Pleuronectes_quadrituberculatus + logCPUE_Lepidopsetta_polyxystra +   
                   ti(LATITUDE, LONGITUDE, YEAR, d=c(2,1)), 
              data=early_wide)
summary(spscovar1)
plot.gam(spscovar1)
gam.check(spscovar1)
vis.gam(spscovar1, view=c("LATITUDE","LONGITUDE"))
vis.gam(spscovar1,view=c("LATITUDE","LONGITUDE"), cond=list(YEAR=1990) )
vis.gam(spscovar1,view=c("LATITUDE","LONGITUDE"), cond=list(YEAR=2010) )

spscovar1 <- gam("logCPUE_Gadus chalcogrammus" ~ YEAR_factor + 
                  # "logCPUE_Chionoecetes bairdi" + "logCPUE_Atheresthes stomias" +           
                   # "logCPUE_Hippoglossus stenolepis" + "logCPUE_Limanda aspera" +             
                   # "logCPUE_Lepidopsetta sp." + "logCPUE_Chionoecetes opilio" +            
                   # "logCPUE_Gadus macrocephalus" + "logCPUE_Hippoglossoides elassodon" +    
                   # "logCPUE_Pleuronectes quadrituberculatus" + "logCPUE_Lepidopsetta polyxystra" +   
                   ti(LATITUDE, LONGITUDE, YEAR, d=c(2,1)), 
                 data=early_wide)

#what about multivariate?
#I think I need to use the long version of the dataset again
tiny_dat <- early_dat[which(early_dat$SCIENTIFIC=="Gadus chalcogrammus"|early_dat$SCIENTIFIC=="Chionoecetes opilio"),]

mmod <- gam(logCPUE ~ s(YEAR, by=SCIENTIFIC) +  s(LATITUDE, LONGITUDE, by=SCIENTIFIC) +
                   ti(LATITUDE, LONGITUDE, YEAR, by=SCIENTIFIC), 
                 data=tiny_dat)
summary(mmod)
plot(mmod)
gam.check(mmod)

