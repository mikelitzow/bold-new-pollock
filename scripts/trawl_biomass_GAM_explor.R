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
library(tidyverse)



sel.trawl.dat <- read.csv("data/select_trawl_dat.csv", stringsAsFactors = FALSE, row.names = 1)

sel.trawl.dat$YEAR_factor <- as.factor(sel.trawl.dat$YEAR)
sel.trawl.dat$BOT_TEMP[which(sel.trawl.dat$BOT_TEMP=="-9999")]<-NA
sel.trawl.dat$SURF_TEMP[which(sel.trawl.dat$SURF_TEMP=="-9999")]<-NA
sel.trawl.dat$WTCPUE[which(sel.trawl.dat$WTCPUE=="-9999")]<-NA
sel.trawl.dat$NUMCPUE[which(sel.trawl.dat$NUMCPUE=="-9999")]<-NA

sel.trawl.dat$LATITUDE  <- as.numeric(sel.trawl.dat$LATITUDE)
sel.trawl.dat$LONGITUDE <- as.numeric(sel.trawl.dat$LONGITUDE)
sel.trawl.dat$YEAR <- as.numeric(sel.trawl.dat$YEAR )
sel.trawl.dat$WTCPUE <- as.numeric(sel.trawl.dat$WTCPUE)
sel.trawl.dat$NUMCPUE <- as.numeric(sel.trawl.dat$NUMCPUE)
sel.trawl.dat$BOT_DEPTH <- as.numeric(sel.trawl.dat$BOT_DEPTH )
sel.trawl.dat$BOT_TEMP <- as.numeric(sel.trawl.dat$BOT_TEMP)
sel.trawl.dat$SURF_TEMP <- as.numeric(sel.trawl.dat$SURF_TEMP)


sel.trawl.dat$logCPUE <- log(sel.trawl.dat$WTCPUE + 1)

#convert latitude & longitude to x/y coordinates that reflect 
# actual distances using the equal-distance Albers projection:
# (I chose the parameters based on a 'rule of thumb' in the help file)
x <- mapproject(sel.trawl.dat$LONGITUDE, sel.trawl.dat$LATITUDE, "albers", param=c(55.9, 60.8))
sel.trawl.dat$long_albers <- x$x
sel.trawl.dat$lat_albers <- x$y




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




#plot covars======================================================================================

plot(early_wide$YEAR, early_wide$BOT_TEMP)
plot(early_wide$YEAR, early_wide$SURF_TEMP)

plot( early_wide$BOT_TEMP, early_wide$logCPUE_Gadus_chalcogrammus)
plot( early_wide$BOT_DEPTH, early_wide$logCPUE_Gadus_chalcogrammus)
plot( early_wide$SURF_TEMP, early_wide$logCPUE_Gadus_chalcogrammus)


ggplot(early_wide, aes(YEAR, BOT_TEMP)) + geom_point()



#plot climate variables========
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
 
 resapend <- analysis_dat[which(is.na(analysis_dat$BOT_TEMP)==FALSE &
            is.na(analysis_dat$logCPUE_Gadus_chalcogrammus)==FALSE),] #missing bottom temps leads to difference 
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

#try refitting with gamm so I can get a variogram
gamm2k9 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=10) +
                  t2(LONGITUDE, LATITUDE, k=20), random=list(YEAR_factor=~1), 
                data=analysis_dat)

mmvar <- Variogram(gamm2k9$lme, form=~ LONGITUDE + LATITUDE, nugget=TRUE, data=analysis_dat)
plot(mmvar)
gam.check(gamm2k9[[2]]) #k little too low but keep same for now to keep parallel w previous


resapend$r <- resid(gamm2k9[[2]])   # Extract residuals
coordinates(resapend) <- c("LONGITUDE","LATITUDE")
j <- resapend$YEAR == 2010  # Extract 2010 data only
bubble(resapend[j,], zcol="r")
k <- resapend$YEAR == 1982  # Extract 2010 data only
bubble(resapend[k,], zcol="r")
l <- resapend$YEAR == 1990  # Extract 2010 data only
bubble(resapend[l,], zcol="r")
m <- resapend$YEAR == 2000  # Extract 2010 data only
bubble(resapend[m,], zcol="r")


# examine autocorrelation for one year at a time because the spatial 
# autocorrelation, if present, should be evident within years only 

r <- resid(gamm2k9[[2]])[j]   # Extract residuals
# Compute pairwise distances among logations based on distances in 'x'
d <- dist(coordinates(resapend)[j,])
d  # Large matrix of pairwise distances (values below diagonal only!)
d <- as.vector(d)   # Need to convert to a vector for 'Variogram' function
# The vector d, contains only one set of pairwise distances (values from below
# the diagonal) and has length: n * (n-1) / 2, where n is the number of observations
# (see help files for 'dist' and 'Variogram')

SemiVar <- Variogram(r, d)
head(SemiVar, 10) 

plot(SemiVar, xlim=c(0,0.1))
plot(SemiVar, xlim=c(0,25))

bins <- cut(SemiVar$dist, seq(0,25, by=1))  
plot(bins, SemiVar$variog)

cgmod <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=10) +
                  t2(LONGITUDE, LATITUDE, k=20), random=list(YEAR_factor=~1), 
              correlation = corGaus(form=~ LONGITUDE + LATITUDE, nugget=TRUE),
                data=analysis_dat)
plot(Variogram(cgmod$lme, form=~ LONGITUDE + LATITUDE, nugget=TRUE, data=analysis_dat))
gam.check(cgmod[[2]])
summary(cgmod[[1]]) #AIC 35898.18 compared to 37302.43 for rangam2k9 or  37859.72 for gamm2k9
csmod <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=10) +
                t2(LONGITUDE, LATITUDE, k=20), random=list(YEAR_factor=~1), 
              correlation = corSpher(form=~ LONGITUDE + LATITUDE, nugget=TRUE),
              data=analysis_dat) #no covergence
crmod <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=10) +
                t2(LONGITUDE, LATITUDE, k=20), random=list(YEAR_factor=~1), 
              correlation = corRatio(form=~ LONGITUDE + LATITUDE, nugget=TRUE),
              data=analysis_dat)
plot(Variogram(crmod$lme, form=~ LONGITUDE + LATITUDE, nugget=TRUE, data=analysis_dat))
gam.check(crmod[[2]])
summary(crmod[[1]]) #AIC 35786.53
cemod <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, k=10) +
                t2(LONGITUDE, LATITUDE, k=20), random=list(YEAR_factor=~1), 
              correlation = corExp(form=~ LONGITUDE + LATITUDE, nugget=TRUE),
              data=analysis_dat)
plot(Variogram(cemod$lme, form=~ LONGITUDE + LATITUDE, nugget=TRUE, data=analysis_dat))
gam.check(cemod[[2]])
summary(cemod[[1]]) #AIC 35777.97


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





#Spatial cor models w/o spatial surface==========================================================================

cg1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP), random=list(YEAR_factor=~1), 
              correlation = corGaus(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=analysis_dat)
plot(Variogram(cg1$lme, form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE, data=analysis_dat))
gam.check(cg1[[2]])
summary(cg1[[1]]) #AIC 36951.44

cggam1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) + s(YEAR_factor, bs="re"), 
              correlation = corGaus(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=analysis_dat)
gam.check(cggam1)
plot(cggam1)
AIC(cggam1) #41780.86

cs1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP), random=list(YEAR_factor=~1), 
              correlation = corSpher(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=analysis_dat) #didn't converge

cr1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP), random=list(YEAR_factor=~1), 
              correlation = corRatio(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=analysis_dat)
plot(Variogram(cr1$lme, form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE, data=analysis_dat))
gam.check(cr1[[2]]) #
summary(cr1[[1]]) #AIC 36831.98

crgam1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) + s(YEAR_factor, bs="re"), 
              correlation = corRatio(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=analysis_dat)
gam.check(crgam1)
plot(crgam1)
AIC(crgam1) #41780.86


ce1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP), random=list(YEAR_factor=~1), 
              correlation = corExp(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=analysis_dat)
plot(Variogram(ce1$lme, form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE, data=analysis_dat))
gam.check(ce1[[2]])
summary(ce1[[1]])
plot(ce1[[2]])

cegam1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP) + s(YEAR_factor, bs="re"), 
               correlation = corExp(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
               data=analysis_dat)
gam.check(cegam1)
plot(cegam1)
AIC(cegam1) #41780.86



#cor models w both periods========================================================================================

#both periods inclusion criteria

both_dat <- sel.trawl.dat
both_dat <- both_dat[,-c(9,11)] #drop columns that repeat info
both_dat <- both_dat[!duplicated(both_dat),] #few duplicated rows, remove them

#widen dataframe to have columns for each sps

both_wide <-both_dat %>% pivot_wider(names_from=SCIENTIFIC, 
                                        values_from=c(WTCPUE, NUMCPUE, logCPUE)) #
#eek this causes some bad column names!
both_wide <- both_wide %>% rename(WTCPUE_Chionoecetes_bairdi = "WTCPUE_Chionoecetes bairdi",            
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

station_summary_both <- both_wide %>% group_by(STATION, LONGITUDE, LATITUDE) %>%
  summarize(n_yrs=n())

station_summary2_both <- both_wide %>% group_by(STATION) %>%
  summarize(n_yrs=n())

joinboth <- left_join(both_wide, station_summary2_both)

periods_dat <- joinboth[which(joinboth$n_yrs>5),]

periods_dat$period <- NA

periods_dat$period[which(periods_dat$YEAR<2014)] <- "early"
periods_dat$period[which(periods_dat$YEAR>2013)] <- "late"

#deal w missing data=================

table(periods_dat$YEAR[which(is.na(periods_dat$BOT_TEMP)==TRUE)])
#all years have some NAs in bottom temp, a LOT in 1994

table(periods_dat$YEAR[which(is.na(periods_dat$BOT_TEMP)==FALSE)])
#but far far more with data, even in 94

tempmod82 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=periods_dat[which(periods_dat$YEAR==1982),] )
summary(tempmod82)
gam.check(tempmod82)
plot(tempmod82)

tempmod83 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=periods_dat[which(periods_dat$YEAR==1983),] )
summary(tempmod83)
gam.check(tempmod83)
plot(tempmod83)

tempmod84 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=periods_dat[which(periods_dat$YEAR==1984),] )
summary(tempmod84)
gam.check(tempmod84)
plot(tempmod84)

tempmod85 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1985),] )
summary(tempmod85)
gam.check(tempmod85)
plot(tempmod85)

tempmod86 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1986),] )
summary(tempmod86)
gam.check(tempmod86)
plot(tempmod86)

tempmod87 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=periods_dat[which(periods_dat$YEAR==1987),] )
summary(tempmod87)
gam.check(tempmod87)
plot(tempmod87)

tempmod88 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=8), data=periods_dat[which(periods_dat$YEAR==1988),] )
summary(tempmod88)
gam.check(tempmod88)
plot(tempmod88)

tempmod89 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1989),] )
summary(tempmod89)
gam.check(tempmod89)
plot(tempmod89)

tempmod90 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1990),] )
summary(tempmod90)
gam.check(tempmod90)
plot(tempmod90)

tempmod91 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1991),]  )
summary(tempmod91)
gam.check(tempmod91)
plot(tempmod91)

tempmod92 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1992),]  )
summary(tempmod92)
gam.check(tempmod92)
plot(tempmod92)

tempmod93 <- gam(BOT_TEMP ~ s(BOT_DEPTH, k=17) + ti(long_albers, lat_albers, k=8), data=periods_dat[which(periods_dat$YEAR==1993),]  )
summary(tempmod93)
gam.check(tempmod93) #seems awfully high
plot(tempmod93) 

tempmod94 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=7), data=periods_dat[which(periods_dat$YEAR==1994),]  )
summary(tempmod94)
gam.check(tempmod94)
plot(tempmod94)

tempmod95 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1995),]  )
summary(tempmod95)
gam.check(tempmod95)
plot(tempmod95)

tempmod96 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1996),]  )
summary(tempmod96)
gam.check(tempmod96)
plot(tempmod96)

tempmod97 <- gam(BOT_TEMP ~ s(BOT_DEPTH, k=8) + ti(long_albers, lat_albers, k=11), data=periods_dat[which(periods_dat$YEAR==1997),]  )
summary(tempmod97)
gam.check(tempmod97)
plot(tempmod97)

tempmod98 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1998),]  )
summary(tempmod98)
gam.check(tempmod98)
plot(tempmod98)

tempmod99 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers, k=9), data=periods_dat[which(periods_dat$YEAR==1999),]  )
summary(tempmod99)
gam.check(tempmod99)
plot(tempmod99)

tempmod00 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2000),]  )
summary(tempmod00)
gam.check(tempmod00)

tempmod01 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2001),]  )
summary(tempmod01)
gam.check(tempmod01)

tempmod02 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2002),]  )
summary(tempmod02)
gam.check(tempmod02)

tempmod03 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2003),]  )
summary(tempmod03)
gam.check(tempmod03)

tempmod04 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2004),]  )
summary(tempmod04)
gam.check(tempmod04)

tempmod05 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2005),]  )
summary(tempmod05)
gam.check(tempmod05)

tempmod06 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2006),]  )
summary(tempmod06)
gam.check(tempmod06)

tempmod07 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2007),]  )
summary(tempmod07)
gam.check(tempmod07)

tempmod08 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2008),]  )
summary(tempmod08)
gam.check(tempmod08)

tempmod09 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2009),]  )
summary(tempmod09)
gam.check(tempmod09)

tempmod10 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2010),]  )
summary(tempmod10)
gam.check(tempmod10)

tempmod11 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2011),] )
summary(tempmod11)
gam.check(tempmod11)

tempmod12 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2012),] )
summary(tempmod12)
gam.check(tempmod12)

tempmod13 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2013),] )
summary(tempmod13)
gam.check(tempmod13)

tempmod14 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2014),] )
summary(tempmod14)
gam.check(tempmod14)

tempmod15 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2015),] )
summary(tempmod15)
gam.check(tempmod15)

tempmod16 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2016),] )
summary(tempmod16)
gam.check(tempmod16)

tempmod17 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2017),] )
summary(tempmod17)
gam.check(tempmod17)

tempmod18 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2018),] )
summary(tempmod18)
gam.check(tempmod18)

tempmod19 <- gam(BOT_TEMP ~ s(BOT_DEPTH) + ti(long_albers, lat_albers), data=periods_dat[which(periods_dat$YEAR==2019),] )
summary(tempmod19)
gam.check(tempmod19)



predtest <- predict.gam(tempmod)

#===***===***===

pg1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
            correlation = corGaus(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
            data=periods_dat)
plot(Variogram(pg1$lme, form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE, data=periods_dat))
gam.check(pg1[[2]])
summary(pg1[[1]]) #44182.43 is this for the whole model though??

pggam1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
              correlation = corGaus(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=periods_dat)
gam.check(pggam1)
plot(pggam1)
AIC(pggam1) #49624.98
summary(pggam1)

ps1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
            correlation = corSpher(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
            data=periods_dat) #

pr1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
            correlation = corRatio(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
            data=periods_dat)
plot(Variogram(pr1$lme, form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE, data=periods_dat))
gam.check(pr1[[2]]) #
summary(cr1[[1]]) #

prgam1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
              correlation = corRatio(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=periods_dat)
gam.check(prgam1)
plot(prgam1)
AIC(prgam1) #


pe1 <- gamm(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, by=as.factor(period), bs="fs"), random=list(YEAR_factor=~1), 
            correlation = corExp(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
            data=periods_dat)
plot(Variogram(pe1$lme, form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE, data=periods_dat))
gam.check(pe1[[2]])
summary(pe1[[1]])
plot(pe1[[2]])

pegam1 <- gam(logCPUE_Gadus_chalcogrammus ~  s(BOT_TEMP, by=as.factor(period), bs="fs") + s(YEAR_factor, bs="re"), 
              correlation = corExp(form=~ LONGITUDE + LATITUDE|YEAR_factor, nugget=TRUE),
              data=periods_dat)
gam.check(pegam1)
plot(pegam1)
AIC(pegam1) #



