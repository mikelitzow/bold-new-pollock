#======================================================================================================================
#Community correlation analyses
#
#Krista, Jan 2021
#======================================================================================================================
#Notes:
#======================================================================================================================

#load packages
library(ggplot2)

#load data
full_wide <- read.csv("data/full_wide_comm_dat.csv", stringsAsFactors = FALSE, row.names = 1)

full_wide$period_num <- NA
full_wide$period_num[which(full_wide$YEAR<2014)]<-1
full_wide$period_num[which(full_wide$YEAR>2013)]<-2
full_wide$period <- as.factor(full_wide$period)

yrs <- unique(full_wide$YEAR)
#loop through species, loop through each year, get correlation and store

yr_vec <- vector(mode="numeric", length=length(yrs)*10)
sps_vec <- vector(mode="numeric", length=length(yrs)*10)
cor_vec <- vector(mode="numeric", length=length(yrs)*10)

#yrs <- unique(full_wide$YEAR)
i <- 1
counter <- 1
for(i in 1:length(yrs)){
  temp_yr <- yrs[i]
  temp_wide <- full_wide[which(full_wide$YEAR==temp_yr),]
#tempmat <- temp_wide[,c(38:48)]
tempmat <- temp_wide[,c(38:47)]

tempcor <- cor(tempmat, method = "pearson", use = "complete.obs")
#corrplot(tempcor)

  #save output
  for(l in 1:10){
  yr_vec[counter] <- temp_yr      
  sps_vec[counter] <- rownames(tempcor)[l]
  cor_vec[counter] <- tempcor[l,8]
  
  counter <- counter + 1
  }
}

cor_output <- data.frame(yr_vec, sps_vec, cor_vec)

cor_output$period <- NA
cor_output$period <- as.character(cor_output$period)
cor_output$period[which(cor_output$yr_vec<2014)]<-"early"
cor_output$period[which(cor_output$yr_vec>2013)]<- "late"
cor_output$period <- as.factor(cor_output$period)

write.csv(cor_output, file=paste(wd,"/data/community-correlations.csv", sep=""))





analysis_cor <- cor_output[which(cor_output$sps_vec!="logCPUE_Gadus_chalcogrammus"),]

plot_cor <- analysis_cor
plot_cor$Species <- NA
plot_cor$Species <- as.character(plot_cor$Species)

plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Atheresthes_stomias")] <- "Arrowtooth flounder"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Hippoglossus_stenolepis")] <- "Pacific halibut"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Hippoglossoides_elassodon")] <- "Flathead sole"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Limanda_aspera")] <- "Yellowfin sole"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Lepidopsetta_polyxystra")] <- "Northern rock sole"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Pleuronectes_quadrituberculatus")] <- "Alaska plaice"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Gadus_macrocephalus")] <- "Pacific cod"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Chionoecetes_bairdi")] <- "Tanner (bairdi) crab"
plot_cor$Species[which(plot_cor$sps_vec=="logCPUE_Chionoecetes_opilio")] <- "Snow (opilio) crab"


ggplot(plot_cor, aes(period, cor_vec)) + geom_boxplot() + facet_wrap(~Species) + 
  geom_hline(yintercept = 0, col="red") + ylab("Correlation") + xlab("Period")

cormod <- lm(cor_vec ~ period, data = analysis_cor)
anova(cormod)

#need to do this by sps
# t.test() function makes the assumption that the variances of the two groups of samples, 
# being compared, are different. Therefore, Welch t-test is performed by default.

t_As <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Atheresthes_stomias"),])
t_As #not sig p-value = 0.2016
p_As <- t_As$p.value

t_Hs <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Hippoglossus_stenolepis"),])
t_Hs #p=0.02326
p_Hs <- t_Hs$p.value

t_He <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Hippoglossoides_elassodon"),])
t_He #p=0.0.04584
p_He <- t_He$p.value

t_La <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Limanda_aspera"),])
t_La #not sig p-value = 0.1204
p_La <- t_La$p.value

t_Lp <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Lepidopsetta_polyxystra"),])
t_Lp #not sig p-value = 0.6489
p_Lp <- t_Lp$p.value

t_Pq <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Pleuronectes_quadrituberculatus"),])
t_Pq #p=0.3.15e-05
p_Pq <- t_Pq$p.value

t_Gm <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Gadus_macrocephalus"),])
t_Gm #NOT SIG p=0.1737
p_Gm <- t_Gm$p.value

t_Cb <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Chionoecetes_bairdi"),])
t_Cb #Not sig p=0.05511
p_Cb <- t_Cb$p.value

t_Co <- t.test(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Chionoecetes_opilio"),])
t_Co #not sig p-value = 0.8567
p_Co <- t_Co$p.value

ggplot(cor_output, aes(yr_vec, cor_vec, colour=sps_vec)) + geom_point() + geom_smooth() + facet_wrap(~sps_vec) + 
  geom_hline(yintercept = 0, col="red")


#FDR control=================

#following Verhoeven et al 2005 Oikos to control false discovery rate

m<- length(unique(plot_cor$Species))
alpha <- 0.05

pvec <- as.vector(c(p_As, p_Hs, p_He, p_La, p_Lp, p_Pq, p_Gm, p_Cb, p_Co))

pvec <- sort(pvec, decreasing=FALSE)

i<-1
for(i in 1:m){
  temp_p <- pvec[i]
  print(temp_p)
  temp_value <- (alpha/m)*i
  print(temp_value)
  print(temp_p < temp_value)
}
#Only p_Pq is significant



#maps=====

library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")



world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Atheresthes_stomias), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Hippoglossus_stenolepis), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Hippoglossoides_elassodon), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Limanda_aspera), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Lepidopsetta_polyxystra), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Pleuronectes_quadrituberculatus), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Gadus_macrocephalus), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Gadus_chalcogrammus), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Chionoecetes_bairdi), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Chionoecetes_opilio), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  



ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, colour=logCPUE_Lepidopsetta_sp), data=full_wide) +   
  scale_colour_gradient2(low="blue", high="red", guide="colorbar") + facet_wrap(~period)  












