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


#loop through species, loop through each year, get correlation and store

yr_vec <- vector(mode="numeric", length=length(yrs)*10)
sps_vec <- vector(mode="numeric", length=length(yrs)*10)
cor_vec <- vector(mode="numeric", length=length(yrs)*10)

yrs <- unique(full_wide$YEAR)
i <- 1
counter <- 1
for(i in 1:length(yrs)){
  temp_yr <- yrs[i]
  temp_wide <- full_wide[which(full_wide$YEAR==temp_yr),]
#tempmat <- temp_wide[,c(38:48)]
tempmat <- temp_wide[,c(38:47)]

tempcor <- cor(tempmat, method = "pearson", use = "complete.obs")
corrplot(tempcor)

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
cormod_As <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Atheresthes_stomias"),])
anova(cormod_As) #not sig

cormod_Hs <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Hippoglossus_stenolepis"),])
anova(cormod_Hs) #p=0.02

cormod_He <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Hippoglossoides_elassodon"),])
anova(cormod_He) #p=0.004

cormod_La <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Limanda_aspera"),])
anova(cormod_La) #not sig

cormod_Lp <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Lepidopsetta_polyxystra"),])
anova(cormod_Lp) #not sig

cormod_Pq <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Pleuronectes_quadrituberculatus"),])
anova(cormod_Pq) #p=0.005

cormod_Gm <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Gadus_macrocephalus"),])
anova(cormod_Gm) #p=0.04

cormod_Cb <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Chionoecetes_bairdi"),])
anova(cormod_Cb) #p=0.002

cormod_Co <- lm(cor_vec ~ period, data = analysis_cor[which(analysis_cor$sps_vec=="logCPUE_Chionoecetes_opilio"),])
anova(cormod_Co) #not sig

ggplot(cor_output, aes(yr_vec, cor_vec, colour=sps_vec)) + geom_point() + geom_smooth() + facet_wrap(~sps_vec) + 
  geom_hline(yintercept = 0, col="red")

