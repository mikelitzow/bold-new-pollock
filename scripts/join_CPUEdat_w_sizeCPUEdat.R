#=======================================================================================================================================
# Join new size CPUE data to CPUE data
#
# Created by Krista, June 2020
#=======================================================================================================================================
# Notes:
#=======================================================================================================================================

library(ggplot2)
library(tidyverse)
library(corrplot)
library(mgcv)
library(nlme)
library(mgcViz)
library(tidyverse)
library(mapproj)
library(visreg)



sex.dat <- read.csv("data/survey data/poll_cpue_by_sex_cm.csv", stringsAsFactors = TRUE)

sex.dat$YEAR_factor <- as.factor(sex.dat$YEAR)
sex.dat$SEX <- as.factor(sex.dat$SEX)
sex.dat$LENGTH[which(sex.dat$LENGTH=="-9")]<-NA
sex.dat$SEX[which(sex.dat$SEX=="-9")]<-NA

indiv_pollock <- read.csv("data/survey data/poll_specimen_haul.csv", stringsAsFactors = TRUE)

indiv_pollock$SEX <- as.factor(indiv_pollock$SEX)
indiv_pollock$AGE <- as.factor(indiv_pollock$AGE)

indiv_pollock <- indiv_pollock %>%
  separate(CRUISE, 
           c("Year", "cruisenum"), 
           sep=cumsum(c(4,2)))
indiv_pollock$Year <- as.factor(indiv_pollock$Year)

#sel.trawl.dat is loaded in trawl_biomass_GAM_explor.R

head(sel.trawl.dat)
head(sex.dat)

trawljoin <- left_join(sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(1:6,9:18,20:21)], 
                      sex.dat)


#seems to catch all sex.dat rows except ~20K rows w missing STRATUM values
fuzziertestjoin <- left_join(sel.trawl.dat[which(sel.trawl.dat$SCIENTIFIC=="Gadus chalcogrammus"),c(1:6,9:18,20:21)], 
                                 sex.dat, by = c("LATITUDE", "LONGITUDE", "YEAR", "VESSEL", "HAUL"))
length(fuzziertestjoin$YEAR) #no new rows


View(sex.dat[is.na(sex.dat$STRATUM),])

#for now going to proceed - the rows that are not matching actually don't seem to have a good match




#bin into size classes========================================================================================

hist(testjoin$LENGTH)

#binning following Thorson et al 2017 Fish Fisheries
#0-20cm = age 1
#21-30 = age 2
#31-40, 41-50, 50+

trawljoin$bin <- NA
trawljoin$bin[which(trawljoin$LENGTH<=200)]<- "0-200"
trawljoin$bin[which(trawljoin$LENGTH>200 & trawljoin$LENGTH<=300)]<- "200-300"
trawljoin$bin[which(trawljoin$LENGTH>300 & trawljoin$LENGTH<=400)]<- "300-400"
trawljoin$bin[which(trawljoin$LENGTH>400 & trawljoin$LENGTH<=500)]<- "400-500"
trawljoin$bin[which(trawljoin$LENGTH>500)]<- "500+"

#certain number have NA in both SEX and LENGTH which causes some issues
trawljoin <- trawljoin[which(is.finite(trawljoin$SEX)==TRUE),]
trawljoin <- droplevels(trawljoin)

# binneddat <- trawljoin %>% group_by(YEAR, STATION, STRATUM, SCIENTIFIC, VESSEL, CRUISE, HAUL, SEX, bin) %>%
#   summarize(bin_sum_WGTCPUE_LEN=sum(WGTCPUE_LENGTH, na.rm=TRUE), bin_sum_NUMCPUE_LEN=sum(NUMCPUE_LENGTH, na.rm=TRUE),
#             n=n()) 

#try without sex
# binneddat <- trawljoin %>% group_by(YEAR, STATION, STRATUM, SCIENTIFIC, VESSEL, CRUISE, HAUL, bin) %>%
#   summarize(bin_sum_WGTCPUE_LEN=sum(WGTCPUE_LENGTH, na.rm=TRUE), bin_sum_NUMCPUE_LEN=sum(NUMCPUE_LENGTH, na.rm=TRUE),
#             n=n()) 
# #new trying to retain missing rows
# binneddat <- trawljoin %>% group_by(YEAR, STATION, STRATUM, SCIENTIFIC, VESSEL, CRUISE, HAUL, bin) %>%
#   summarize(bin_sum_WGTCPUE_LEN=sum(WGTCPUE_LENGTH, na.rm=TRUE), bin_sum_NUMCPUE_LEN=sum(NUMCPUE_LENGTH, na.rm=TRUE),
#             n=n()) 
# 
# binneddat <- trawljoin %>% group_by(YEAR, STATION, STRATUM, SCIENTIFIC, VESSEL, CRUISE, HAUL, bin) %>%
#   summarize(bin_sum_WGTCPUE_LEN=sum(WGTCPUE_LENGTH, na.rm=TRUE), bin_sum_NUMCPUE_LEN=sum(NUMCPUE_LENGTH, na.rm=TRUE),
#             n=n(), .drop = FALSE)
# 
# trawljoin$bin <- as.factor(trawljoin$bin)
# binneddat <- trawljoin %>% group_by(YEAR, STATION, STRATUM, SCIENTIFIC, VESSEL, CRUISE, HAUL, bin) %>%
#   summarize(bin_sum_WGTCPUE_LEN=sum(WGTCPUE_LENGTH, na.rm=TRUE, .drop = FALSE), 
#             bin_sum_NUMCPUE_LEN=sum(NUMCPUE_LENGTH, na.rm=TRUE, .drop = FALSE),
#             count=count(bin , .drop = FALSE))


#going to need to be a for loop instead I think

#create some output objects, separate objects by bin should be fine since analyzing separately

outlen <- length(unique(trawljoin$YEAR))*length(unique(trawljoin$HAUL))*length(unique(trawljoin$bin)) *length(unique(trawljoin$VESSEL))
#will be a little long, assumes all hauls in all years

output <- data.frame(year = character(outlen),
                      haul = character(outlen),
                     bin = character(outlen),
                     vessel = character(outlen),
                     sum_wgtCPUE_len = integer(outlen),
                      stringsAsFactors = FALSE)
yrs <- unique(trawljoin$YEAR)
bins <- unique(trawljoin$bin)

#need to add in vessel too b/c haul is not unique w/in yr

k<-1
i<-1
for(i in 1:length(yrs)){
  temp.yr <- yrs[i]
  print(temp.yr)
  
  temp_yr_dat <- trawljoin[which(trawljoin$YEAR==temp.yr),]
  yr_hauls <- unique(temp_yr_dat$HAUL)
  
  j<-1
  for(j in 1:length(yr_hauls)){
    temp_haul <- yr_hauls[j]
    temp_haul_dat <- temp_yr_dat[which(temp_yr_dat$HAUL==temp_haul),]
    temp_vessels <- unique(temp_haul_dat$VESSEL)
    
    #now sum within a haul for each bin 
    
  for(h in 1:length(temp_vessels)){   
    temp_ves <- temp_vessels[h]
    temp_ves_dat <- temp_haul_dat[which(temp_haul_dat$VESSEL==temp_ves),]
    
    for(l in 1:5){
      temp_bin <- bins[l]
      temp_bin_dat <- temp_ves_dat[which(temp_ves_dat$bin==temp_bin),]
      
      #NO NAs in WGTCPUE_LENGTH, so not going to na.rm  
      output$sum_wgtCPUE_len[k] <- sum(temp_bin_dat$WGTCPUE_LENGTH)
      output$year[k] <- temp.yr 
      output$haul[k] <- temp_haul
      output$bin[k] <- temp_bin
      output$vessel[k] <- temp_ves
      
      k<-k+1
    }}
   # k<-k+1
  }
 # k<-k+1
}

#

output$year <- as.numeric(output$year)
output$haul <- as.numeric(output$haul)
output$bin <- as.factor(output$bin)
output$vessel <- as.integer(output$vessel)

binjoin <- left_join(trawljoin[,c(1:21)], output, by=c("YEAR"="year", "HAUL"="haul", 
                                                       "VESSEL"="vessel"))
length(binjoin$YEAR)
length(output$year)

binmeta_clean <- binjoin[!duplicated(binjoin),] 

#let's move forwarrd with this and come back to troubleshoot the join

length(output$year)
length(binjoin$YEAR)

#build one col by one col, see where duplicates come in
binjoin1 <- left_join(trawljoin[,c(5,15)], output, by=c("YEAR"="year", "HAUL"="haul"))
length(output$year)
length(binjoin1$YEAR) #even this is repeating!!

#switch direction??
binjoin2 <- left_join( output, trawljoin[,c(5,15)], by=c("year" = "YEAR", "haul" = "HAUL"))
length(output$year)
length(binjoin2$year) #also repeats

mergetry <- merge(output, trawljoin[,c(5,15)], by.x= c("year", "haul"), ,by.y= c("YEAR", "HAUL"))
length(output$year)
length(mergetry$year)


#SHOULD BE ABLE TO REMOVE THIS if loop works
binjoin <- left_join(trawljoin[,c(1:21)], binneddat) #this creates a ton of duplicate rows
#I tried everything I could think of to stop them form being created but can't figure it out
#I'll just remove the duplicates, this is a bit clumbsy but kind of stuck otherwise
binmeta <- binjoin[!duplicated(binjoin),] #check length is same as binneddat, looks good



#bottom temp anom join=====


wd <- getwd()
periods_analysis_dat<- read.csv(file=paste(wd,"/data/processed_periods_analysis_data.csv", sep=""), row.names = 1)

per_sub <- periods_analysis_dat[,c(5,10,12,50:52)]
per_sub$HAUL <- as.factor(per_sub$HAUL)

binmeta_clean$HAUL <- as.factor(binmeta_clean$HAUL)

binmeta_clean_anom <- left_join(per_sub, binmeta_clean, by=c("YEAR", "HAUL", 
                                                        "VESSEL"))

length(binmeta_clean$YEAR)
length(per_sub$YEAR)

binmeta_clean <- binmeta_clean[!duplicated(binmeta_clean),] 

length(binmeta_clean$YEAR)
length(per_sub$YEAR)


#periods_analysis_dat is also loaded in trawl_biomass_GAM_explor.R
wd <- getwd()
periods_analysis_dat <- read.csv(paste(wd,"/data/processed_periods_analysis_data.csv",sep=""), row.names = 1)

periods_meta <- periods_analysis_dat[,c(1:15, 49:52)]

binjoin2 <- left_join(periods_meta, binneddat)
binmeta2 <- binjoin2[!duplicated(binjoin2),] #check length is same as binneddat
#there are some rows here that don't match for sure, return to this
#going to remove 562 rows that have NAs in all columns from binned dat, these seem to be hauls in periods_meta that have no match
#in binneddat, but do we want to retain these? Are they zeros?
binmeta2 <- binmeta2[which(is.finite(binmeta2$bin_sum_WGTCPUE_LEN)==TRUE),]
