#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Load, clean, explore 2021 data and 1982-2019 online NEBS csv  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#found out that the publicly available 1982-2019 csv for the NEBS has more years
#and has 2018 stations (and 80s-90s stations?) automatically renamed to match standard stations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#load data-------

wd <- getwd()
nebs2021dat <- read.csv(paste(wd,"/data/survey data/GAP_BottomTrawl_SurveyData_2021/NBS2021.csv",sep=""))
sebs2021dat <- read.csv(paste(wd,"/data/survey data/GAP_BottomTrawl_SurveyData_2021/EBS2021.csv",sep=""))

nebs82_19 <- read.csv(paste(wd,"/data/survey data/nbs1982_2019.csv",sep=""))

#all same column names which is nice

newdat <- rbind(nebs2021dat, sebs2021dat, nebs82_19)

#will limit to SID 21740 but not yet











