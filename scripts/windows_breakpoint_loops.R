#====================================================================================================================
#Looping through to test different windows and breakpoints
#
#by Krista, Oct-2020
#====================================================================================================================
#Notes:
#====================================================================================================================

#read in periods_analysis_data from trawl_biomass_GAM_explor but rename
wd <- getwd()
loops_dat <- read.csv(file=paste(wd,"/data/processed_periods_analysis_data.csv", sep=""), row.names = 1)
names(loops_dat)

yrs <- unique(loops_dat$YEAR)
n_yrs <- length(yrs)
possible_breaks <- yrs[which(yrs>(1982+5) & yrs<(2019-5))]

i<-1
for(i in 1:length(possible_breaks)){
  print(possible_breaks[i])
}













