#===========================================================================================================
# MDS on full dataset
#
# Created by Krista for Mike to run on cluster
#Nov 2020
#===========================================================================================================
#Notes:
#===========================================================================================================

#read in data set up in other script
wd <- getwd()
comm_csv <- read.csv(file=paste(wd,"/data/community_data_matrix.csv", sep=""), row.names = 1)

comm_mat <- as.matrix(comm_csv)








