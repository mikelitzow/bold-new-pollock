#===========================================================================================================
# MDS on full dataset
#
# Created by Krista for Mike to run on cluster
#Nov 2020
#===========================================================================================================
#Notes:
#===========================================================================================================
library(vegan)


#read in data set up in other script
wd <- getwd()
comm_csv <- read.csv(file=paste(wd,"/data/community_data_matrix.csv", sep=""), row.names = 1)

comm_mat <- as.matrix(comm_csv)

dfull <- dist(comm_mat) # euclidean distances between the rows
fitfull <- cmdscale(dfull,eig=TRUE, k=2) # k is the number of dim 

fitfull # view results
saveRDS(fitfull, file="scripts/MDS_full_output.RDS")

x <- fitfull$points[,1]
y <- fitfull$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric MDS", type="n")
text(x, y, labels = row.names(comm_mat), cex=.7)






