#===========================================================================================================
# MDS on full dataset
#
# Created by Krista for Mike to run on cluster
#Nov 2020
#===========================================================================================================
#Notes:
#===========================================================================================================
library(vegan)

#control section=====

#read in data set up in other script
wd <- getwd()
comm_csv <- read.csv(file=paste(wd,"/data/community_data_matrix.csv", sep=""), row.names = 1)

comm_mat <- as.matrix(comm_csv)

#scale data
comm_std <- sweep(comm_mat, 2, apply(comm_mat, 2, max), "/")


#MDS====
dfull <- dist(comm_std) # euclidean distances between the rows
fitfull2 <- cmdscale(dfull,eig=TRUE, k=2) # k is the number of dim 
fitfull1 <- cmdscale(dfull,eig=TRUE, k=1)
fitfull3 <- cmdscale(dfull,eig=TRUE, k=3)

#fitfull # view results
saveRDS(fitfull1, file="scripts/MDS_full1_output.RDS")
saveRDS(fitfull2, file="scripts/MDS_full2_output.RDS")
saveRDS(fitfull3, file="scripts/MDS_full3_output.RDS")

#check - run only if you want
x <- fitfull1$points[,1]
y <- fitfull1$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric MDS", type="n")
text(x, y, labels = row.names(comm_std), cex=.7)

#NMDS====

meta1 <- metaMDS(comm_std, # Our community-by-species matrix
           distance = "bray",
           k=1, # The number of reduced dimensions
           na.rm=TRUE) 

meta2 <- metaMDS(comm_std, # Our community-by-species matrix
                 distance = "bray",
                 k=2, # The number of reduced dimensions
                 na.rm=TRUE) 

meta3 <- metaMDS(comm_std, # Our community-by-species matrix
                 distance = "bray",
                 k=3, # The number of reduced dimensions
                 na.rm=TRUE) 

saveRDS(meta1, file="scripts/NMDS_full1_output.RDS")
saveRDS(meta2, file="scripts/NMDS_full2_output.RDS")
saveRDS(meta3, file="scripts/NMDS_full3_output.RDS")
