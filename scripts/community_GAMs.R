#==========================================================================================================
# Community models
#
# by Krista, Jan 2021
#==========================================================================================================
#Notes:
#==========================================================================================================




#Mike ran these! Read in the RDS instead. See 'MDS_on_cluster.R' for running
fitfull <- readRDS(file="scripts/MDS_full_output.RDS")

rownames(early_comm_mat) <- paste(early_wide$CRUISE, early_wide$HAUL, sep="-")

x1 <- fitfull$points[,1]
y1 <- fitfull$points[,2]
plot(x1, y1,  xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric MDS", type="n")
text(x1, y1, labels = row.names(full_comm_mat), cex=.7)
#perhaps paste rownames split into year and haul and colour by year
mat2plot <- as.data.frame(full_comm_mat)
mat2plot$rowname <- paste(rownames(mat2plot))
mat2plot$year <- str_sub(mat2plot$rowname, 2, 5)

text(x1, y1, labels = row.names(mat2plot), cex=.7, col=mat2plot$year)

text(x1, y1, labels = row.names(full_comm_mat), cex=.7, col=full_wide$YEAR)

text(x1, y1, labels = full_wide$YEAR, cex=.7, col=full_wide$YEAR)

full_wide$period_num <- NA
full_wide$period_num[which(full_wide$YEAR<2014)]<-1
full_wide$period_num[which(full_wide$YEAR>2013)]<-2
full_wide$period <- as.factor(full_wide$period)

text(x1, y1, labels = full_wide$YEAR, cex=.7, col=full_wide$period_num)
text(x1, y1, labels = full_wide$YEAR, cex=.7, col=full_wide$STRATUM)
text(x1, y1, labels = full_wide$STRATUM, cex=.7, col=full_wide$YEAR)
text(x1, y1, labels = full_wide$STRATUM, cex=.7, col=full_wide$STRATUM)

























