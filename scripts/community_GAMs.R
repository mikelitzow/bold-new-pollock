#==========================================================================================================
# Community models
#
# by Krista, Jan 2021
#==========================================================================================================
#Notes:
#==========================================================================================================



full_wide <- read.csv("data/full_wide_comm_dat.csv", stringsAsFactors = FALSE, row.names = 1)

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



mat1 <- full_wide[,c(38:48)]

cor1 <- cor(mat1, method = "pearson", use = "complete.obs")
corrplot(cor1)

write.csv(cor1, file=paste(wd,"/data/community-correlations.csv", sep=""))


#those above 0.1
#Atheresthes_stomias, Hippoglossoides_elassodon, Limanda_aspera, Gadus_macrocephalus, 
#Chionoecetes_bairdi, Chionoecetes_opilio

#those above 0.2
#Atheresthes_stomias, Hippoglossoides_elassodon, Limanda_aspera, Gadus_macrocephalus

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Chionoecetes_bairdi, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Atheresthes_stomias, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Hippoglossus_stenolepis, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Limanda_aspera, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Lepidopsetta_sp, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Chionoecetes_opilio, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Gadus_macrocephalus, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Hippoglossoides_elassodon, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Pleuronectes_quadrituberculatus, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Lepidopsetta_polyxystra, col=as.factor(period))) + geom_point() + geom_smooth()

ggplot(full_wide, aes(logCPUE_Gadus_chalcogrammus, logCPUE_Gadus_macrocephalus, col=as.factor(period))) + geom_point() + geom_smooth()

wide_an <- full_wide
wide_an$period <- NA

wide_an$period[which(wide_an$YEAR<2014)]<-"early"
wide_an$period[which(wide_an$YEAR>2013)]<-"late"

big1 <- gamm(logCPUE_Gadus_chalcogrammus ~ s(logCPUE_Chionoecetes_bairdi, by=as.factor(period)) +
               s(logCPUE_Atheresthes_stomias, by=as.factor(period)) +
               s(logCPUE_Hippoglossus_stenolepis, by=as.factor(period)) + 
               s(logCPUE_Limanda_aspera, by=as.factor(period)) + 
               s(logCPUE_Chionoecetes_opilio, by=as.factor(period)) +
               s(logCPUE_Gadus_macrocephalus, by=as.factor(period)) +
               s(logCPUE_Hippoglossoides_elassodon, by=as.factor(period)) +
               s(logCPUE_Pleuronectes_quadrituberculatus, by=as.factor(period)) +
               s(logCPUE_Lepidopsetta_polyxystra, by=as.factor(period)) +
               s(logCPUE_Gadus_macrocephalus, by=as.factor(period)) +
                       te(long_albers, lat_albers), random=list(YEAR_factor=~1), 
                     data=periods_analysis_dat, method="REML")
gam.check(big1[[2]]) 
summary(big1[[1]]) #  
summary(big1[[2]])
plot(big1[[2]])


#try w those above 0.2
#Atheresthes_stomias, Hippoglossoides_elassodon, Limanda_aspera, Gadus_macrocephalus

spat1 <- gamm(logCPUE_Gadus_chalcogrammus ~ 
                te(long_albers, lat_albers, logCPUE_Atheresthes_stomias, by=as.factor(period)) +
                te(long_albers, lat_albers, logCPUE_Hippoglossoides_elassodon, by=as.factor(period)) +
                te(long_albers, lat_albers, logCPUE_Limanda_aspera, by=as.factor(period)) +
                te(long_albers, lat_albers, logCPUE_Gadus_macrocephalus, by=as.factor(period)), random=list(YEAR_factor=~1), 
             data=wide_an, method="REML")
gam.check(spat1[[2]]) 
summary(spat1[[1]]) #  
summary(spat1[[2]])
plot(spat1[[2]])




