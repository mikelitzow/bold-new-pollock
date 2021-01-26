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

wide_an$period <- as.factor(wide_an$period)

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



#sps separately========

As1 <- gamm(logCPUE_Gadus_chalcogrammus ~ 
                te(long_albers, lat_albers, logCPUE_Atheresthes_stomias, 
                   by=period), random=list(YEAR_factor=~1), 
              data=wide_an, method="REML")
gam.check(As1[[2]]) 
summary(As1[[1]]) #  
summary(As1[[2]])
plot(As1[[2]])

A1 <- getViz(As1[[2]])


Ap2 <- plotSlice(x = sm(A1,2), 
                 fix = list("logCPUE_Atheresthes_stomias" = seq(-4, 4, length.out = 5), "period"=seq(1,2)))
Ap2 + geom_polygon(data = map_data ("world"), 
                   aes(x=long, y = lat,group=group),fill=NA,color="black",
                   inherit.aes = F)+coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE)+
  l_fitRaster(pTrans = function(.p) 0.5) + 
  l_fitContour() + l_points() + l_rug()

As2 <- gamm(logCPUE_Gadus_chalcogrammus ~ te(long_albers, lat_albers) +
              te(long_albers, lat_albers, logCPUE_Atheresthes_stomias, 
                 by=as.factor(period)), random=list(YEAR_factor=~1), 
            data=wide_an, method="REML")
gam.check(As2[[2]]) 
summary(As2[[1]]) #  
summary(As2[[2]])
plot(As2[[2]])


He1 <- gamm(logCPUE_Gadus_chalcogrammus ~ 
              te(long_albers, lat_albers, logCPUE_Hippoglossoides_elassodon, 
                 by=period), random=list(YEAR_factor=~1), 
            data=wide_an, method="REML")
gam.check(He1[[2]]) 
summary(He1[[1]]) #  
summary(He1[[2]])
plot(He1[[2]])

He2 <- gamm(logCPUE_Gadus_chalcogrammus ~ te(long_albers, lat_albers, k=30) +
              te(long_albers, lat_albers, logCPUE_Hippoglossoides_elassodon, 
                 by=period), random=list(YEAR_factor=~1), 
            data=wide_an, method="REML")
gam.check(He2[[2]]) 
summary(He2[[1]]) #  
summary(He2[[2]])
plot(He2[[2]])


La1 <- gamm(logCPUE_Gadus_chalcogrammus ~ 
              te(long_albers, lat_albers, logCPUE_Limanda_aspera, 
                 by=period), random=list(YEAR_factor=~1), 
            data=wide_an, method="REML")
gam.check(La1[[2]]) 
summary(La1[[1]]) #  
summary(La1[[2]])
plot(La1[[2]])

La2 <- gamm(logCPUE_Gadus_chalcogrammus ~ te(long_albers, lat_albers) +
              te(long_albers, lat_albers, logCPUE_Limanda_aspera, 
                 by=period), random=list(YEAR_factor=~1), 
            data=wide_an, method="REML")
gam.check(La2[[2]]) 
summary(La2[[1]]) #  
summary(La2[[2]])
plot(La2[[2]])


Gm1 <- gamm(logCPUE_Gadus_chalcogrammus ~ 
              te(long_albers, lat_albers, logCPUE_Gadus_macrocephalus, 
                 by=period), random=list(YEAR_factor=~1), 
            data=wide_an, method="REML")
gam.check(Gm1[[2]]) 
summary(Gm1[[1]]) #  
summary(Gm1[[2]])
plot(Gm1[[2]])

Gm2 <- gamm(logCPUE_Gadus_chalcogrammus ~ te(long_albers, lat_albers) +
              te(long_albers, lat_albers, logCPUE_Gadus_macrocephalus, 
                 by=period), random=list(YEAR_factor=~1), 
            data=wide_an, method="REML")
gam.check(Gm2[[2]]) 
summary(Gm2[[1]]) #  
summary(Gm2[[2]])
plot(Gm2[[2]])
