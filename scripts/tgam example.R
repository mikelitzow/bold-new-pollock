require(mgcv)

# load modified gCV function (gCV2) that allows for threshold to be looped
source("scripts/gCV.modified.R")

source("scripts/ThresholdGam.R") ### shortcut to look for threshold value

stocks.env <- read.csv("data/tgam.example.data.csv", row.names=1)

# add year column
stocks.env$year <- 1965:2014

# we are looking for the best threshold year for dividing different periods
# of sensitivity to SST. Each candidate threshold must maintain at least 20% of the data in the pre-
# and post-threshold periods!

     lower <- round(quantile(1965:2012, prob = 0.2)) # 2012 is the last year of complete data...
     upper <- round(quantile(1965:2012, prob = 0.8))
     
upper; lower # these define the upper and lower possible thresholds!

 # for every TS, I'll consider the same set of thresholds: 1974-2003
  
  
png("figs/aggregated salmon-sst tgam gCV.png", 5,6, units='in', res=300)
par(mfrow=c(3,2), mar=c(4,4,2,1))
gam <- best.threshold <- NA
thresholds <- 1974:2003
all.thr <- matrix(NA, nrow=length(thresholds), ncol=6) # matrix to record all thresholds
dimnames(all.thr) <- list(thresholds, colnames(stocks.env[1:6]))
sub <- na.omit(stocks.env) # drop incomplete years

set.seed(9948)

for(i in 1:6){ # looping through the first 6 time series!

form <- as.formula(paste(colnames(sub)[i], " ~ s(sst.3, k = 4)", sep="")) # this is the formula for a simple gam
gam.pop<-gam(form,data=sub,control=list(keepData=TRUE)) # fit the gam
    gam[i] <- sqrt(gCV2(gam.pop,100,round(length(sub$year)*30/100))$mean.sse)  # save the gam gCV score

for(j in 1:length(thresholds)){ # looping through each candidate threshold!

    form <- as.formula(paste(colnames(sub)[i], " ~ s(sst.3, by = I(1*(year <= ", thresholds[j], ")), k = 4) + s(sst.3, by = I(1*(year > ", thresholds[j], ")), k = 4)", sep="")) # this is the tgam formulation!
tgam.pop<-gam(form,data=sub,control=list(keepData=TRUE)) 
    all.thr[j,i]<-sqrt(gCV2(tgam.pop,100,round(length(sub$year)*30/100))$mean.sse) # this saves the gCV score for the candidate threshold!   
}
plot(thresholds, all.thr[,i], type="l", xlab="Threshold year", ylab="gCV", ylim=range(all.thr[,i], gam[i]), cex.lab=1.1, cex.axis=1.2) # plot the gCV scores for the tgam using each candidate threshold
abline(h=gam[i], lty=2) # this plots the gCV score for the simple gam for comparison
mtext(colnames(sub)[i])
keep <- which(all.thr[,i]==min(all.thr[,i]))
best.threshold[i] <- thresholds[keep] # the best TGAM for each time series
legend("bottomright", legend=thresholds[keep], bty="n", text.col="red", cex=1.1)
}
names(best.threshold) <- names(gam) <- colnames(stocks.env)[1:6]
dev.off()

