sse <- NULL
gCV2 <- function(mod, nsamp, exsamp, ...){
	
	# mod - a gam object from the function 'gam' (mgcv)
	# nsamp - number of resamples
	# exsamp - number of observations excluded in each resample

 # mod <- tgam.pop
 # nsamp <- 100
 # exsamp <- round(nrow(mod$data)*30/100)
 
 

	for(i in 1:nsamp){
		  # i <- 1
		n <- sample(dim(mod$data)[1],exsamp, replace=F)
		dat.samp <- mod$data[-n,]
		# "form" already defined in the tgam!
		#form <- as.formula(paste(colnames(sub)[i], "~ s(vari, by = I(1*(Year <= ", thresholds[j], ")), k = 4) + s(vari, by = I(1*(Year > ", thresholds[j], ")), k = 4)", sep=""))
		mod.samp <- gam(form, data=dat.samp, na.action=)
		dat.exsamp <- mod$data[n,]
		pred.tmp <- predict.gam(mod.samp, dat.exsamp, se=F)
	
		# c <- sum(rowSums(!is.na(dat.exsamp)) == 3) # change to count only non-NAs!
		sse <- c(sse,sum((pred.tmp-mod$y[n])^2, na.rm=T)/exsamp)
		}
	return(list("sse"=sse,"mean.sse"=mean(sse)))
}