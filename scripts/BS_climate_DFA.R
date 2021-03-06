#============================================================================================================================================
# DFA on EBS climate

#Created by Krista, March 2020
#Borrowing heavily from 'example script -DFA on GOA climate and community data.R'
#============================================================================================================================================
#Notes:
#============================================================================================================================================

library(ggplot2)
library(reshape2)
library(dplyr)
library(pracma)
library(MARSS)
library(tidyr)
# devtools::install_github("kassambara/ggpubr")
library(ggpubr)
library(cowplot)
library(gridExtra)
library(broom)
library(lemon)
library(MuMIn)
library(lmtest)
library(cowplot)

#Get data plot data =======

# load environmental data
dat <- read.csv("data/climate data.csv", row.names = 1)


# plot TS for SI
do.dat <- as.data.frame(scale(dat)) # scale to plot on 1 axis
plot.dat <- gather(do.dat)
plot.dat$year <- 1951:2019


ggplot(plot.dat, aes(x=year, y=value)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~key) +
  ylab("Standard anomaly") + xlab("") + theme_bw() + geom_hline(yintercept = 0)

# check
head(dat)

#manupulate data for DFA========

# save the full data set for later use...
all.clim.dat <- t(as.matrix(dat))

# now fit DFA models
# first the early era
# e.cli.dat = as.matrix(dat[rownames(dat) %in% 1950:1988,])

#new early data
 e.cli.dat = as.matrix(dat[rownames(dat) %in% 1988:2014,])

# 
# # and transpose
 e.clim.dat <- t(e.cli.dat)


#fit model========

# now fit DFA models with 1-3 trends and different error structures and compare

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001, safe=TRUE, trace=1)

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1:4) {  # allowing up to 3 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(e.clim.dat, model=dfa.model, control=cntl.list,
                 form="dfa", z.score=TRUE)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)
  } # end m loop
} # end R loop

# calculate delta-AICc scores, sort in descending order, and compare
model.data$dAICc <- model.data$AICc-min(model.data$AICc)
model.data <- model.data %>%
  arrange(dAICc)
model.data

#rotate=======

# now fit best model

model.list = list(A="zero", m=2, R="diagonal and unequal") # best model
#mod.best = MARSS(all.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
mod.best = MARSS(e.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

saveRDS(mod.best, file="scripts/DFA_best_mod88-2014.RDS")


# and rotate the loadings
Z.est = coef(mod.best, type="matrix")$Z
H.inv = varimax(coef(mod.best, type="matrix")$Z)$rotmat
Z.rot = as.data.frame(Z.est %*% H.inv)

proc_rot = solve(H.inv) %*% mod.best$states

# reverse trend 2 to plot
Z.rot[,2] <- -Z.rot[,2]

Z.rot$names <- rownames(all.clim.dat)
Z.rot <- arrange(Z.rot, V1)
Z.rot <- gather(Z.rot[,c(1,2)])
Z.rot$names <- rownames(all.clim.dat)
#Z.rot$plot.names <- reorder(Z.rot$names, 1:14)



#plot=========
# get CI and plot loadings...
modCI <- MARSSparamCIs(mod.best)

#BELOW HERE is where things get bad
# plot.CI <- data.frame(names=rownames(all.clim.dat), mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
#                            lowCI=modCI$par.lowCI$Z)
plot.CI <- data.frame(mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
                      lowCI=modCI$par.lowCI$Z)

plot.CI <- arrange(plot.CI, mean)
plot.CI$names.order <- reorder(plot.CI$names, plot.CI$mean)
dodge <- position_dodge(width=0.9)

clim.plot <- ggplot(Z.rot, aes(names, value, fill=key)) + geom_bar(stat="identity", position="dodge") #+
  # theme_bw() + ylab("Loading") + xlab("") + 
  # scale_fill_manual(values=c("Trend 1" = cb[2], "Trend 2" = cb[3])) +
  # theme(legend.position = c(0.8,0.2), legend.title=element_blank()) + geom_hline(yintercept = 0) +
  # theme(axis.text.x  = element_text(angle=45, hjust=1, size=12)) + ylim(-0.6, 0.8)

#based on nwfsc-timeseries.github.io

yr_frst <- 1988

## get number of time series
N_ts <- dim(e.clim.dat)[1]
## get length of time series
TT <- dim(e.clim.dat)[2]

## get the estimated ZZ
Z_est <- coef(mod.best, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
proc_rot = solve(H_inv) %*% mod.best$states

mm <- 2 #2 processes

clim_names <- rownames(e.clim.dat)
 ylbl <- clim_names
 w_ts <- seq(dim(e.clim.dat)[2])
 layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(2, 1))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
# jpeg("figs/ugly_DFA_trends_loadings.jpg")
par(mfcol=c(mm,2), mar = c(1,1,1,1), omi = c(0, 0, 0, 0))
## plot the processes
for (i in 1:mm) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
  ## set up plot area
  plot(w_ts, proc_rot[i, ], type = "n", bty = "L", #ylim = ylm, 
       xlab = "", ylab = "", xaxt = "n")
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(w_ts, proc_rot[i, ], lwd = 2)
  lines(w_ts, proc_rot[i, ], lwd = 2)
  ## add panel labels
  mtext(paste("State", i), side = 3, line = 0.5)
  #axis(1, 12 * (0:dim(all.clim.dat)[2]) + 1, yr_frst + 0:dim(all.clim.dat)[2])
  axis(1, 1:70, yr_frst + 0:dim(all.clim.dat)[2])
}
## plot the loadings
clr <- c("brown", "brown", "brown", "brown", 
         "blue", 
         "darkgreen", "darkgreen", "darkgreen", "darkgreen", 
         "darkred", 
         "purple", 
         "darkorange",
         "red",  "red",  "red",  "red", 
         "green",  "green",
         "darkblue", 
         "pink", "pink","pink","pink","pink")
minZ <- 0
ylm <- c(-1, 1) * max(abs(Z_rot))
for (i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, i]) > minZ, i]), 
       type = "h", lwd = 2, xlab = "", ylab = "", 
       xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col=clr)
  for (j in 1:N_ts) {
    if (Z_rot[j, i] > minZ) {
      text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, col=clr[j])
    }
    if (Z_rot[j, i] < -minZ) {
      text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, col=clr[j])
    }
    abline(h = 0, lwd = 1.5, col = "gray")
  }
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
}
#dev.off()

par(mai = c(0.9, 0.9, 0.1, 0.1))
ccf(proc_rot[1, ], proc_rot[2, ], lag.max = 12, main = "")


#plot obs vs fitted========

#from online course "nwfsc-timeseries.github.io"

get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type = "matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if (!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for (tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[, 
                                                         , tt] %*% t(ZZ)
    SS <- Ey$yxtT[, , tt] - Ey$ytT[, tt, drop = FALSE] %*% 
      t(MLEobj$states[, tt, drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1 - alpha/2) * SE + fits$ex
  fits$lo <- qnorm(alpha/2) * SE + fits$ex
  return(fits)
}

#demean data
# y_bar <- apply(all.clim.dat, 1, mean, na.rm = TRUE)
# dat <- all.clim.dat - y_bar
# rownames(dat) <- rownames(all.clim.dat)

dat <- scale(e.clim.dat)

head(e.clim.dat)

e.clim.dat.std <- e.clim.dat

i <- 1
for(i in 1:nrow(e.clim.dat)) {
  e.clim.dat.std[i,] <- (e.clim.dat[i,]-mean(e.clim.dat[i,], na.rm=TRUE))/sd(e.clim.dat[i,], na.rm=TRUE)  
}
#Double checking
apply(e.clim.dat.std, 1, mean, na.rm=TRUE)
apply(e.clim.dat.std, 1, sd, na.rm=TRUE)
dat <- e.clim.dat.std
#plot demeaned data

driv <- rownames(e.clim.dat)
#clr <- c("brown", "blue", "darkgreen", "darkred", "purple")
cnt <- 1
par(mfrow = c(N_ts/4, 4), mar = c(1, 1,1.5,1), omi = c(0.1, 
                                                             0.1, 0.1, 0.1))
for (i in driv) {
  plot(dat[i, ], xlab = "", ylab = "", bty = "L", 
       xaxt = "n", pch = 16, col = clr[cnt], type = "b")
  axis(1,  (0:dim(e.clim.dat)[2]) + 1, yr_frst + 0:dim(e.clim.dat)[2])
  title(i)
  cnt <- cnt + 1
}

## get model fits & CI's
mod_fit <- get_DFA_fits(mod.best)
## plot the fits
par(mfrow = c(N_ts/4, 4), mar = c(1, 1, 1, 1), omi = c(0, 
                                                             0, 0, 0))
for (i in 1:N_ts) {
  up <- mod_fit$up[i, ]
  mn <- mod_fit$ex[i, ]
  lo <- mod_fit$lo[i, ]
  plot(w_ts, mn, xlab = "", xaxt = "n", type = "n", 
       cex.lab = 1.2, ylim = c(min(lo), max(up)))
  axis(1,  (0:dim(all.clim.dat)[2]) + 1, yr_frst + 0:dim(all.clim.dat)[2])
  points(w_ts, dat[i, ], pch = 16, col = clr[i])
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}



#refit only best model w AR=====

model.list = list(A="zero", m=3, R="diagonal and unequal") # best model
mod.best = MARSS(all.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# # object for model data
# model.data = data.frame()

# object for residual autocorrelation results
res.ar <- data.frame()

# fit models


    # dfa.model = list(A="zero", m=3, R="diagonal and unequal")
    # kemzar = MARSS(all.clim.dat, model=dfa.model, control=cntl.list,
    #              form="dfa", z.score=TRUE)
    # model.data = rbind(model.data,
    #                    data.frame(R=R,
    #                               m=m,
    #                               logLik=kemzar$logLik,
    #                               K=kemzar$num.params,
    #                               AICc=kemzar$AICc,
    #                               stringsAsFactors=FALSE))
    # assign(paste("kemzar", m, R, sep="."), kemzar)
    
    # add in calculation of residual AR(1) values!
    dw.p <- NA
    res <- residuals(mod.best)$residuals
   # res <- residuals(kemzar)$residuals
    # drop the 0s - these should be NAs!
    
    drop <- res==0
    res[drop] <- NA
    
    for(ii in 1:nrow(res)){     #for each i in length (climate variables)

      dw.p <- dwtest(res[ii,] ~ 1)$p.value #durbin-watson test for autocorrelation of disturbance on
                                                #that climate variable's row - one value each year
      

    res.ar <- rbind(res.ar,
                    data.frame(time.series=row.names(all.clim.dat)[ii], 
                               p=dw.p)) 
    
    }


res.ar

model.res.table <- as.data.frame(res)
#model.res.table$R <- model.data$R
#model.res.table$m <- model.data$m

write.csv(model.res.table, ".csv")




#model diagnostics=======
par(mfrow = c(1, 2))
resids <- residuals(kemzar)
plot(resids$model.residuals[1, ], ylab = "model residual", xlab = "", 
     main = "flat level")
abline(h = 0)
plot(resids$state.residuals[1, ], ylab = "state residual", xlab = "", 
     main = "flat level")
abline(h = 0)

acf(resids$model.residuals[1, ])
acf(resids$state.residuals[1, ], na.action=na.pass)
acf(resids$model.residuals, na.action=na.pass)





#try w late=================================================

#having trouble w early period, try late

#manupulate data for DFA========

# save the full data set for later use...
#all.clim.dat <- t(as.matrix(dat))

# now fit DFA models
# first the early era
# e.cli.dat = as.matrix(dat[rownames(dat) %in% 1950:1988,])

#new early data
l.cli.dat = as.matrix(dat[rownames(dat) %in% 2014:2019,])

#remove two columns w all zeros
l.cli.dat <- l.cli.dat[,-c(14:15)]

# 
# # and transpose
l.clim.dat <- t(l.cli.dat)


#fit model========

# now fit DFA models with 1-3 trends and different error structures and compare

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001, safe=TRUE, trace=1)

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1:4) {  # allowing up to 3 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(l.clim.dat, model=dfa.model, control=cntl.list,
                 form="dfa", z.score=TRUE)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)
  } # end m loop
} # end R loop

# calculate delta-AICc scores, sort in descending order, and compare
model.data$dAICc <- model.data$AICc-min(model.data$AICc)
model.data <- model.data %>%
  arrange(dAICc)
model.data


#rotate=======

# now fit best model

model.list = list(A="zero", m=1, R="diagonal and equal") # best model
#mod.best = MARSS(all.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
mod.best = MARSS(l.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

saveRDS(mod.best, file="scripts/DFA_best_modpost2014.RDS")


# and rotate the loadings
Z.est = coef(mod.best, type="matrix")$Z
H.inv = varimax(coef(mod.best, type="matrix")$Z)$rotmat #hmm wrong dimensions?
Z.rot = as.data.frame(Z.est %*% H.inv)

proc_rot = solve(H.inv) %*% mod.best$states

# reverse trend 2 to plot
Z.rot[,2] <- -Z.rot[,2]

Z.rot$names <- rownames(all.clim.dat)
Z.rot <- arrange(Z.rot, V1)
Z.rot <- gather(Z.rot[,c(1,2)])
Z.rot$names <- rownames(all.clim.dat)
#Z.rot$plot.names <- reorder(Z.rot$names, 1:14)



#plot=========
# get CI and plot loadings...
modCI <- MARSSparamCIs(mod.best)

#BELOW HERE is where things get bad
# plot.CI <- data.frame(names=rownames(all.clim.dat), mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
#                            lowCI=modCI$par.lowCI$Z)
plot.CI <- data.frame(mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
                      lowCI=modCI$par.lowCI$Z)

plot.CI <- arrange(plot.CI, mean)
plot.CI$names.order <- reorder(plot.CI$names, plot.CI$mean)
dodge <- position_dodge(width=0.9)

clim.plot <- ggplot(Z.rot, aes(names, value, fill=key)) + geom_bar(stat="identity", position="dodge") #+
# theme_bw() + ylab("Loading") + xlab("") + 
# scale_fill_manual(values=c("Trend 1" = cb[2], "Trend 2" = cb[3])) +
# theme(legend.position = c(0.8,0.2), legend.title=element_blank()) + geom_hline(yintercept = 0) +
# theme(axis.text.x  = element_text(angle=45, hjust=1, size=12)) + ylim(-0.6, 0.8)

#based on nwfsc-timeseries.github.io

yr_frst <- 2014

## get number of time series
N_ts <- dim(l.clim.dat)[1]
## get length of time series
TT <- dim(l.clim.dat)[2]

## get the estimated ZZ
Z_est <- coef(mod.best, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
proc_rot = solve(H_inv) %*% mod.best$states

mm <- 2 #2 processes

clim_names <- rownames(e.clim.dat)
ylbl <- clim_names
w_ts <- seq(dim(e.clim.dat)[2])
layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(2, 1))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
# jpeg("figs/ugly_DFA_trends_loadings.jpg")
par(mfcol=c(mm,2), mar = c(1,1,1,1), omi = c(0, 0, 0, 0))
## plot the processes
for (i in 1:mm) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
  ## set up plot area
  plot(w_ts, proc_rot[i, ], type = "n", bty = "L", #ylim = ylm, 
       xlab = "", ylab = "", xaxt = "n")
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(w_ts, proc_rot[i, ], lwd = 2)
  lines(w_ts, proc_rot[i, ], lwd = 2)
  ## add panel labels
  mtext(paste("State", i), side = 3, line = 0.5)
  #axis(1, 12 * (0:dim(all.clim.dat)[2]) + 1, yr_frst + 0:dim(all.clim.dat)[2])
  axis(1, 1:70, yr_frst + 0:dim(all.clim.dat)[2])
}
## plot the loadings
clr <- c("brown", "brown", "brown", "brown", 
         "blue", 
         "darkgreen", "darkgreen", "darkgreen", "darkgreen", 
         "darkred", 
         "purple", 
         "darkorange",
         "red",  "red",  "red",  "red", 
         "green",  "green",
         "darkblue", 
         "pink", "pink","pink","pink","pink")
minZ <- 0
ylm <- c(-1, 1) * max(abs(Z_rot))
for (i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, i]) > minZ, i]), 
       type = "h", lwd = 2, xlab = "", ylab = "", 
       xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col=clr)
  for (j in 1:N_ts) {
    if (Z_rot[j, i] > minZ) {
      text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, col=clr[j])
    }
    if (Z_rot[j, i] < -minZ) {
      text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, col=clr[j])
    }
    abline(h = 0, lwd = 1.5, col = "gray")
  }
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
}
#dev.off()

par(mai = c(0.9, 0.9, 0.1, 0.1))
ccf(proc_rot[1, ], proc_rot[2, ], lag.max = 12, main = "")


#plot obs vs fitted========

#from online course "nwfsc-timeseries.github.io"

get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type = "matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if (!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for (tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[, 
                                                         , tt] %*% t(ZZ)
    SS <- Ey$yxtT[, , tt] - Ey$ytT[, tt, drop = FALSE] %*% 
      t(MLEobj$states[, tt, drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1 - alpha/2) * SE + fits$ex
  fits$lo <- qnorm(alpha/2) * SE + fits$ex
  return(fits)
}

#demean data
# y_bar <- apply(all.clim.dat, 1, mean, na.rm = TRUE)
# dat <- all.clim.dat - y_bar
# rownames(dat) <- rownames(all.clim.dat)

dat <- scale(e.clim.dat)

head(e.clim.dat)

e.clim.dat.std <- e.clim.dat

i <- 1
for(i in 1:nrow(e.clim.dat)) {
  e.clim.dat.std[i,] <- (e.clim.dat[i,]-mean(e.clim.dat[i,], na.rm=TRUE))/sd(e.clim.dat[i,], na.rm=TRUE)  
}
#Double checking
apply(e.clim.dat.std, 1, mean, na.rm=TRUE)
apply(e.clim.dat.std, 1, sd, na.rm=TRUE)
dat <- e.clim.dat.std
#plot demeaned data

driv <- rownames(e.clim.dat)
#clr <- c("brown", "blue", "darkgreen", "darkred", "purple")
cnt <- 1
par(mfrow = c(N_ts/4, 4), mar = c(1, 1,1.5,1), omi = c(0.1, 
                                                       0.1, 0.1, 0.1))
for (i in driv) {
  plot(dat[i, ], xlab = "", ylab = "", bty = "L", 
       xaxt = "n", pch = 16, col = clr[cnt], type = "b")
  axis(1,  (0:dim(e.clim.dat)[2]) + 1, yr_frst + 0:dim(e.clim.dat)[2])
  title(i)
  cnt <- cnt + 1
}

## get model fits & CI's
mod_fit <- get_DFA_fits(mod.best)
## plot the fits
par(mfrow = c(N_ts/4, 4), mar = c(1, 1, 1, 1), omi = c(0, 
                                                       0, 0, 0))
for (i in 1:N_ts) {
  up <- mod_fit$up[i, ]
  mn <- mod_fit$ex[i, ]
  lo <- mod_fit$lo[i, ]
  plot(w_ts, mn, xlab = "", xaxt = "n", type = "n", 
       cex.lab = 1.2, ylim = c(min(lo), max(up)))
  axis(1,  (0:dim(all.clim.dat)[2]) + 1, yr_frst + 0:dim(all.clim.dat)[2])
  points(w_ts, dat[i, ], pch = 16, col = clr[i])
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}




#again w all data====================

#manupulate data for DFA========

# save the full data set for later use...
all.clim.dat <- t(as.matrix(dat))

# now fit DFA models
# first the early era
# e.cli.dat = as.matrix(dat[rownames(dat) %in% 1950:1988,])

#new early data
long.cli.dat = as.matrix(dat[rownames(dat) %in% 1988:2019,])

# 
# # and transpose
long.clim.dat <- t(long.cli.dat)


#fit model========

# now fit DFA models with 1-3 trends and different error structures and compare

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001, safe=TRUE, trace=1)

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1:4) {  # allowing up to 3 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(long.clim.dat, model=dfa.model, control=cntl.list,
                 form="dfa", z.score=TRUE)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)
  } # end m loop
} # end R loop

# calculate delta-AICc scores, sort in descending order, and compare
model.data$dAICc <- model.data$AICc-min(model.data$AICc)
model.data <- model.data %>%
  arrange(dAICc)
model.data

#rotate=======

# now fit best model

model.list = list(A="zero", m=2, R="diagonal and unequal") # best model
#mod.best = MARSS(all.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
mod.best = MARSS(long.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

saveRDS(mod.best, file="scripts/DFA_best_mod88-2019.RDS")
