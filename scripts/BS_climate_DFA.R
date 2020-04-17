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


ggplot(plot.dat, aes(x=year, y=value)) + geom_bar(position="dodge", stat="identity", fill=cb[2]) + facet_wrap(~key) +
  ylab("Standard anomaly") + xlab("") + theme_bw() + geom_hline(yintercept = 0)

# check
head(dat)

#manupulate data for DFA========

# save the full data set for later use...
all.clim.dat <- t(as.matrix(dat))

# now fit DFA models
# first the early era
# e.cli.dat = as.matrix(dat[rownames(dat) %in% 1950:1988,])
# 
# # and transpose
# e.cli.dat <- t(e.cli.dat)


#fit model========

# now fit DFA models with 1-3 trends and different error structures and compare

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1:3) {  # allowing up to 3 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(all.clim.dat, model=dfa.model, control=cntl.list,
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

model.list = list(A="zero", m=3, R="diagonal and unequal") # best model
mod.best = MARSS(all.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)


# and rotate the loadings
Z.est = coef(mod.best, type="matrix")$Z
H.inv = varimax(coef(mod.best, type="matrix")$Z)$rotmat
Z.rot = as.data.frame(Z.est %*% H.inv)

proc_rot = solve(H_inv) %*% mod.best$states

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

yr_frst <- 1951

## get number of time series
N_ts <- dim(all.clim.dat)[1]
## get length of time series
TT <- dim(all.clim.dat)[2]

## get the estimated ZZ
Z_est <- coef(mod.best, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
proc_rot = solve(H_inv) %*% mod.best$states

mm <- 3 #3 processes

clim_names <- rownames(all.clim.dat)
 ylbl <- clim_names
 w_ts <- seq(dim(all.clim.dat)[2])
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


#refit best model w AR=====

# HAVING TROUBLE with the below, going to copy and do just for best model below

# object for model data
model.data = data.frame()

# object for residual autocorrelation results
res.ar <- matrix(nrow=1, ncol=27)

# fit models
for(R in levels.R) {
  for(m in 1:3) {
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(all.clim.dat, model=dfa.model, control=cntl.list,
                 form="dfa", z.score=TRUE)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)
    
    # add in calculation of residual AR(1) values!
    dw.p <- NA
    res <- residuals(kemz)$residuals
    
    # drop the 0s - these should be NAs!
    
    drop <- res==0
    res[drop] <- NA
    
    for(ii in 1:nrow(res)){
      
      dw.p[ii] <- dwtest(res[ii,] ~ 1)$p.value
      
    }
    
    # pad to the correct length for 1- and 2-trend models
    if(length(dw.p)==8) {dw.p <- c(dw.p, NA, NA)}
    if(length(dw.p)==9) {dw.p <- c(dw.p, NA)}
    
    res.ar <- rbind(res.ar, dw.p)
    
  } # end m loop
} # end R loop

res.ar
colnames(res.ar) <- rownames(res) # make sure this case of "res" is a full case, i.e., three shared trends
res.ar <- res.ar[2:nrow(res.ar),] # drop first row of NAs

model.res.table <- as.data.frame(res.ar)
model.res.table$R <- model.data$R
model.res.table$m <- model.data$m

write.csv(model.res.table, ".csv")

#refit only best model w AR=====

# HAVING TROUBLE with the below, going to copy and do just for best model below
model.list = list(A="zero", m=3, R="diagonal and unequal") # best model
mod.best = MARSS(all.clim.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# object for model data
model.data = data.frame()

# object for residual autocorrelation results
res.ar <- matrix(nrow=1, ncol=27)

# fit models


    dfa.model = list(A="zero", m=3, R="diagonal and unequal")
    kemzar = MARSS(all.clim.dat, model=dfa.model, control=cntl.list,
                 form="dfa", z.score=TRUE)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemzar$logLik,
                                  K=kemzar$num.params,
                                  AICc=kemzar$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemzar", m, R, sep="."), kemzar)
    
    # add in calculation of residual AR(1) values!
    dw.p <- NA
    res <- residuals(kemzar)$residuals
    
    # drop the 0s - these should be NAs!
    
    drop <- res==0
    res[drop] <- NA
    
    for(ii in 1:nrow(res)){     #NOT WORKING
      
      dw.p[ii] <- dwtest(res[ii,] ~ 1)$p.value
      
    
    
    # pad to the correct length for 1- and 2-trend models
    if(length(dw.p)==8) {dw.p <- c(dw.p, NA, NA)}
    if(length(dw.p)==9) {dw.p <- c(dw.p, NA)}
    
    res.ar <- rbind(res.ar, dw.p)
    
    }


res
#colnames(res.ar) <- rownames(res) # make sure this case of "res" is a full case, i.e., three shared trends
#res <- res[2:nrow(res),] # drop first row of NAs

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


