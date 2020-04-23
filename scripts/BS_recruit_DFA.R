#============================================================================================================================================
# DFA on EBS recruitment data

#Created by Krista, April 2020
#Based on "BS_climate_DFA.R"
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
rec_dat <- read.csv("data/EBS.recruit.time.series.csv")

# plot TS for SI
do.rec.dat <- as.data.frame(scale(rec_dat[,-1])) # scale to plot on 1 axis
do.rec.dat$year <- rec_dat$year
plot.rec.dat <- gather(do.rec.dat, key=key, value=value, -year)


ggplot(plot.rec.dat, aes(x=year, y=value)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~key) +
  ylab("Standard anomaly") + xlab("") + theme_bw() + geom_hline(yintercept = 0)

raw.rec.dat <- gather(rec_dat, key=species, value=recruitment, -year)

ggplot(raw.rec.dat, aes(x=year, y=recruitment)) + geom_point() + geom_line() +
  facet_wrap(~species, scales="free") +
  ylab("Recruitment") + xlab("") + theme_bw()

ggplot(raw.rec.dat, aes(x=year, y=recruitment)) + geom_point() + geom_smooth() +
  facet_wrap(~species, scales="free") +
  ylab("Recruitment") + xlab("") + theme_bw() 

#log transform====

log_rec <- as.data.frame(cbind(rec_dat$year, log(rec_dat$YLFN.R.age1.18.1a), log(rec_dat$TRBT.Age.0.R.16.1b),
                 log(rec_dat$FHSL.R.age0), log(rec_dat$Opilio.age0.recruits),
                 log(rec_dat$cod.age0.R), log(rec_dat$pollock.age0.R)))
#set col names
log_rec <- log_rec %>% 
  rename(
    year = V1,
    YLFN.R.age1.18.1a = V2,
    TRBT.Age.0.R.16.1b = V3,
    FHSL.R.age0 = V4,
    Opilio.age0.recruits = V5,
    cod.age0.R = V6,
    pollock.age0.R = V7
  )


log.rec.dat <- gather(log_rec, key=species, value=log_recruitment, -year)

ggplot(log.rec.dat, aes(x=year, y=log_recruitment)) + geom_point() + geom_line() +
  facet_wrap(~species, scales="free") +
  ylab("log(Recruitment)") + xlab("") + theme_bw()



#manupulate data for DFA========


# save the full data set for later use...
log.rec.mat <- t(as.matrix(log_rec))
colnames(log.rec.mat) <- log.rec.mat[1,]
log.rec.mat <- log.rec.mat[-1,]


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
  for(m in 1:4) {  # allowing up to 3 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(log.rec.mat, model=dfa.model, control=cntl.list,
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

model.list.1 = list(A="zero", m=2, R="unconstrained") # best model but not by much
model.1 = MARSS(log.rec.mat, model=model.list.1, z.score=TRUE, form="dfa", control=cntl.list)


# and rotate the loadings
Z.est = coef(model.1, type="matrix")$Z
H_inv = varimax(coef(model.1, type="matrix")$Z)$rotmat
Z.rot = as.data.frame(Z.est %*% H_inv)

proc_rot = solve(H_inv) %*% model.1$states #doesn't work

# reverse trend 2 to plot
Z.rot[,2] <- -Z.rot[,2]

Z.rot$names <- rownames(log.rec.mat)
Z.rot <- arrange(Z.rot, V1)
Z.rot <- gather(Z.rot[,c(1,2)])
Z.rot$names <- rownames(log.rec.mat)
#Z.rot$plot.names <- reorder(Z.rot$names, 1:14)



#plot=========
# get CI and plot loadings...
modCI <- MARSSparamCIs(model.1)

plot.CI <- data.frame(mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
                      lowCI=modCI$par.lowCI$Z)

plot.CI <- arrange(plot.CI, mean)
#plot.CI$names.order <- reorder(plot.CI$names, plot.CI$mean)
dodge <- position_dodge(width=0.9)

rec.plot <- ggplot(Z.rot, aes(names, value, fill=key)) + geom_bar(stat="identity", position="dodge") #+
# theme_bw() + ylab("Loading") + xlab("") + 
# scale_fill_manual(values=c("Trend 1" = cb[2], "Trend 2" = cb[3])) +
# theme(legend.position = c(0.8,0.2), legend.title=element_blank()) + geom_hline(yintercept = 0) +
# theme(axis.text.x  = element_text(angle=45, hjust=1, size=12)) + ylim(-0.6, 0.8)

#based on nwfsc-timeseries.github.io

yr_frst <- 1953

## get number of time series
N_ts <- dim(log.rec.mat)[1]
## get length of time series
TT <- dim(log.rec.mat)[2]

## get the estimated ZZ
Z_est <- coef(model.1, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
proc_rot = solve(H_inv) %*% model.1$states

mm <- 2 #3 processes

rec_names <- rownames(log.rec.mat)
ylbl <- rec_names
w_ts <- seq(dim(log.rec.mat)[2])
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
clr <- c("brown", 
         "blue", 
         "darkgreen", 
         "darkred", 
         "purple", 
         "darkorange")
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











