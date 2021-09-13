#===============================================================================================
#Fitting LvB models for cold vs warm periods

#Krista, July 2021
#===============================================================================================
#Notes:
#===============================================================================================



#from Franz

# Ludwig von Bertalanffy growth model:
LvB <- function(t, k, L.inf, t0=0) {
  L.inf*(1-exp(-k*(t-t0)))
}

# Explore some reasonable parameter values for starting values:
# We need reasonable starting values for fitting a non-linear model!
plot(LENGTH ~ AGE, data=scale.dat) #loaded in weight_age_exploration.R
# Some trial values for paramters
curve(LvB(x, k=0.05, L.inf=800), 0, 60, ylim=c(0,900), col=2, lwd=2, add=T)
curve(LvB(x, k=0.2, L.inf=730), 0, 60, col=3, lwd=2, add=T)
curve(LvB(x, k=0.07, L.inf=750, t0=-10), 0, 60, col=4, lwd=2, add=T)
# green line provides reasonable fit!

### Fit LvB model across all years:
START <- c(k = 0.2, L.inf = 730, t0 = 0)
fit.all <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=scale.dat, start=START)
summary(fit.all)
cf <- coef(fit.all)
cf
# Visualize the fitted model:
plot(LENGTH~ AGE, data=scale.dat)
curve(LvB(x, cf[1],cf[2], cf[3]), col=2, lwd=3, add=T)



#only cold period 2006-2012======

cold.dat <- scale.dat[which(scale.dat$YEAR>2005 & scale.dat$YEAR<2014),]
cold.dat$period <- "cold"


plot(LENGTH ~ AGE, data=cold.dat)

#use same starting values
### Fit LvB model across all years:
START <- c(k = 0.2, L.inf = 730, t0 = 0)
fit.cold <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=cold.dat, start=START)
summary(fit.cold)
cfcold <- coef(fit.cold)
cfcold
# Visualize the fitted model:
plot(LENGTH~ AGE, data=cold.dat)
curve(LvB(x, cfcold[1],cfcold[2], cfcold[3]), col=2, lwd=3, add=T)





#only warm period 2013-2019======

warm.dat <- scale.dat[which(scale.dat$YEAR>2013),]
warm.dat$period <- "warm"


plot(LENGTH ~ AGE, data=warm.dat)

#use same starting values
### Fit LvB model across all years:
START <- c(k = 0.2, L.inf = 730, t0 = 0)
fit.warm <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=warm.dat, start=START)
summary(fit.warm)
cfwarm <- coef(fit.warm)
cfwarm
# Visualize the fitted model:
plot(LENGTH~ AGE, data=warm.dat)
curve(LvB(x, cfwarm[1],cfwarm[2], cfwarm[3]), col=2, lwd=3, add=T)


#plot
plot.dat <- rbind(cold.dat, warm.dat)

plot(LENGTH~ AGE, data=plot.dat, col=as.factor(period))
curve(LvB(x, cfcold[1],cfcold[2], cfcold[3]), col=4, lwd=3, add=T)
curve(LvB(x, cfwarm[1],cfwarm[2], cfwarm[3]), col=2, lwd=3, add=T)

#manuscript plot
plot(LENGTH~ jitter(AGE), data=plot.dat, col=c("blue", "red")[as.factor(period)], pch = 16, cex =0.3,
     xlab="Age", ylab="Length (mm)")
curve(LvB(x, cfcold[1],cfcold[2], cfcold[3]), col=4, lwd=3, add=T)
curve(LvB(x, cfwarm[1],cfwarm[2], cfwarm[3]), col=2, lwd=3, add=T)



#only young cold period 2006-2013======

cold.y.dat <- scale.dat[which(scale.dat$YEAR>2005 & scale.dat$YEAR<2014),]
cold.y.dat$period <- "cold"
cold.y.dat <- cold.y.dat[which(cold.y.dat$AGE<7),]


plot(LENGTH ~ AGE, data=cold.y.dat)

#use same starting values
### Fit LvB model across all years:
START <- c(k = 0.2, L.inf = 730, t0 = 0)
fit.y.cold <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=cold.y.dat, start=START)
summary(fit.y.cold)
cfycold <- coef(fit.y.cold)
cfycold
# Visualize the fitted model:
plot(LENGTH~ AGE, data=cold.y.dat)
curve(LvB(x, cfycold[1],cfycold[2], cfycold[3]), col=2, lwd=3, add=T)





#only young warm period 2014-2019======

warm.y.dat <- scale.dat[which(scale.dat$YEAR>2013),]
warm.y.dat$period <- "warm"
warm.y.dat <- warm.y.dat[which(warm.y.dat$AGE<7),]


plot(LENGTH ~ AGE, data=warm.y.dat)

#use same starting values
### Fit LvB model across all years:
START <- c(k = 0.2, L.inf = 730, t0 = 0)
fit.y.warm <- nls(LENGTH ~ LvB(AGE, k, L.inf, t0), data=warm.y.dat, start=START)
summary(fit.y.warm)
cfywarm <- coef(fit.y.warm)
cfywarm
# Visualize the fitted model:
plot(LENGTH~ AGE, data=warm.y.dat)
curve(LvB(x, cfywarm[1],cfywarm[2], cfywarm[3]), col=2, lwd=3, add=T)


#plot
plot.y.dat <- rbind(cold.y.dat, warm.y.dat)

plot(LENGTH~ AGE, data=plot.y.dat, col=as.factor(period))
curve(LvB(x, cfycold[1],cfycold[2], cfycold[3]), col=4, lwd=3, add=T)
curve(LvB(x, cfywarm[1],cfywarm[2], cfywarm[3]), col=2, lwd=3, add=T)

plot(LENGTH~ jitter(AGE), data=plot.y.dat, col=c("blue", "red")[as.factor(period)], pch = 16, cex =0.3)
curve(LvB(x, cfycold[1],cfycold[2], cfycold[3]), col=4, lwd=3, add=T)
curve(LvB(x, cfywarm[1],cfywarm[2], cfywarm[3]), col=2, lwd=3, add=T)







#log(L) by log(W)==============================================

log_mod <- lm(log(WEIGHT) ~ log(LENGTH), data=scale.dat)
summary(log_mod)
plot(log(WEIGHT) ~ log(LENGTH), data=scale.dat)

ggplot(plot.dat, aes(log(LENGTH), log(WEIGHT), col=period)) +
  geom_point() + geom_smooth(method="lm") 

plot_mod <- lm(log(WEIGHT) ~ log(LENGTH) * period, data=plot.dat)
summary(plot_mod)

w_mod <- lm(log(WEIGHT) ~ log(LENGTH), data=warm.dat)
summary(w_mod)

c_mod <- lm(log(WEIGHT) ~ log(LENGTH), data=cold.dat)
summary(c_mod)

plot(log(cold.dat$LENGTH), log(cold.dat$WEIGHT))
abline(lm(log(cold.dat$WEIGHT) ~ log(cold.dat$LENGTH)))
#abline(4.0410, 0.3236)

plot(log(cold.dat$LENGTH), log(cold.dat$WEIGHT))
abline(lm(log(cold.dat$WEIGHT) ~ log(cold.dat$LENGTH)))

#manuscript plot
plot(log(WEIGHT)~ log(LENGTH), data=plot.dat, col=c("blue", "red")[as.factor(period)], pch = 16, cex =0.3,
     xlab="log(Length)", ylab="log(Weight)")
abline(-12.403, 3.076, col="blue")
abline(-12.06, 3.02, col="red")


#both manuscript plots together
par(mfrow=c(1,2), mar = c(4.2, 4, 2, 2)) 

plot(LENGTH~ jitter(AGE), data=plot.dat, col=c("blue", "red")[as.factor(period)], pch = 16, cex =0.3,
     xlab="Age", ylab="Length (mm)")
curve(LvB(x, cfcold[1],cfcold[2], cfcold[3]), col=4, lwd=3, add=T)
curve(LvB(x, cfwarm[1],cfwarm[2], cfwarm[3]), col=2, lwd=3, add=T)

plot(log(WEIGHT)~ log(LENGTH), data=plot.dat, col=c("blue", "red")[as.factor(period)], pch = 16, cex =0.3,
     xlab="log(Length)", ylab="log(Weight)")
abline(-12.403, 3.076, col="blue")
abline(-12.06, 3.02, col="red")

#standard deviations for illustation=====

sd(scale.dat$logWEIGHT[which(scale.dat$AGE==3)], na.rm=TRUE)

sd(scale.dat$logWEIGHT[which(scale.dat$AGE==8)])

