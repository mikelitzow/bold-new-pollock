library(tidyverse)
library(MARSS)

# fit DFA to long-term southeast Bering climate time series for different eras

# set colors
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load climate data 
dat <- read.csv("data/climate data.csv")

names(dat)

# subset
keep <- c("ice.area.jfma", "m4.march.ice", "m5.march.ice", "NW.wind.May.Sep",
          "NW.wind.Oct.Apr", "SE.wind.May.Sep", "SE.wind.Oct.Apr", "south.sst.amj",
          "south.sst.ndjfm", "south.wind.stress.amj")


dfa.dat <- dat %>%
  select(keep) 

dfa.dat <- as.matrix(t(dfa.dat))
colnames(dfa.dat) <- dat$year


# find best error structure for 1-trend model

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
  for(m in 1) {  # allowing up to 1 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:1988], model=dfa.model, control=cntl.list, 
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

era1.models <- model.data

era1.models; era2.models
# one trend, equalvarcov is best for era1
# one trend, diagonal and unequal is best for era2

# fit models to each era, plot loadings and trend
model.list = list(A="zero", m=1, R="equalvarcov")
mod1 = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:1988], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

model.list = list(A="zero", m=1, R="diagonal and unequal")
mod2 = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1989:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# get CI and plot loadings...
mod1.CI <- MARSSparamCIs(mod1)

mod2.CI <- MARSSparamCIs(mod2)

plot.CI <- data.frame(names=rep(rownames(dfa.dat),2), 
                      era=rep(c("1965-1988", "1989-2013"), each=nrow(dfa.dat)),
                      mean=c(mod1.CI$par$Z, mod2.CI$par$Z),
                      upCI=c(mod1.CI$par.upCI$Z,mod2.CI$par.upCI$Z),
                      lowCI=c(mod1.CI$par.lowCI$Z,mod2.CI$par.lowCI$Z))

# plot.CI$names <- reorder(plot.CI$names, plot.CI$mean)
dodge <- position_dodge(width=0.9)

ggplot(plot.CI, aes(x=names, y=mean, fill=era)) + 
  geom_bar(position=dodge, stat="identity") +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") + 
  xlab("") + 
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), axis.title.y = element_blank()) +
  geom_hline(yintercept = 0) 

# plot trend
plot.trend <- data.frame(t=1965:2013,
                         era=c(rep("1965-1988", length(1965:1988)), rep("1989-2013", length(1989:2013))),
                         estimate=c(as.vector(mod1$states),as.vector(mod2$states)),
                         conf.low=c(as.vector(mod1$states)-1.96*as.vector(mod1$states.se), as.vector(mod2$states)-1.96*as.vector(mod2$states.se)),
                         conf.high=c(as.vector(mod1$states)+1.96*as.vector(mod1$states.se), as.vector(mod2$states)+1.96*as.vector(mod2$states.se)))


ggplot(plot.trend, aes(t, estimate)) +
  geom_line() + 
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) + xlab("") + ylab("Trend") +
  facet_wrap(~era, scales="free")

# now...regression or correlation onto SLP fields
library(ncdf4)
library(maps)
library(maptools)
library(mapdata)
library(fields)
library(oce)
library(chron)
library(zoo)

# load slp
dat <- nc_open("data/NCEP.NCAR.slp.nc")

x <- ncvar_get(dat, "longitude")
y <- ncvar_get(dat, "latitude")
slp <- ncvar_get(dat, "slp", verbose = F)
dim(slp) # 144 long, 29 lat, 864 months

# need to reverse latitude for plotting!
y <- rev(y)
slp <- slp[,29:1,]

# Change data into a matrix with months / cells for rows / columns
slp <- aperm(slp, 3:1)  
slp <- matrix(slp, nrow=dim(slp)[1], ncol=prod(dim(slp)[2:3]))  

# plot to confirm that everything is ok
z <- colMeans(slp, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", yaxt="n", xaxt="n")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# will use winter (NDJFM) SLP

# define the winter period
# first, extract dates
raw <- ncvar_get(dat, "time") # seconds since 1-1-1970
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))
m <- months(d)
yr <- as.numeric(as.character(years(d)))

# make vectors of lat/long and add (with date) as dimnames
lat <- rep(y, length(x))   
lon <- rep(x, each = length(y)) 

dimnames(slp) <- list(as.character(d), paste("N", lat, "E", lon, sep=""))

# now define winter
win <- c("Nov", "Dec", "Jan", "Feb", "Mar")
# set Nov and Dec equal to the year corresponding to Jan!
win.yr <- ifelse(m %in% c("Nov", "Dec"), yr+1, yr) 
# and restrict both winter year and slp to the winter months
win.yr <- win.yr[m %in% win]
win.slp <- slp[m %in% win,]

# now get annual winter means for each cell

ff <- function(x) tapply(x, win.yr, mean)

win.slp <- apply(win.slp, 2, ff)

# drop winter 2020, whixh is incomplete
win.slp <- win.slp[rownames(win.slp) < 2020,]

ff <- function(x) rollmean(x, 2, align="right", fill=NA)

win.slp2 <- apply(win.slp, 2, ff)

# vectors to catch correlation coefficients
cor.65.88 <- cor.89.13 <- NA

for(j in 1:ncol(win.slp2)){
  
  cor.65.88[j] <- cor(as.vector(mod1$states), 
                      win.slp2[rownames(win.slp2) %in% 1965:1988,j])  
  
  cor.89.13[j] <- cor(as.vector(mod2$states), 
                      win.slp2[rownames(win.slp2) %in% 1989:2013,j])  
  
}

# plot the resulting maps
# set the limit for plotting so that it is identical across panels

zlim <- range(cor.65.88, cor.89.13)

# set colors 
new.col <- oce.colorsPalette(64)

png("figs/Bering climate era dfa-slp correlations.png", 8, 3, units="in", res=300)

par(mfrow=c(1,2), mar=c(1,1,1,1), las=1)

# 1965-1988
z <- cor.65.88
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
mtext("1965-1988")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1989-2013
z <- cor.89.13
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
mtext("1989-2013")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

dev.off()

###############
# and regression
regr.65.88 <- regr.89.13 <- NA

# try regression

for(j in 1:ncol(win.slp2)){
  
  regr.65.88[j] <- 
    summary(lm(as.vector(mod1$states) ~ 
                 win.slp2[rownames(win.slp2) %in% 1965:1988,j]))$coeff[2,2]
  
  regr.89.13[j] <- 
    summary(lm(as.vector(mod2$states) ~
                 win.slp2[rownames(win.slp2) %in% 1989:2013,j]))$coeff[2,2] 
  
}

# plot the resulting maps
# set the limit for plotting so that it is identical across panels

zlim <- range(regr.65.88, regr.89.13)



# set colors 
new.col <- oce.colorsPalette(64)

png("figs/slp-Bering climate era dfa regression.png", 8, 3, units="in", res=300)

par(mfrow=c(1,2), mar=c(1,1,1,1), las=1)


# 1965-1988
z <- regr.65.88
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("1965-1988")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1989-2013
z <- regr.89.13
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("1989-2013")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)


dev.off()

# so..regr maps NOT informative...correlation maps seem to suggest a declining influence of Arctic SLP in the second era!

# fit a single DFA to the entire TS and fit stan models to PDO/NPGO/AO
# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1) {  # allowing up to 1 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:2013], model=dfa.model, control=cntl.list, 
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

# unconstrained!
model.list = list(A="zero", m=1, R="unconstrained")
mod = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# fit stan models to AO-trend 1 relationship
library(tidyverse)
library(rstan)
library(ggplot2)
library(rstanarm)
library(bayesplot)

dat <- read.csv("data/climate data.csv")

stan.data <- data.frame(year=1965:2013,
                        trend=as.vector(mod$states),
                        AO.jfm=dat$AO.jfm[dat$year %in% 1965:2013])

stan.data$era <- ifelse(stan.data$year %in% 1965:1988, "1965-1988", "1989-2013")

# AO.stan <- stan_glm(trend ~ era + AO.jfm + AO.jfm:era,
                            # data = stan.data,
                            # chains = 4, cores = 4, thin = 1, seed=421,
                            # warmup = 1000, iter = 4000, refresh = 0,
                            # prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            # prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            # prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

scatter <- ggplot(stan.data, aes(AO.jfm, trend, color=era)) +
  theme_bw() +
  geom_point() +
  scale_color_manual(values=cb[2:4]) +
  geom_smooth(method="lm", se=F) +
  xlab("AO Index (Jan-Mar)") +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')

ggsave("figs/AO vs climate dfa scatter.png", width = 3.5, height = 3, units="in")

# load pdo/npgo and run the same analysis
dat2 <- read.csv("data/winter pdo-npgo.csv")

# plot to check
check <- dat2 %>%
  pivot_longer(cols=-year)

ggplot(check, aes(year, value, color=name)) + 
  theme_bw() +
  geom_line()
  
stan.data <- left_join(stan.data, dat2)

# an NPI!
dat3 <- read.csv("data/winter.NPI.csv")

stan.data <- left_join(stan.data, dat3)

# add ALBSA
dat4 <- read.csv("data/monthly albsa.csv")
stan.data <- left_join(stan.data, dat4)

# add mean Arctic SLP
dat5 <- read.csv("data/mean winter arctic slp.csv")
stan.data <- left_join(stan.data, dat5)


plot <- stan.data %>%
  select(-year) %>%
  pivot_longer(cols=c(-trend, -era))

scatter <- ggplot(plot, aes(value, trend, color=era)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~name, scales="free_x") +
  scale_color_manual(values=cb[2:3]) +
  geom_smooth(method="lm", se=F) +
  xlab("Index value") +
  ylab("Climate trend value") +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')

ggsave("figs/ALBSA AO Arctic SLP NPGO NPI PDO vs climate dfa scatter.png", width = 10, height = 8, units="in")

# scale albsa, arctic.slp, NPI
stan.data$albsa.djf <- scale(stan.data$albsa.djf)
stan.data$arctic.slp.ndjfm <- scale(stan.data$arctic.slp.ndjfm)
stan.data$NPI.ndjfm <- scale(stan.data$NPI.ndjfm)

stan.this <- stan.data %>%
select(-year, -AO.jfm, -npgo.ndjfm, -albsa.mam) %>%
  pivot_longer(cols=c(-trend, -era))

# stan era-specific regressions
albsa.djf.stan <- stan_glm(trend ~ era + value + value:era,
                    data = stan.this[stan.this$name=="albsa.djf",],
                    chains = 4, cores = 4, thin = 1,
                    warmup = 1000, iter = 4000, refresh = 0,
                    prior = normal(location = 0, scale = 5, autoscale = FALSE),
                    prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                    prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

arctic.slp.ndjfm.stan <- stan_glm(trend ~ era + value + value:era,
                                  data = stan.this[stan.this$name=="arctic.slp.ndjfm",],
                           chains = 4, cores = 4, thin = 1,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

NPI.ndjfm.stan <- stan_glm(trend ~ era + value + value:era,
                           data = stan.this[stan.this$name=="NPI.ndjfm",],
                                  chains = 4, cores = 4, thin = 1,
                                  warmup = 1000, iter = 4000, refresh = 0,
                                  prior = normal(location = 0, scale = 5, autoscale = FALSE),
                                  prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                                  prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pdo.ndjfm.stan <- stan_glm(trend ~ era + value + value:era,
                           data = stan.this[stan.this$name=="pdo.ndjfm",],
                           chains = 4, cores = 4, thin = 1,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(albsa.djf.stan, arctic.slp.ndjfm.stan, NPI.ndjfm.stan, pdo.ndjfm.stan)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2013"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("value", "era1989-2013:value"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope <- plyr::rbind.fill(lst.slope)
mdf_slope <- reshape2::melt(coef_slope, id.vars = "key")


# plot intercepts
int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1965-1988", "1989-2013")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int)

ggsave("figs/era intercepts - large-scale modes on Bering climate DFA.png", width=10, height=8, units="in")

# plot slopes
slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(slope)

ggsave("figs/era slopes - climate variables on AO.png", width=10, height=8, units='in')

######################
# hmmmm....rolling regressions on PDO-trend relationship?

roll.dat <- stan.data %>%
  select(year, trend, pdo.ndjfm)

roll.dat$slope <- roll.dat$intercept <- NA

# 25-yr windows
for(i in 1977:2001){
  # i <- 1977
  temp <- roll.dat %>%
    filter(year %in% (i-12):(i+12))
  
  mod <- lm(temp$trend ~ temp$pdo.ndjfm)
  roll.dat$intercept[roll.dat$year==i] <- mod$coefficients[1]
  roll.dat$slope[roll.dat$year==i] <- mod$coefficients[2]  
}

plot <- roll.dat %>%
  select(year, intercept, slope) %>%
  pivot_longer(cols=-year)

plot <- na.omit(plot)
  
ggplot(plot, aes(year, value, color=name)) +
  theme_bw() +
  geom_line() +
  geom_vline(xintercept = 1988.5, lty=2)

# and AICc!
library(MuMIn)

roll.dat <- stan.data %>%
  select(-era) %>%
  pivot_longer(cols=c(-year, -trend))
  
vars <- unique(roll.dat$name)
AICc.out <- data.frame()

# 25-yr windows
for(j in 1:length(vars)){
  # j <- 1
  
  temp <- roll.dat %>%
    filter(name == vars[j])
  
 for(i in 1977:2001){
     # i <- 1977

  temp$era <- ifelse(temp$year <= i, "era1", "era2")
  
  mod <- lm(temp$trend ~ temp$value * temp$era)
  AICc.out <- rbind(AICc.out,
                    data.frame(variable=vars[j],
                               year=i,
                               AICc=AICc(mod)))
  
}
}

ggplot(AICc.out, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

# non-interaction model...
AICc.out <- data.frame()

# 25-yr windows
for(j in 1:length(vars)){
  # j <- 1
  
  temp <- roll.dat %>%
    filter(name == vars[j])
  
  for(i in 1977:2001){
    # i <- 1977
    
    temp$era <- ifelse(temp$year <= i, "era1", "era2")
    
    mod <- lm(temp$trend ~ temp$value + temp$era)
    AICc.out <- rbind(AICc.out,
                      data.frame(variable=vars[j],
                                 year=i,
                                 AICc=AICc(mod)))
    
  }
}

ggplot(AICc.out, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

# now! just fit a single model to 25-year periods..
AICc.out <- data.frame()

# 25-yr windows
for(j in 1:length(vars)){
  # j <- 1
  
  temp <- roll.dat %>%
    filter(name == vars[j])
  
  for(i in 1977:2001){
    # i <- 1977
    temp2 <- temp %>%
      filter(year %in% (i-12):(i+12))
    mod <- lm(scale(temp2$trend) ~ temp2$value)
    
    AICc.out <- rbind(AICc.out,
                      data.frame(variable=vars[j],
                                 year=i,
                                 AICc=AICc(mod)))
    
  }
}

ggplot(AICc.out, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")
#################################
# add more years to the rolling AICc analysis

# find best model for 1950-2013
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1) {  # allowing up to 1 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1950:2013], model=dfa.model, control=cntl.list, 
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

# unconstrained!
model.list = list(A="zero", m=1, R="unconstrained")
mod = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1951:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

stan.data <- data.frame(year=1951:2013,
                        trend=as.vector(mod$states),
                        AO.jfm=dat$AO.jfm[dat$year %in% 1951:2013])
# load pdo/npgo and run the same analysis
dat2 <- read.csv("data/winter pdo-npgo.csv")

# plot to check
check <- dat2 %>%
  pivot_longer(cols=-year)

ggplot(check, aes(year, value, color=name)) + 
  theme_bw() +
  geom_line()

stan.data <- left_join(stan.data, dat2)

# an NPI!
dat3 <- read.csv("data/winter.NPI.csv")

stan.data <- left_join(stan.data, dat3)

# add ALBSA
dat4 <- read.csv("data/monthly albsa.csv")
stan.data <- left_join(stan.data, dat4)

# add mean Arctic SLP
dat5 <- read.csv("data/mean winter arctic slp.csv")
stan.data <- left_join(stan.data, dat5)


roll.dat <- stan.data %>%
  pivot_longer(cols=c(-year, -trend))

vars <- unique(roll.dat$name)
AICc.out <- data.frame()

# 25-yr windows
for(j in 1:length(vars)){
  # j <- 1
  
  temp <- roll.dat %>%
    filter(name == vars[j])
  
  for(i in 1963:2001){
    # i <- 1977
    
    temp$era <- ifelse(temp$year <= i, "era1", "era2")
    
    mod <- lm(temp$trend ~ temp$value * temp$era)
    AICc.out <- rbind(AICc.out,
                      data.frame(variable=vars[j],
                                 year=i,
                                 AICc=AICc(mod)))
    
  }
}

ggplot(AICc.out, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

ggsave("figs/large-scale modes and bering climate dfa AICc.png", width=8, height=7, units='in')

#################################
# for comparison, look at the same analysis for GOA!!
dat <- read.csv("data/GOA data/GOA.climate.csv", row.names=1)

unique(dat$key)
# subset
keep <- c("NDJ.grad", "FMA.FW", "FMA.WS", "MJJ.UW", "FMA.SST", "FMA.SSH")

dfa.dat <- dat %>%
  filter(key %in% keep) %>% 
  pivot_wider(names_from="key", values_from = "value")

years <- dfa.dat$year

dfa.dat <- dfa.dat %>%
  select(-year)

dfa.dat <- as.matrix(t(dfa.dat))
colnames(dfa.dat) <- years
View(dfa.dat)

# find best error structure for 1-trend model

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
  for(m in 1) {  # allowing up to 1 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(dfa.dat, model=dfa.model, control=cntl.list, 
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

# unconstrained!
model.list = list(A="zero", m=1, R="unconstrained")
mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

stan.data <- data.frame(year=1950:2012,
                        trend=as.vector(mod$states))
# load pdo/npgo and run the same analysis
dat2 <- read.csv("data/winter pdo-npgo.csv")

# plot to check
check <- dat2 %>%
  pivot_longer(cols=-year)

ggplot(check, aes(year, value, color=name)) + 
  theme_bw() +
  geom_line()

stan.data <- left_join(stan.data, dat2)

# an NPI!
dat3 <- read.csv("data/winter.NPI.csv")

stan.data <- left_join(stan.data, dat3)


roll.dat <- stan.data %>%
  pivot_longer(cols=c(-year, -trend))

vars <- unique(roll.dat$name)
AICc.out <- data.frame()

# 25-yr windows
for(j in 1:length(vars)){
  # j <- 1
  
  temp <- roll.dat %>%
    filter(name == vars[j])
  
  for(i in 1962:2000){
    # i <- 1977
    
    temp$era <- ifelse(temp$year <= i, "era1", "era2")
    
    mod <- lm(temp$trend ~ temp$value * temp$era)
    AICc.out <- rbind(AICc.out,
                      data.frame(variable=vars[j],
                                 year=i,
                                 AICc=AICc(mod)))
    
  }
}

ggplot(AICc.out, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

ggsave("figs/large-scale modes and GOA climate dfa AICc.png", width=8, height=7, units='in')

# still quite confusing!

# try the simplest approach - 25 yr rolling correlations between PDO and individual
# time series in each ecosystem!

# for GOA, restrict to the time series known to show changing associations with PDO
dat <- read.csv("data/GOA data/GOA.climate.csv", row.names=1)
keep <- c("NDJ.grad", "FMA.FW", "FMA.WS", "FMA.SSH", "Papa")

dfa.dat <- dat %>%
  filter(key %in% keep) %>% 
  pivot_wider(names_from="key", values_from = "value")

dat2 <- read.csv("data/winter pdo-npgo.csv")

dat2 <- dat2 %>%
  select(-npgo.ndjfm)

dfa.dat <- left_join(dfa.dat, dat2)

dfa.dat <- dfa.dat %>%
  pivot_longer(cols=c(-year, -pdo.ndjfm))

# get rolling 25-yr correlations

goa.cor <- data.frame()
vars <- unique(dfa.dat$name)

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])

for(i in 1963:2000){
  # i <- 1990
  goa.cor <- rbind(goa.cor,
                   data.frame(year=i,
                              var=vars[j],
                              cor=cor(temp$pdo.ndjfm[temp$year %in% (i-12):(i+12)],
                                      temp$value[temp$year %in% (i-12):(i+12)])))
  
  
}}


ggplot(goa.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations")

ggsave("figs/rolling correlations - GOA climate vars and PDO.png", width=8, height=5, units='in')

# ok - that's a good example to compare with...now how about the Bering TS?
# load climate data 
dat <- read.csv("data/climate data.csv")

names(dat)

# subset
keep <- c("year", "m4.march.ice", "m5.march.ice", "NW.wind.May.Sep",
          "NW.wind.Oct.Apr", "SE.wind.May.Sep", "SE.wind.Oct.Apr", "south.sst.amj",
          "south.sst.ndjfm", "south.wind.stress.amj")

dfa.dat <- dat %>%
  select(keep) %>%
  pivot_longer(cols=-year)


dat2 <- read.csv("data/winter pdo-npgo.csv")

dat2 <- dat2 %>%
  select(-npgo.ndjfm)

dfa.dat <- left_join(dfa.dat, dat2)

# limit to 2013 and earlier
dfa.dat <- dfa.dat %>%
  filter(year <= 2013)

# get rolling 25-yr correlations

ebs.cor <- data.frame()
vars <- unique(dfa.dat$name)

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                var=vars[j],
                                cor=cor(temp$pdo.ndjfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


ggplot(ebs.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations with PDO")

ggsave("figs/rolling correlations - EBS climate vars and PDO.png", width=8, height=6, units='in')

# add other large-scale modes!

dat3 <- read.csv("data/climate data.csv")
dat3 <- dat3 %>%
  select(year, AO.jfm)
dfa.dat <- left_join(dfa.dat, dat3)

# add ALBSA
dat4 <- read.csv("data/monthly albsa.csv")
dfa.dat <- left_join(dfa.dat, dat4)

# add mean Arctic SLP
dat5 <- read.csv("data/mean winter arctic slp.csv")
dfa.dat <- left_join(dfa.dat, dat5)

# now rolling AO corrs
# get rolling 25-yr correlations

ebs.cor <- data.frame()
vars <- unique(dfa.dat$name)

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                var=vars[j],
                                cor=cor(temp$AO.jfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


ggplot(ebs.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations with AO")

ggsave("figs/rolling correlations - EBS climate vars and AO.png", width=8, height=6, units='in')


############
# now rolling spring ALBSA corrs
# get rolling 25-yr correlations

ebs.cor <- data.frame()
vars <- unique(dfa.dat$name)

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                var=vars[j],
                                cor=cor(temp$albsa.mam[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


ggplot(ebs.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations with spring ALBSA") # these are totally weak!!

ggsave("figs/rolling correlations - EBS climate vars and spring ALBSA.png", width=8, height=6, units='in')

############
# now rolling winter ALBSA corrs
# get rolling 25-yr correlations

ebs.cor <- data.frame()
vars <- unique(dfa.dat$name)

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                var=vars[j],
                                cor=cor(temp$albsa.djf[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


ggplot(ebs.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations with winter ALBSA") # these are stronger than spring...

ggsave("figs/rolling correlations - EBS climate vars and winter ALBSA.png", width=8, height=6, units='in')


############
# now Arctic SLP
# get rolling 25-yr correlations

ebs.cor <- data.frame()
vars <- unique(dfa.dat$name)

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                var=vars[j],
                                cor=cor(temp$arctic.slp.ndjfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


ggplot(ebs.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations with winter Artic SLP") # these are totally weak!!

ggsave("figs/rolling correlations - EBS climate vars and winter Arctic SLP.png", width=8, height=6, units='in')

#################################
# old stuff below!
#################################
plot <- roll.dat %>%
  select(year, AICc)

plot <- na.omit(plot)

ggplot(plot, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  geom_vline(xintercept = 1988.5, lty=2) +
  ggtitle("PDO")

# other variables!
roll.dat <- stan.data %>%
  select(year, trend, AO.jfm)

roll.dat$AICc <- NA
roll.dat$era <- as.factor(ifelse(roll.dat$year <=1988, "era1", "era2"))

# 25-yr windows
for(i in 1977:2001){
  # i <- 1977
  temp <- roll.dat %>%
    filter(year %in% (i-12):(i+12))
  
  mod <- lm(temp$trend ~ temp$AO.jfm*temp$era)
  roll.dat$AICc[roll.dat$year==i] <- AICc(mod)
}

plot <- roll.dat %>%
  select(year, AICc)

plot <- na.omit(plot)

ggplot(plot, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  geom_vline(xintercept = 1988.5, lty=2) +
  ggtitle("AO")

# 
roll.dat <- stan.data %>%
  select(year, trend, albsa.djf)

roll.dat$AICc <- NA
roll.dat$era <- as.factor(ifelse(roll.dat$year <=1988, "era1", "era2"))

# 25-yr windows
for(i in 1977:2001){
  # i <- 1977
  temp <- roll.dat %>%
    filter(year %in% (i-12):(i+12))
  
  mod <- lm(temp$trend ~ temp$albsa.djf*temp$era)
  roll.dat$AICc[roll.dat$year==i] <- AICc(mod)
}

plot <- roll.dat %>%
  select(year, AICc)

plot <- na.omit(plot)

ggplot(plot, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  geom_vline(xintercept = 1988.5, lty=2) + 
  ggtitle("ALBSA")

# 
roll.dat <- stan.data %>%
  select(year, trend, arctic.slp.ndjfm)

roll.dat$AICc <- NA
roll.dat$era <- as.factor(ifelse(roll.dat$year <=1988, "era1", "era2"))

# 25-yr windows
for(i in 1977:2001){
  # i <- 1977
  temp <- roll.dat %>%
    filter(year %in% (i-12):(i+12))
  
  mod <- lm(temp$trend ~ temp$arctic.slp.ndjfm*temp$era)
  roll.dat$AICc[roll.dat$year==i] <- AICc(mod)
}

plot <- roll.dat %>%
  select(year, AICc)

plot <- na.omit(plot)

ggplot(plot, aes(year, AICc)) +
  theme_bw() +
  geom_line() +
  geom_vline(xintercept = 1988.5, lty=2) + 
  ggtitle("Arctic SLP")

