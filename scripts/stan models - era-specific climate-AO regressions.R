library(tidyverse)
library(rstan)
library(ggplot2)
library(plyr)
library(rstanarm)
library(bayesplot)

# load climate data 
dat <- read.csv("data/climate data.csv")

names(dat)

# check sample size for 1965-1988, 1989-2013, and 2014-2019

era1 <- dat %>%
  filter(year %in% 1965:1988)

era2 <- dat %>%
  filter(year %in% 1989:2013)

era3 <- dat %>%
  filter(year %in% 2014:2019)

ff <- function(x) sum(!is.na(x))

era1.n <- apply(era1, 2, ff)
era2.n <- apply(era2, 2, ff)
era3.n <- apply(era3, 2, ff)

# limit to 1965-2019 and add era factor
dat <- dat %>%
  filter(year>=1965)

dat$era <- ifelse(dat$year %in% 1965:1988, "era1",
                  ifelse(dat$year %in% 1989:2013, "era2", "era3"))

# gather (pivot longer!)
dat <- dat %>%
  pivot_longer(names_to = "name", values_to = "value", -c(year, era, AO.jfm))

# get sample sizes!
sample.size <- tapply(dat$value, list(dat$name, dat$era), ff)

# limit to TS with at least 10 obs in era 1!
keep <- row.names(sample.size)[sample.size[,1] >= 10]

dat.3.era <- dat %>%
  filter(name %in% keep)

# and plot the data!

# load colorblind palette 
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

scatter <- ggplot(dat.3.era, aes(AO.jfm, value, color=era)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~name, scales="free_y") +
  scale_color_manual(values=cb[2:4]) +
  geom_smooth(method="lm", se=F) +
  xlab("AO Index (Jan-Mar)") +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')

ggsave("figs/AO vs climate scatter.png", width = 10, height = 8, units="in")


# now fit models
south.sst.ndjfm <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                      data = dat.3.era[dat.3.era$name=="south.sst.ndjfm",],
                      chains = 4, cores = 4, thin = 1, seed=421,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

south.sst.amj <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="south.sst.amj",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

north.sst.ndjfm <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="north.sst.ndjfm",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

north.sst.amj <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                          data = dat.3.era[dat.3.era$name=="north.sst.amj",],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

SE.wind.Oct.Apr <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="SE.wind.Oct.Apr",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

NW.wind.Oct.Apr <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                          data = dat.3.era[dat.3.era$name=="NW.wind.Oct.Apr",],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

SE.wind.May.Sep <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="SE.wind.May.Sep",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

NW.wind.May.Sep <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                          data = dat.3.era[dat.3.era$name=="NW.wind.May.Sep",],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

ice.area.jfma <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="ice.area.jfma",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

m8.march.ice <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                          data = dat.3.era[dat.3.era$name=="m8.march.ice",],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

m5.march.ice <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="m5.march.ice",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

m4.march.ice <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="m4.march.ice",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

north.wind.stress.amj <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="north.wind.stress.amj",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

south.wind.stress.amj <- stan_glm(scale(value) ~ era + AO.jfm + AO.jfm:era,
                            data = dat.3.era[dat.3.era$name=="south.wind.stress.amj",],
                            chains = 4, cores = 4, thin = 1, seed=421,
                            warmup = 1000, iter = 4000, refresh = 0,
                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

lst <- list(south.sst.ndjfm, south.sst.amj, north.sst.ndjfm, north.sst.amj, SE.wind.Oct.Apr,      
             NW.wind.Oct.Apr, SE.wind.May.Sep, NW.wind.May.Sep, ice.area.jfma, m8.march.ice,         
             m5.march.ice, m4.march.ice, north.wind.stress.amj, south.wind.stress.amj)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "eraera2", "eraera3"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2],
             era3 = beta[ , 1] + beta[ , 3])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("AO.jfm", "eraera2:AO.jfm", "eraera3:AO.jfm"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2],
             era3 = beta[ , 1] + beta[ , 3])
})
coef_slope <- plyr::rbind.fill(lst.slope)
mdf_slope <- reshape2::melt(coef_slope, id.vars = "key")


# plot intercepts
int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int)

ggsave("figs/era intercepts - climate variables on AO.png", width=10, height=8, units="in")

# plot slopes
slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(slope)

ggsave("figs/era slopes - climate variables on AO.png", width=10, height=8, units='in')

#############################
# new approach - fit DFA to the TS showing some relationship to the AO and calculate rolling correlations,
# both with individual TS, and with AO

keep <- c("south.sst.ndjfm",  "SE.wind.Oct.Apr", "m8.march.ice",
  "m5.march.ice", "m4.march.ice", "south.wind.stress.amj")

dfa.dat <- dat.3.era %>%
  filter(name %in% keep)


# spread and transpose

dfa.dat <- dfa.dat %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(-AO.jfm, -era, -year)

dfa.dat <- as.matrix(t(dfa.dat))

colnames(dfa.dat) <- unique(dat.3.era$year)

# find best error structure for 1-trend model

library(MARSS)

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
    kemz = MARSS(dfa.dat, model=dfa.model,
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
# unconstrained error structure is far and away the best

# fit best model, plot loadings and trend
model.list = list(A="zero", m=1, R="unconstrained")
mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa")

# get CI and plot loadings...
modCI <- MARSSparamCIs(mod)

plot.CI <- data.frame(names=rownames(dfa.dat), mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
                           lowCI=modCI$par.lowCI$Z)

plot.CI$names <- reorder(plot.CI$names, plot.CI$mean)
dodge <- position_dodge(width=0.9)

ggplot(plot.CI, aes(x=names, y=mean)) + 
  geom_bar(position="dodge", stat="identity", fill=cb[2]) +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") + 
  xlab("") + 
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), axis.title.y = element_blank()) +
  geom_hline(yintercept = 0) 

# plot trend
plot.trend <- data.frame(t=as.numeric(colnames(dfa.dat)),estimate=as.vector(mod$states),
                         conf.low=as.vector(mod$states)-1.96*as.vector(mod$states.se),
                         conf.high=as.vector(mod$states)+1.96*as.vector(mod$states.se))


ggplot(plot.trend, aes(t, estimate)) +
  geom_line() + 
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) + xlab("") + ylab("Trend")

# now fit the rolling correlations
temp <- read.csv("data/climate data.csv")
AO.jfm <- temp$AO.jfm[temp$year >= 1965]

# load PDO/NPGO
pdo.npgo <- read.csv("data/winter pdo-npgo.csv")

pdo.ndjfm <- pdo.npgo$pdo.ndjfm[pdo.npgo$year %in% 1965:2019]
npgo.ndjfm <- pdo.npgo$npgo.ndjfm[pdo.npgo$year %in% 1965:2019]


cor.dat <- rbind(dfa.dat, AO.jfm, pdo.ndjfm, npgo.ndjfm)


correlations <- as.data.frame(matrix(nrow=31, ncol=(nrow(cor.dat))))
xx <- as.vector(mod$states)

for(i in 1965:1995){
 # i <- 1965
  
  temp <- cor.dat[,colnames(cor.dat) %in% i:(i+24)]
  
  for(ii in 1:nrow(cor.dat)){ # loop through each time series/variable
    
    correlations[(i-1964),ii] <- cor(temp[ii,], xx[(i-1964):(i-1964+24)], use="p")
    
  }}

# now  plot
colnames(correlations) <- rownames(cor.dat)
correlations$year <- 1977:2007
plot.cor <- pivot_longer(as.data.frame(correlations), -year, names_to = "key", values_to = "value") 

ggplot(plot.cor, aes(year, value)) +
  facet_wrap(~key) + geom_hline(yintercept = 0) + theme_bw() +
  geom_line(color=cb[2]) + 
  xlab("") + ylab("Correlation") + theme(axis.title.x = element_blank())

ggsave("figs/rolling 25-yr correlations bering climate dfa.png", width=10, height=8, units='in')

######################################
# same thing, but 15-yr correlations #

correlations <- as.data.frame(matrix(nrow=41, ncol=(nrow(cor.dat))))
xx <- as.vector(mod$states)

for(i in 1965:2005){
  # i <- 1965
  
  temp <- cor.dat[,colnames(cor.dat) %in% i:(i+14)]
  
  for(ii in 1:nrow(cor.dat)){ # loop through each time series/variable
    
    correlations[(i-1964),ii] <- cor(temp[ii,], xx[(i-1964):(i-1964+14)], use="p")
    
  }}

# now  plot
colnames(correlations) <- rownames(cor.dat)
correlations$year <- 1972:2012
plot.cor <- pivot_longer(as.data.frame(correlations), -year, names_to = "key", values_to = "value") 

ggplot(plot.cor, aes(year, value)) +
  facet_wrap(~key) + geom_hline(yintercept = 0) + theme_bw() +
  geom_line(color=cb[2]) + 
  xlab("") + ylab("Correlation") + theme(axis.title.x = element_blank())

ggsave("figs/rolling 15-yr correlations bering climate dfa.png", width=10, height=8, units='in')
