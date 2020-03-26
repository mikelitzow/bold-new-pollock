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

ggsave("figs/AO vs climate scatter.png", width = 10, height = 10, units="in")


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

ggsave("figs/era intercepts - climate variables on AO.png", width=10, height=10, units="in")

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

ggsave("figs/era slopes - climate variables on AO.png", width=10, height=10, units='in')


