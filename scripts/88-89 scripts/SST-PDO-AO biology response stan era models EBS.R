# fit era-dependent linear regression models in stan
# to test hypothesis that biology responses to climate variability 
# changed across 1988/89 in the EBS, as in the GOA

library(tidyverse)
library(zoo)

cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load recruitment time series
d <- read.csv("data/EBS.recruit.time.series.csv")
head(d)


# and load Bristol Bay sockeye catches - note these are lagged 2 years
# to align with ocean entry, following Hare and Mantua 2000
d2 <- read.csv("data/BB sockeye catch.csv")

d <- left_join(d, d2) %>%
  select(-Opilio.age0.recruits, -cod.age0.R)

names(d)

names(d)[2:6] <- c("yellowfin.sole",
                   "Greenland.turbot",
                   "flathead.sole",
                   "walleye.pollock",
                   "sockeye.salmon")

# rename and limit to 1965-2012
dat <- d %>%
  filter(year %in% 1965:2012)

# and add SST/PDO/AO

cdat <- read.csv("data/climate data.csv")
head(cdat)

cdat$sst <- rollmean(cdat$south.sst.ndjfm, 2, align = "right", fill=NA)
cdat$ao <- rollmean(cdat$AO.jfm, 2, align = "right", fill=NA)

cdat <- cdat %>%
  select(year, sst, ao)

dat <- left_join(dat, cdat)

goa.dat <- read.csv("data/GOA data/GOA community data Litzow et al 2019 Ecology.csv")

goa.dat <- goa.dat %>%
  select(year, win.PDO2)

names(goa.dat)[2] <- "pdo"

dat <- left_join(dat, goa.dat)

# ready to analyze!

# first, scale climate variables
dat$sst <- scale(dat$sst)
dat$pdo <- scale(dat$pdo)
dat$ao <- scale(dat$ao)

# and log transform all the recruitment TS
dat$yellowfin.sole <- log(dat$yellowfin.sole*1000) 
dat$Greenland.turbot <- log(dat$Greenland.turbot)
dat$flathead.sole <- log(dat$flathead.sole)
dat$walleye.pollock <- log(dat$walleye.pollock)

# now plot relative to SST!
dat$era <- ifelse(dat$year <= 1988, "1965-1988", "1989-2012")

dat <- dat %>%
  select(-year) %>%
  pivot_longer(cols=c(-sst, -pdo, -ao, -era), names_to = 'key')



scatter.sst <- ggplot(dat, aes(sst, value, color=era)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~key, scales="free_y") +
  scale_color_manual(values=cb[2:4], labels=c("1965-1988", "1989-2012")) +
  geom_smooth(method="lm", se=F) +
  labs(x="SST (Nov-Mar)", y="Anomaly") +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')

scatter.sst

# and analyze era relationships with stan models
library(rstan)
library(ggplot2)
library(plyr)
library(rstanarm)
library(bayesplot)


#era as factor
change <- grep("1965", dat$era)
dat$era[change] <- 1
dat$era[-change] <- 2

dat$era <- as.factor(dat$era)

names <- unique(dat$key)[order(unique(dat$key))]


## fit a model with era-specific intercepts and slopes

flathead.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                           data = dat[dat$key == names[1], ],
                           chains = 4, cores = 4, thin = 1, seed=421,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

turbot.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                        data = dat[dat$key == names[2], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sockeye.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                     data = dat[dat$key == names[3], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pollock.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                     data = dat[dat$key == names[4], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

yellowfin.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                         data = dat[dat$key == names[5], ],
                         chains = 4, cores = 4, thin = 1, seed=421,
                         warmup = 1000, iter = 4000, refresh = 0,
                         prior = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

lst <- list(flathead.sst, turbot.sst, sockeye.sst, pollock.sst, yellowfin.sst)

lst <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era2"))
  data.frame(key = unique(x$data$key),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm.sst <- plyr::rbind.fill(lst)
mdf_indv_arm.sst <- reshape2::melt(coef_indv_arm.sst, id.vars = "key")


# for(i in 1:length(unique(coef_indv_arm$key))) {
# 
#   sub = dplyr::filter(coef_indv_arm, key == unique(coef_indv_arm$key)[i])
#   # calculate pairwise overlaps in slopes and intercepts
#   int_overlap = overlapping::overlap(x = list(int1 = sub$era1,int2=sub$era2,int3=sub$era3))
#   saveRDS(int_overlap$OV,file=paste0("output/", sub$key[1], "_climate_int_overlap.rds"))
# 
# }

## extract slopes
lst <- list(flathead.sst, turbot.sst, sockeye.sst, pollock.sst, yellowfin.sst)

lst.slope.sst <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("sst", "era2:sst"))
  data.frame(key = unique(x$data$key),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope.sst <- plyr::rbind.fill(lst.slope.sst)
mdf_slope.sst <- reshape2::melt(coef_slope.sst, id.vars = "key")

# calculate overlap in slopes
sst.overlap <- data.frame()

for(i in 1:length(unique(coef_slope.sst$key))) {
  
  sub = dplyr::filter(coef_slope.sst, key == unique(coef_slope.sst$key)[i])
  # calculate pairwise overlaps in slopes
  int_overlap = overlapping::overlap(x = list(slope1 = sub$era1,slope2=sub$era2))
  sst.overlap <- rbind(sst.overlap,
                       data.frame(species=unique(coef_slope.sst$key)[i],
                                  overlap=int_overlap$OV))
  
  
}

int_tab.sst <- plyr::ddply(mdf_indv_arm.sst, .(key, variable), summarize,
                           mean = mean(value),
                           lower95 = quantile(value, probs = 0.025),
                           upper95 = quantile(value, probs = 0.975))

slope_tab.sst <- plyr::ddply(mdf_slope.sst, .(key, variable), summarize,
                             mean = mean(value),
                             lower95 = quantile(value, probs = 0.025),
                             upper95 = quantile(value, probs = 0.975))



# 
# write.csv(int_tab, "output/climate intercept table.csv")
# 
# write.csv(slope_tab, "output/climate slope table.csv")

int.sst <- ggplot(mdf_indv_arm.sst, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int.sst)


slope.sst <- ggplot(mdf_slope.sst, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free", ncol=3)
print(slope.sst)

########################
# and now, PDO
flathead.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                         data = dat[dat$key == names[1], ],
                         chains = 4, cores = 4, thin = 1, seed=421,
                         warmup = 1000, iter = 4000, refresh = 0,
                         prior = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

turbot.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                       data = dat[dat$key == names[2], ],
                       chains = 4, cores = 4, thin = 1, seed=421,
                       warmup = 1000, iter = 4000, refresh = 0,
                       prior = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sockeye.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                        data = dat[dat$key == names[3], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pollock.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                        data = dat[dat$key == names[4], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

yellowfin.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                          data = dat[dat$key == names[5], ],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

lst <- list(flathead.pdo, turbot.pdo, sockeye.pdo, pollock.pdo, yellowfin.pdo)

lst <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era2"))
  data.frame(key = unique(x$data$key),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm.pdo <- plyr::rbind.fill(lst)
mdf_indv_arm.pdo <- reshape2::melt(coef_indv_arm.pdo, id.vars = "key")


# for(i in 1:length(unique(coef_indv_arm$key))) {
# 
#   sub = dplyr::filter(coef_indv_arm, key == unique(coef_indv_arm$key)[i])
#   # calculate pairwise overlaps in slopes and intercepts
#   int_overlap = overlapping::overlap(x = list(int1 = sub$era1,int2=sub$era2,int3=sub$era3))
#   saveRDS(int_overlap$OV,file=paste0("output/", sub$key[1], "_climate_int_overlap.rds"))
# 
# }

## extract slopes
lst <- list(flathead.pdo, turbot.pdo, sockeye.pdo, pollock.pdo, yellowfin.pdo)

lst.slope.pdo <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("pdo", "era2:pdo"))
  data.frame(key = unique(x$data$key),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope.pdo <- plyr::rbind.fill(lst.slope.pdo)
mdf_slope.pdo <- reshape2::melt(coef_slope.pdo, id.vars = "key")

# calculate overlap in slopes
pdo.overlap <- data.frame()

for(i in 1:length(unique(coef_slope.pdo$key))) {
  
  sub = dplyr::filter(coef_slope.sst, key == unique(coef_slope.pdo$key)[i])
  # calculate pairwise overlaps in slopes
  int_overlap = overlapping::overlap(x = list(slope1 = sub$era1,slope2=sub$era2))
  pdo.overlap <- rbind(pdo.overlap,
                       data.frame(species=unique(coef_slope.pdo$key)[i],
                                  overlap=int_overlap$OV))
  
  
}

int_tab.pdo <- plyr::ddply(mdf_indv_arm.pdo, .(key, variable), summarize,
                           mean = mean(value),
                           lower95 = quantile(value, probs = 0.025),
                           upper95 = quantile(value, probs = 0.975))

slope_tab.pdo <- plyr::ddply(mdf_slope.pdo, .(key, variable), summarize,
                             mean = mean(value),
                             lower95 = quantile(value, probs = 0.025),
                             upper95 = quantile(value, probs = 0.975))



# 
# write.csv(int_tab, "output/climate intercept table.csv")
# 
# write.csv(slope_tab, "output/climate slope table.csv")

int.pdo <- ggplot(mdf_indv_arm.pdo, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1965-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int.pdo)


slope.pdo <- ggplot(mdf_slope.pdo, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1965-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free", ncol=3)
print(slope.pdo)

#################
# and AO
flathead.ao <- stan_glm(scale(value) ~ era + ao + ao:era,
                         data = dat[dat$key == names[1], ],
                         chains = 4, cores = 4, thin = 1, seed=421,
                         warmup = 1000, iter = 4000, refresh = 0,
                         prior = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

turbot.ao <- stan_glm(scale(value) ~ era + ao + ao:era,
                       data = dat[dat$key == names[2], ],
                       chains = 4, cores = 4, thin = 1, seed=421,
                       warmup = 1000, iter = 4000, refresh = 0,
                       prior = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sockeye.ao <- stan_glm(scale(value) ~ era + ao + ao:era,
                        data = dat[dat$key == names[3], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pollock.ao <- stan_glm(scale(value) ~ era + ao + ao:era,
                        data = dat[dat$key == names[4], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

yellowfin.ao <- stan_glm(scale(value) ~ era + ao + ao:era,
                          data = dat[dat$key == names[5], ],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

lst <- list(flathead.ao, turbot.ao, sockeye.ao, pollock.ao, yellowfin.ao)

lst <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era2"))
  data.frame(key = unique(x$data$key),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm.ao <- plyr::rbind.fill(lst)
mdf_indv_arm.ao <- reshape2::melt(coef_indv_arm.ao, id.vars = "key")


# for(i in 1:length(unique(coef_indv_arm$key))) {
# 
#   sub = dplyr::filter(coef_indv_arm, key == unique(coef_indv_arm$key)[i])
#   # calculate pairwise overlaps in slopes and intercepts
#   int_overlap = overlapping::overlap(x = list(int1 = sub$era1,int2=sub$era2,int3=sub$era3))
#   saveRDS(int_overlap$OV,file=paste0("output/", sub$key[1], "_climate_int_overlap.rds"))
# 
# }

## extract slopes
lst <- list(flathead.ao, turbot.ao, sockeye.ao, pollock.ao, yellowfin.ao)

lst.slope.ao <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("ao", "era2:ao"))
  data.frame(key = unique(x$data$key),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope.ao <- plyr::rbind.fill(lst.slope.ao)
mdf_slope.ao <- reshape2::melt(coef_slope.ao, id.vars = "key")

# calculate overlap in slopes

# for(i in 1:length(unique(coef_slope$key))) {
#  
#   sub = dplyr::filter(coef_slope, key == unique(coef_slope$key)[i])
#   # calculate pairwise overlaps in slopes and intercepts
#   int_overlap = overlapping::overlap(x = list(slope1 = sub$era1,slope2=sub$era2,slope3=sub$era3))
#   saveRDS(int_overlap$OV,file=paste0("output/", sub$key[1], "_climate_slope_overlap.rds"))
#   
# 
# }

int_tab.ao <- plyr::ddply(mdf_indv_arm.ao, .(key, variable), summarize,
                           mean = mean(value),
                           lower95 = quantile(value, probs = 0.025),
                           upper95 = quantile(value, probs = 0.975))

slope_tab.ao <- plyr::ddply(mdf_slope.ao, .(key, variable), summarize,
                             mean = mean(value),
                             lower95 = quantile(value, probs = 0.025),
                             upper95 = quantile(value, probs = 0.975))



# 
# write.csv(int_tab, "output/climate intercept table.csv")
# 
# write.csv(slope_tab, "output/climate slope table.csv")

int.ao <- ggplot(mdf_indv_arm.ao, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int.ao)


slope.ao <- ggplot(mdf_slope.ao, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free", ncol=3)
print(slope.ao)

# weak AO effects overall!!

# could focus just on changes in slope for GOA and EBS....
# and plot as violin plots to reduce ink:info ratio

# and a violin plot of slopes on SST/PDO


mdf_slope.pdo$driver <- "PDO"
mdf_slope.pdo$driver.order <- 2

mdf_slope.sst$driver <- "SST"
mdf_slope.sst$driver.order <- 1

mdf_slope.ao$driver <- "AO"
mdf_slope.ao$driver.order <- 3

slopes <- rbind(mdf_slope.pdo, mdf_slope.sst, mdf_slope.ao)

rank <- tapply(mdf_slope.sst$value[mdf_slope.sst$variable=="era1"], mdf_slope.sst$key[mdf_slope.sst$variable=="era1"], median, na.rm=T)

slopes$rank <- rank[match(slopes$key, names(rank))]
slopes$key <- reorder(slopes$key, -slopes$rank)

slopes$plot.era <- ifelse(slopes$variable=="era1", "1965-1988", "1989-2013")


# and order eras!
slopes$era.order <- ifelse(slopes$plot.era=="1965-1988", 2, 1)
slopes$plot.era <- reorder(slopes$plot.era, slopes$era.order)

slopes$driver.order <- ifelse(slopes$driver=="SST", 1, 
                              ifelse(slopes$driver=="PDO", 2, 3))
slopes$driver <- reorder(slopes$driver, slopes$driver.order)

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(slopes, aes(key, value, fill=plot.era)) +
  geom_violin(color=NA) +
  scale_fill_manual(values=cb[c(4,2)]) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) +
  facet_wrap(~driver) +
  ylim(-1.5, 1.3) +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top',
        title = element_text(size=9)) +
  coord_flip() + ylab("Slope") + 
  guides(fill=guide_legend(reverse=T))

ggsave("figs/EBS biology on SST PDO AO violin.png", width=6, height=3, units='in')

# plot overlaps
sst.overlap$driver <- "SST"
pdo.overlap$driver <- "PDO"

overlaps <- rbind(sst.overlap, pdo.overlap)


ebs.slope.overlap <- ggplot(overlaps, aes(overlap)) +
  theme_bw() +
  facet_grid(~driver, scale="free") +
  geom_histogram(position="dodge", bins=8, fill=cb[2], color="black") +
  xlab("Overlap (%)") 

ebs.slope.overlap

# make a combined plot with GOA overlap
png("figs/EBS and GOA slope overlaps.png", 6, 6, units='in', res=300)
ggpubr::ggarrange(goa.slope.overlap, ebs.slope.overlap, ncol=1, nrow=2, labels=c("a) GOA", "b) EBS"))
dev.off()

# note - need to align GOA / EBS axis ranges!

# and finally, plot probabillity of slope = 0, by era

# calculate prob of slope != 0
prob.0 <- data.frame()

for(i in 1:length(unique(slopes$key))) {
  
  sub = dplyr::filter(slopes, key == unique(slopes$key)[i])
  
  
  prob.pdo1 = ifelse(mean(sub$value[sub$driver=="PDO" & sub$variable=="era1"])>0, 
                     sum(sub$value[sub$driver=="PDO" & sub$variable=="era1"]>0)/length(sub$value[sub$driver=="PDO" & sub$variable=="era1"]),
                     sum(sub$value[sub$driver=="PDO" & sub$variable=="era1"]<0)/length(sub$value[sub$driver=="PDO" & sub$variable=="era1"]))
  
  prob.pdo2 = ifelse(mean(sub$value[sub$driver=="PDO" & sub$variable=="era2"])>0, 
                     sum(sub$value[sub$driver=="PDO" & sub$variable=="era2"]>0)/length(sub$value[sub$driver=="PDO" & sub$variable=="era2"]),
                     sum(sub$value[sub$driver=="PDO" & sub$variable=="era2"]<0)/length(sub$value[sub$driver=="PDO" & sub$variable=="era2"]))
  
  prob.sst1 = ifelse(mean(sub$value[sub$driver=="SST" & sub$variable=="era1"])>0, 
                     sum(sub$value[sub$driver=="SST" & sub$variable=="era1"]>0)/length(sub$value[sub$driver=="SST" & sub$variable=="era1"]),
                     sum(sub$value[sub$driver=="SST" & sub$variable=="era1"]<0)/length(sub$value[sub$driver=="SST" & sub$variable=="era1"]))
  
  prob.sst2 = ifelse(mean(sub$value[sub$driver=="SST" & sub$variable=="era2"])>0, 
                     sum(sub$value[sub$driver=="SST" & sub$variable=="era2"]>0)/length(sub$value[sub$driver=="SST" & sub$variable=="era2"]),
                     sum(sub$value[sub$driver=="SST" & sub$variable=="era2"]<0)/length(sub$value[sub$driver=="SST" & sub$variable=="era2"]))
  
  prob.0 <- rbind(prob.0,
                  data.frame(species=rep(unique(slopes$key)[i], 2),
                             pdo = c(prob.pdo1, prob.pdo2),
                             sst = c(prob.sst1, prob.sst2),
                             era = c("era1", "era2")))
}

prob.0 <- prob.0 %>%
  pivot_longer(cols = c(-species, -era))

# note that e.g. northern shrimp-PDO changes sign, which this plot misses!
ebs.slope.prob.0 <- ggplot(prob.0, aes(value)) +
  theme_bw() +
  facet_grid(era~name, scale="free") +
  geom_histogram(position="dodge", bins=12, fill=cb[2], color="black") +
  xlab("Probability of slope < or > 0") 

ebs.slope.prob.0

# make a combined plot with GOA 
png("figs/EBS and GOA slope proability not 0.png", 6, 4, units='in', res=300)
ggpubr::ggarrange(goa.slope.prob.0, ebs.slope.prob.0, ncol=2, nrow=1, labels=c("a) GOA", "b) EBS"))
dev.off()


# and finally (finally!), check for autocorrelated residuals in linear fits to SST

# first, fit stationary models
flathead.stationary <- stan_glm(scale(value) ~ sst,
                         data = dat[dat$key == names[1], ],
                         chains = 4, cores = 4, thin = 1, seed=421,
                         warmup = 1000, iter = 4000, refresh = 0,
                         prior = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

turbot.stationary <- stan_glm(scale(value) ~ sst,
                       data = dat[dat$key == names[2], ],
                       chains = 4, cores = 4, thin = 1, seed=421,
                       warmup = 1000, iter = 4000, refresh = 0,
                       prior = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sockeye.stationary <- stan_glm(scale(value) ~ sst,
                        data = dat[dat$key == names[3], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pollock.stationary <- stan_glm(scale(value) ~ sst,
                        data = dat[dat$key == names[4], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

yellowfin.stationary <- stan_glm(scale(value) ~ sst,
                          data = dat[dat$key == names[5], ],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))


# now combine residuals for stationary and nonstationary models
sst.resid <- data.frame(year=rep(1965:2012, length(names)),
                        species=rep(names, each=length(1965:2012)),
                        stationary=c(resid(flathead.stationary),
                                           resid(turbot.stationary),
                                           resid(sockeye.stationary),
                                           resid(pollock.stationary),
                                           resid(yellowfin.stationary)),
                        nonstationary=c(resid(flathead.sst),
                                              resid(turbot.sst),
                                              resid(sockeye.sst),
                                              resid(pollock.sst),
                                              resid(yellowfin.sst)))
sst.resid <- sst.resid %>%
  pivot_longer(cols=c(-year, -species))

ggplot(sst.resid, aes(year, value, color=name, fill=name)) +
  theme_bw() +
  facet_wrap(~species) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=cb[5:6])
  

# Fig.1 - Regression of SLP on GOA/EBS SST by era
# Fig.2 - slopes on SST/PDO for GOA
# Fig.3 - Rolling correlations for non-SST climate variables on SST / PDO for GOA (and Bayesian era slopes??)
#         or slope of % of post-88/89 data in each 25-yr window vs the strength of relationship...          
# Fig.4 - Slopes on SST/PDO/AO for EBS
# Fig.5 - DFA loadings/trends for ice and wind time series EBS
# Fig.6 - Rolling correlations (and Bayesian slopes?) for DFA trends on SST/PDO/AO