library(gplots)
library(tidyr)
library(overlapping)
library(tidyverse)

# dat <- read.csv("data/GOA data/GOA_groundfish_recruitment.csv")

cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# names(dat)[1] <- "year"
# 
# # excluding CIs
# # and limit to 1965-2013
# dat <- dat %>%
#   select(-ATF_LI, -ATF_UI) %>%
#   filter(year %in% 1965:2013)
# 
# 
# # log transform
# dat$ATF_R0 <- log(dat$ATF_R0)
# dat$POLL_R0 <- log(dat$POLL_R0)

# changing from above - just use biology data from 2019 Ecology paper
dat <- read.csv("data/GOA data/GOA community data Litzow et al 2019 Ecology.csv")

# and rename/scale sst/pdo!
names(dat)[16:17] <- c("sst", "pdo")

dat$sst <- scale(dat$sst)
dat$pdo <- scale(dat$pdo)

# now plot relative to SST!
dat$era <- ifelse(dat$year <= 1988, "1965-1988", "1989-2012")

dat <- dat %>%
  select(-year) %>%
  pivot_longer(cols=c(-sst, -pdo, -era), names_to = 'key')



scatter.sst <- ggplot(dat, aes(sst, scale(value), color=era)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~key, scales="free_y", ncol=3) +
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

arrowtooth.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                      data = dat[dat$key == names[1], ],
                      chains = 4, cores = 4, thin = 1, seed=421,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

capelin.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                         data = dat[dat$key == names[2], ],
                         chains = 4, cores = 4, thin = 1, seed=421,
                         warmup = 1000, iter = 4000, refresh = 0,
                         prior = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

chum.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                       data = dat[dat$key == names[3], ],
                       chains = 4, cores = 4, thin = 1, seed=421,
                       warmup = 1000, iter = 4000, refresh = 0,
                       prior = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

coho.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                    data = dat[dat$key == names[4], ],
                    chains = 4, cores = 4, thin = 1, seed=421,
                    warmup = 1000, iter = 4000, refresh = 0,
                    prior = normal(location = 0, scale = 5, autoscale = FALSE),
                    prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                    prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

rockfish.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                           data = dat[dat$key == names[5], ],
                           chains = 4, cores = 4, thin = 1, seed=421,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

shrimp.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                        data = dat[dat$key == names[6], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

cod.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                     data = dat[dat$key == names[7], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

herring.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                     data = dat[dat$key == names[8], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pink.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                           data = dat[dat$key == names[9], ],
                           chains = 4, cores = 4, thin = 1, seed=421,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sablefish.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                        data = dat[dat$key == names[10], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sidestripe.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                     data = dat[dat$key == names[11], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sockeye.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                     data = dat[dat$key == names[12], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

Tanner.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                           data = dat[dat$key == names[13], ],
                           chains = 4, cores = 4, thin = 1, seed=421,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pollock.sst <- stan_glm(scale(value) ~ era + sst + sst:era,
                        data = dat[dat$key == names[14], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

lst <- list(arrowtooth.sst, capelin.sst, chum.sst, coho.sst, rockfish.sst, shrimp.sst, cod.sst, herring.sst,
            pink.sst, sablefish.sst, sidestripe.sst, sockeye.sst, Tanner.sst, pollock.sst)

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
lst <- list(arrowtooth.sst, capelin.sst, chum.sst, coho.sst, rockfish.sst, shrimp.sst, cod.sst, herring.sst,
            pink.sst, sablefish.sst, sidestripe.sst, sockeye.sst, Tanner.sst, pollock.sst)
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

###
# and repeat for PDO!

scatter.pdo <- ggplot(dat, aes(pdo, value, color=era)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~key, scales="free_y", ncol=3) +
  scale_color_manual(values=cb[2:4], labels=c("1965-1988", "1989-2012")) +
  geom_smooth(method="lm", se=F) +
  xlab("PDO (Nov-Mar)") +
  ylab("Anomaly") +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')

scatter.pdo

## fit a model with era-specific intercepts and slopes

arrowtooth.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                           data = dat[dat$key == names[1], ],
                           chains = 4, cores = 4, thin = 1, seed=421,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

capelin.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                        data = dat[dat$key == names[2], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

chum.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                     data = dat[dat$key == names[3], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

coho.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                     data = dat[dat$key == names[4], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

rockfish.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                         data = dat[dat$key == names[5], ],
                         chains = 4, cores = 4, thin = 1, seed=421,
                         warmup = 1000, iter = 4000, refresh = 0,
                         prior = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                         prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

shrimp.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                       data = dat[dat$key == names[6], ],
                       chains = 4, cores = 4, thin = 1, seed=421,
                       warmup = 1000, iter = 4000, refresh = 0,
                       prior = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

cod.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                    data = dat[dat$key == names[7], ],
                    chains = 4, cores = 4, thin = 1, seed=421,
                    warmup = 1000, iter = 4000, refresh = 0,
                    prior = normal(location = 0, scale = 5, autoscale = FALSE),
                    prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                    prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

herring.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                        data = dat[dat$key == names[8], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pink.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                     data = dat[dat$key == names[9], ],
                     chains = 4, cores = 4, thin = 1, seed=421,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sablefish.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                          data = dat[dat$key == names[10], ],
                          chains = 4, cores = 4, thin = 1, seed=421,
                          warmup = 1000, iter = 4000, refresh = 0,
                          prior = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                          prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sidestripe.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                           data = dat[dat$key == names[11], ],
                           chains = 4, cores = 4, thin = 1, seed=421,
                           warmup = 1000, iter = 4000, refresh = 0,
                           prior = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                           prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

sockeye.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                        data = dat[dat$key == names[12], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

Tanner.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                       data = dat[dat$key == names[13], ],
                       chains = 4, cores = 4, thin = 1, seed=421,
                       warmup = 1000, iter = 4000, refresh = 0,
                       prior = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                       prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

pollock.pdo <- stan_glm(scale(value) ~ era + pdo + pdo:era,
                        data = dat[dat$key == names[14], ],
                        chains = 4, cores = 4, thin = 1, seed=421,
                        warmup = 1000, iter = 4000, refresh = 0,
                        prior = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                        prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

lst <- list(arrowtooth.pdo, capelin.pdo, chum.pdo, coho.pdo, rockfish.pdo, shrimp.pdo, cod.pdo, herring.pdo,
            pink.pdo, sablefish.pdo, sidestripe.pdo, sockeye.pdo, Tanner.pdo, pollock.pdo)

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
lst <- list(arrowtooth.pdo, capelin.pdo, chum.pdo, coho.pdo, rockfish.pdo, shrimp.pdo, cod.pdo, herring.pdo,
            pink.pdo, sablefish.pdo, sidestripe.pdo, sockeye.pdo, Tanner.pdo, pollock.pdo)
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("pdo", "era2:pdo"))
  data.frame(key = unique(x$data$key),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope.pdo <- plyr::rbind.fill(lst.slope)
mdf_slope.pdo <- reshape2::melt(coef_slope.pdo, id.vars = "key")

# calculate overlap in slopes
pdo.overlap <- data.frame()

for(i in 1:length(unique(coef_slope.pdo$key))) {

  sub = dplyr::filter(coef_slope.pdo, key == unique(coef_slope.pdo$key)[i])
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
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int.pdo)


slope.pdo <- ggplot(mdf_slope.pdo, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free", ncol=3)
print(slope.pdo)

# for now, save only a plot of slopes on SST / PDO
png("figs/GOA biology slopes on PDO-SST.png", width=10, height=12, units='in', res=300)
ggpubr::ggarrange(scatter.sst, scatter.pdo, slope.sst, slope.pdo, ncol=2, nrow=2,
                  labels=c("a) SST", "b) PDO", "c) SST", "d) PDO"))
dev.off()


# and a violin plot of slopes on SST/PDO


mdf_slope.pdo$driver <- "PDO"
mdf_slope.pdo$driver.order <- 2

mdf_slope.sst$driver <- "SST"
mdf_slope.sst$driver.order <- 1

slopes <- rbind(mdf_slope.pdo, mdf_slope.sst)

rank <- tapply(mdf_slope.sst$value[mdf_slope.sst$variable=="era1"], mdf_slope.sst$key[mdf_slope.sst$variable=="era1"], median, na.rm=T)

slopes$rank <- rank[match(slopes$key, names(rank))]
slopes$key <- reorder(slopes$key, -slopes$rank)

slopes$plot.era <- ifelse(slopes$variable=="era1", "1965-1988", "1989-2013")


# and order eras!
slopes$era.order <- ifelse(slopes$plot.era=="1965-1988", 2, 1)
slopes$plot.era <- reorder(slopes$plot.era, slopes$era.order)

slopes$driver.order <- ifelse(slopes$driver=="SST", 1, 2)
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

ggsave("figs/GOA biology on SST PDO violin.png", width=4, height=6, units='in')

# now plot the era1 slopes vs era2 slopes for both pdo and sst

slope_tab.pdo$driver <- "PDO"
slope_tab.sst$driver <- "SST"

slope_tab_comb <- rbind(slope_tab.pdo, slope_tab.sst)

slope_tab_comb <- slope_tab_comb %>%
  select(-lower95, -upper95) 

ggplot(slope_tab_comb, aes(abs(mean))) +
  theme_bw() +
  facet_grid(variable~driver, scale="free") +
  geom_histogram(position="dodge", bins=7, fill=cb[2], color="black") +
  xlab("Slope (absolute value)")

# plot overlaps
sst.overlap$driver <- "SST"
pdo.overlap$driver <- "PDO"

overlaps <- rbind(sst.overlap, pdo.overlap)

overlaps$overlap.percentage <- 100*overlaps$overlap

goa.slope.overlap <- ggplot(overlaps, aes(overlap)) +
  theme_bw() +
  facet_grid(~driver, scale="free") +
  geom_histogram(position="dodge", bins=15, fill=cb[2], color="black") +
  xlab("Overlap") 

goa.slope.overlap

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
goa.slope.prob.0 <- ggplot(prob.0, aes(value)) +
  theme_bw() +
  facet_grid(era~name, scale="free") +
  geom_histogram(position="dodge", bins=12, fill=cb[2], color="black") +
  xlab("Probability of slope < or > 0") 

goa.slope.prob.0

## Bayesian model diagnostics
# Need Mike Malick's input on interpreting these!
post_era_NPI_2    <- as.array(era_NPI_2)
post_era_stress_2 <- as.array(era_stress_2)
post_era_SSH_2    <- as.array(era_SSH_2)
post_era_SST_2    <- as.array(era_SST_2)

mcmc_trace(post_era_NPI_2)
mcmc_trace(post_era_stress_2)
mcmc_trace(post_era_SSH_2)
mcmc_trace(post_era_SST_2)

mcmc_areas(post_era_NPI_2)
mcmc_areas(post_era_stress_2)
mcmc_areas(post_era_SSH_2)
mcmc_areas(post_era_SST_2)

range(summary(era_NPI_2[["stanfit"]])[["summary"]][ , "n_eff"])
range(summary(era_NPI_2[["stanfit"]])[["summary"]][ , "Rhat"])
range(summary(era_stress_2[["stanfit"]])[["summary"]][ , "n_eff"])
range(summary(era_stress_2[["stanfit"]])[["summary"]][ , "Rhat"])
range(summary(era_SSH_2[["stanfit"]])[["summary"]][ , "n_eff"])
range(summary(era_SSH_2[["stanfit"]])[["summary"]][ , "Rhat"])
range(summary(era_SST_2[["stanfit"]])[["summary"]][ , "n_eff"])
range(summary(era_SST_2[["stanfit"]])[["summary"]][ , "Rhat"])
