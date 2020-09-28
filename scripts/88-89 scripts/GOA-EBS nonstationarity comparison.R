library(tidyverse)
library(MARSS)

# fit DFA to long-term southeast Bering climate time series for different eras

# set colors
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load climate data
# dat <- read.csv("data/climate data.csv")
# 
# names(dat)
# 
# # subset
# keep <- c("ice.area.jfma", "m4.march.ice", "m5.march.ice", "NW.wind.May.Sep",
#           "NW.wind.Oct.Apr", "SE.wind.May.Sep", "SE.wind.Oct.Apr", "south.ao.amj",
#           "south.ao.ndjfm", "south.wind.stress.amj")
# 
# 
# dfa.dat <- dat %>%
#   select(keep) 
# 
# dfa.dat <- as.matrix(t(dfa.dat))
# colnames(dfa.dat) <- dat$year
# 
# 
# # find best error structure for 1-trend model
# 
# # changing convergence criterion to ensure convergence
# cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
# 
# # set up forms of R matrices
# levels.R = c("diagonal and equal",
#              "diagonal and unequal",
#              "equalvarcov",
#              "unconstrained")
# model.data = data.frame()
# 
# # fit models & store results
# for(R in levels.R) {
#   for(m in 1) {  # allowing up to 1 trends
#     dfa.model = list(A="zero", R=R, m=m)
#     kemz = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:1988], model=dfa.model, control=cntl.list, 
#                  form="dfa", z.score=TRUE)
#     model.data = rbind(model.data,
#                        data.frame(R=R,
#                                   m=m,
#                                   logLik=kemz$logLik,
#                                   K=kemz$num.params,
#                                   AICc=kemz$AICc,
#                                   stringsAsFactors=FALSE))
#     assign(paste("kemz", m, R, sep="."), kemz)
#   } # end m loop
# } # end R loop
# 
# # calculate delta-AICc scores, sort in descending order, and compare
# model.data$dAICc <- model.data$AICc-min(model.data$AICc)
# model.data <- model.data %>%
#   arrange(dAICc)
# 
# era1.models <- model.data
# 
# era1.models; era2.models
# # one trend, equalvarcov is best for era1
# # one trend, diagonal and unequal is best for era2
# 
# # fit models to each era, plot loadings and trend
# model.list = list(A="zero", m=1, R="equalvarcov")
# mod1 = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:1988], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# 
# model.list = list(A="zero", m=1, R="diagonal and unequal")
# mod2 = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1989:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# 
# # get CI and plot loadings...
# mod1.CI <- MARSSparamCIs(mod1)
# 
# mod2.CI <- MARSSparamCIs(mod2)
# 
# plot.CI <- data.frame(names=rep(rownames(dfa.dat),2), 
#                       era=rep(c("1965-1988", "1989-2013"), each=nrow(dfa.dat)),
#                       mean=c(mod1.CI$par$Z, mod2.CI$par$Z),
#                       upCI=c(mod1.CI$par.upCI$Z,mod2.CI$par.upCI$Z),
#                       lowCI=c(mod1.CI$par.lowCI$Z,mod2.CI$par.lowCI$Z))
# 
# # plot.CI$names <- reorder(plot.CI$names, plot.CI$mean)
# dodge <- position_dodge(width=0.9)
# 
# ggplot(plot.CI, aes(x=names, y=mean, fill=era)) + 
#   geom_bar(position=dodge, stat="identity") +
#   geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
#   ylab("Loading") + 
#   xlab("") + 
#   theme_bw() + 
#   theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), axis.title.y = element_blank()) +
#   geom_hline(yintercept = 0) 
# 
# # plot trend
# plot.trend <- data.frame(t=1965:2013,
#                          era=c(rep("1965-1988", length(1965:1988)), rep("1989-2013", length(1989:2013))),
#                          estimate=c(as.vector(mod1$states),as.vector(mod2$states)),
#                          conf.low=c(as.vector(mod1$states)-1.96*as.vector(mod1$states.se), as.vector(mod2$states)-1.96*as.vector(mod2$states.se)),
#                          conf.high=c(as.vector(mod1$states)+1.96*as.vector(mod1$states.se), as.vector(mod2$states)+1.96*as.vector(mod2$states.se)))
# 
# 
# ggplot(plot.trend, aes(t, estimate)) +
#   geom_line() + 
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) + xlab("") + ylab("Trend") +
#   facet_wrap(~era, scales="free")
# 
# # now...regression or correlation onto SLP fields
# library(ncdf4)
# library(maps)
# library(maptools)
# library(mapdata)
# library(fields)
# library(oce)
# library(chron)
# library(zoo)
# 
# # load slp
# dat <- nc_open("data/NCEP.NCAR.slp.nc")
# 
# x <- ncvar_get(dat, "longitude")
# y <- ncvar_get(dat, "latitude")
# slp <- ncvar_get(dat, "slp", verbose = F)
# dim(slp) # 144 long, 29 lat, 864 months
# 
# # need to reverse latitude for plotting!
# y <- rev(y)
# slp <- slp[,29:1,]
# 
# # Change data into a matrix with months / cells for rows / columns
# slp <- aperm(slp, 3:1)  
# slp <- matrix(slp, nrow=dim(slp)[1], ncol=prod(dim(slp)[2:3]))  
# 
# # plot to confirm that everything is ok
# z <- colMeans(slp, na.rm=T) # mean value for each cell
# z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
# image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", yaxt="n", xaxt="n")
# 
# contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
# map('world2Hires', add=T, lwd=1)
# 
# # will use winter (NDJFM) SLP
# 
# # define the winter period
# # first, extract dates
# raw <- ncvar_get(dat, "time") # seconds since 1-1-1970
# h <- raw/(24*60*60)
# d <- dates(h, origin = c(1,1,1970))
# m <- months(d)
# yr <- as.numeric(as.character(years(d)))
# 
# # make vectors of lat/long and add (with date) as dimnames
# lat <- rep(y, length(x))   
# lon <- rep(x, each = length(y)) 
# 
# dimnames(slp) <- list(as.character(d), paste("N", lat, "E", lon, sep=""))
# 
# # now define winter
# win <- c("Nov", "Dec", "Jan", "Feb", "Mar")
# # set Nov and Dec equal to the year corresponding to Jan!
# win.yr <- ifelse(m %in% c("Nov", "Dec"), yr+1, yr) 
# # and restrict both winter year and slp to the winter months
# win.yr <- win.yr[m %in% win]
# win.slp <- slp[m %in% win,]
# 
# # now get annual winter means for each cell
# 
# ff <- function(x) tapply(x, win.yr, mean)
# 
# win.slp <- apply(win.slp, 2, ff)
# 
# # drop winter 2020, whixh is incomplete
# win.slp <- win.slp[rownames(win.slp) < 2020,]
# 
# ff <- function(x) rollmean(x, 2, align="right", fill=NA)
# 
# win.slp2 <- apply(win.slp, 2, ff)
# 
# # vectors to catch correlation coefficients
# cor.65.88 <- cor.89.13 <- NA
# 
# for(j in 1:ncol(win.slp2)){
#   
#   cor.65.88[j] <- cor(as.vector(mod1$states), 
#                       win.slp2[rownames(win.slp2) %in% 1965:1988,j])  
#   
#   cor.89.13[j] <- cor(as.vector(mod2$states), 
#                       win.slp2[rownames(win.slp2) %in% 1989:2013,j])  
#   
# }
# 
# # plot the resulting maps
# # set the limit for plotting so that it is identical across panels
# 
# zlim <- range(cor.65.88, cor.89.13)
# 
# # set colors 
# new.col <- oce.colorsPalette(64)
# 
# png("figs/Bering climate era dfa-slp correlations.png", 8, 3, units="in", res=300)
# 
# par(mfrow=c(1,2), mar=c(1,1,1,1), las=1)
# 
# # 1965-1988
# z <- cor.65.88
# z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
# 
# image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
# mtext("1965-1988")
# 
# contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
# map('world2Hires', add=T, lwd=1)
# 
# # 1989-2013
# z <- cor.89.13
# z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
# 
# image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
# mtext("1989-2013")
# 
# contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
# map('world2Hires', add=T, lwd=1)
# 
# dev.off()
# 
# ###############
# # and regression
# regr.65.88 <- regr.89.13 <- NA
# 
# # try regression
# 
# for(j in 1:ncol(win.slp2)){
#   
#   regr.65.88[j] <- 
#     summary(lm(as.vector(mod1$states) ~ 
#                  win.slp2[rownames(win.slp2) %in% 1965:1988,j]))$coeff[2,2]
#   
#   regr.89.13[j] <- 
#     summary(lm(as.vector(mod2$states) ~
#                  win.slp2[rownames(win.slp2) %in% 1989:2013,j]))$coeff[2,2] 
#   
# }
# 
# # plot the resulting maps
# # set the limit for plotting so that it is identical across panels
# 
# zlim <- range(regr.65.88, regr.89.13)
# 
# 
# 
# # set colors 
# new.col <- oce.colorsPalette(64)
# 
# png("figs/slp-Bering climate era dfa regression.png", 8, 3, units="in", res=300)
# 
# par(mfrow=c(1,2), mar=c(1,1,1,1), las=1)
# 
# 
# # 1965-1988
# z <- regr.65.88
# z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
# 
# image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
# mtext("1965-1988")
# 
# contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
# map('world2Hires', add=T, lwd=1)
# 
# # 1989-2013
# z <- regr.89.13
# z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
# 
# image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
# mtext("1989-2013")
# 
# contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
# map('world2Hires', add=T, lwd=1)
# 
# 
# dev.off()
# 
# # so..regr maps NOT informative...correlation maps seem to suggest a declining influence of Arctic SLP in the second era!
# 
# # fit a single DFA to the entire TS and fit stan models to PDO/NPGO/AO
# # set up forms of R matrices
# levels.R = c("diagonal and equal",
#              "diagonal and unequal",
#              "equalvarcov",
#              "unconstrained")
# model.data = data.frame()
# 
# # fit models & store results
# for(R in levels.R) {
#   for(m in 1) {  # allowing up to 1 trends
#     dfa.model = list(A="zero", R=R, m=m)
#     kemz = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:2013], model=dfa.model, control=cntl.list, 
#                  form="dfa", z.score=TRUE)
#     model.data = rbind(model.data,
#                        data.frame(R=R,
#                                   m=m,
#                                   logLik=kemz$logLik,
#                                   K=kemz$num.params,
#                                   AICc=kemz$AICc,
#                                   stringsAsFactors=FALSE))
#     assign(paste("kemz", m, R, sep="."), kemz)
#   } # end m loop
# } # end R loop
# 
# # calculate delta-AICc scores, sort in descending order, and compare
# model.data$dAICc <- model.data$AICc-min(model.data$AICc)
# model.data <- model.data %>%
#   arrange(dAICc)
# 
# model.data
# 
# # unconstrained!
# model.list = list(A="zero", m=1, R="unconstrained")
# mod = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1965:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# 
# # fit stan models to AO-trend 1 relationship
# library(tidyverse)
# library(rstan)
# library(ggplot2)
# library(rstanarm)
# library(bayesplot)
# 
# dat <- read.csv("data/climate data.csv")
# 
# stan.data <- data.frame(year=1965:2013,
#                         trend=as.vector(mod$states),
#                         AO.jfm=dat$AO.jfm[dat$year %in% 1965:2013])
# 
# stan.data$era <- ifelse(stan.data$year %in% 1965:1988, "1965-1988", "1989-2013")
# 
# # AO.stan <- stan_glm(trend ~ era + AO.jfm + AO.jfm:era,
#                             # data = stan.data,
#                             # chains = 4, cores = 4, thin = 1, seed=421,
#                             # warmup = 1000, iter = 4000, refresh = 0,
#                             # prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                             # prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                             # prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# scatter <- ggplot(stan.data, aes(AO.jfm, trend, color=era)) +
#   theme_bw() +
#   geom_point() +
#   scale_color_manual(values=cb[2:4]) +
#   geom_smooth(method="lm", se=F) +
#   xlab("AO Index (Jan-Mar)") +
#   theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')
# 
# ggsave("figs/AO vs climate dfa scatter.png", width = 3.5, height = 3, units="in")
# 
# # load pdo/npgo and run the same analysis
# dat2 <- read.csv("data/winter pdo-npgo.csv")
# 
# # plot to check
# check <- dat2 %>%
#   pivot_longer(cols=-year)
# 
# ggplot(check, aes(year, value, color=name)) + 
#   theme_bw() +
#   geom_line()
#   
# stan.data <- left_join(stan.data, dat2)
# 
# # an NPI!
# dat3 <- read.csv("data/winter.NPI.csv")
# 
# stan.data <- left_join(stan.data, dat3)
# 
# # add ALBSA
# dat4 <- read.csv("data/monthly albsa.csv")
# stan.data <- left_join(stan.data, dat4)
# 
# # add mean Arctic SLP
# dat5 <- read.csv("data/mean winter arctic slp.csv")
# stan.data <- left_join(stan.data, dat5)
# 
# 
# plot <- stan.data %>%
#   select(-year) %>%
#   pivot_longer(cols=c(-trend, -era))
# 
# scatter <- ggplot(plot, aes(value, trend, color=era)) +
#   theme_bw() +
#   geom_point() +
#   facet_wrap(~name, scales="free_x") +
#   scale_color_manual(values=cb[2:3]) +
#   geom_smooth(method="lm", se=F) +
#   xlab("Index value") +
#   ylab("Climate trend value") +
#   theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')
# 
# ggsave("figs/ALBSA AO Arctic SLP NPGO NPI PDO vs climate dfa scatter.png", width = 10, height = 8, units="in")
# 
# # scale albsa, arctic.slp, NPI
# stan.data$albsa.djf <- scale(stan.data$albsa.djf)
# stan.data$arctic.slp.ndjfm <- scale(stan.data$arctic.slp.ndjfm)
# stan.data$NPI.ndjfm <- scale(stan.data$NPI.ndjfm)
# 
# stan.this <- stan.data %>%
# select(-year, -AO.jfm, -npgo.ndjfm, -albsa.mam) %>%
#   pivot_longer(cols=c(-trend, -era))
# 
# # stan era-specific regressions
# albsa.djf.stan <- stan_glm(trend ~ era + value + value:era,
#                     data = stan.this[stan.this$name=="albsa.djf",],
#                     chains = 4, cores = 4, thin = 1,
#                     warmup = 1000, iter = 4000, refresh = 0,
#                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# arctic.slp.ndjfm.stan <- stan_glm(trend ~ era + value + value:era,
#                                   data = stan.this[stan.this$name=="arctic.slp.ndjfm",],
#                            chains = 4, cores = 4, thin = 1,
#                            warmup = 1000, iter = 4000, refresh = 0,
#                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# NPI.ndjfm.stan <- stan_glm(trend ~ era + value + value:era,
#                            data = stan.this[stan.this$name=="NPI.ndjfm",],
#                                   chains = 4, cores = 4, thin = 1,
#                                   warmup = 1000, iter = 4000, refresh = 0,
#                                   prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                                   prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                                   prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# pdo.ndjfm.stan <- stan_glm(trend ~ era + value + value:era,
#                            data = stan.this[stan.this$name=="pdo.ndjfm",],
#                            chains = 4, cores = 4, thin = 1,
#                            warmup = 1000, iter = 4000, refresh = 0,
#                            prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                            prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                            prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# ##############
# # and plot!
# 
# lst <- list(albsa.djf.stan, arctic.slp.ndjfm.stan, NPI.ndjfm.stan, pdo.ndjfm.stan)
# 
# 
# # extract intercepts
# lst.int <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2013"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_indv_arm <- plyr::rbind.fill(lst.int)
# 
# mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")
# 
# ## extract slopes
# lst.slope <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("value", "era1989-2013:value"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_slope <- plyr::rbind.fill(lst.slope)
# mdf_slope <- reshape2::melt(coef_slope, id.vars = "key")
# 
# 
# # plot intercepts
# int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1965-1988", "1989-2013")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Intercept (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(int)
# 
# ggsave("figs/era intercepts - large-scale modes on Bering climate DFA.png", width=10, height=8, units="in")
# 
# # plot slopes
# slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Slope (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(slope)
# 
# ggsave("figs/era slopes - climate variables on AO.png", width=10, height=8, units='in')
# 
# ######################
# # hmmmm....rolling regressions on PDO-trend relationship?
# 
# roll.dat <- stan.data %>%
#   select(year, trend, pdo.ndjfm)
# 
# roll.dat$slope <- roll.dat$intercept <- NA
# 
# # 25-yr windows
# for(i in 1977:2001){
#   # i <- 1977
#   temp <- roll.dat %>%
#     filter(year %in% (i-12):(i+12))
#   
#   mod <- lm(temp$trend ~ temp$pdo.ndjfm)
#   roll.dat$intercept[roll.dat$year==i] <- mod$coefficients[1]
#   roll.dat$slope[roll.dat$year==i] <- mod$coefficients[2]  
# }
# 
# plot <- roll.dat %>%
#   select(year, intercept, slope) %>%
#   pivot_longer(cols=-year)
# 
# plot <- na.omit(plot)
#   
# ggplot(plot, aes(year, value, color=name)) +
#   theme_bw() +
#   geom_line() +
#   geom_vline(xintercept = 1988.5, lty=2)
# 
# # and AICc!
# library(MuMIn)
# 
# roll.dat <- stan.data %>%
#   select(-era) %>%
#   pivot_longer(cols=c(-year, -trend))
#   
# vars <- unique(roll.dat$name)
# AICc.out <- data.frame()
# 
# # 25-yr windows
# for(j in 1:length(vars)){
#   # j <- 1
#   
#   temp <- roll.dat %>%
#     filter(name == vars[j])
#   
#  for(i in 1977:2001){
#      # i <- 1977
# 
#   temp$era <- ifelse(temp$year <= i, "era1", "era2")
#   
#   mod <- lm(temp$trend ~ temp$value * temp$era)
#   AICc.out <- rbind(AICc.out,
#                     data.frame(variable=vars[j],
#                                year=i,
#                                AICc=AICc(mod)))
#   
# }
# }
# 
# ggplot(AICc.out, aes(year, AICc)) +
#   theme_bw() +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y")
# 
# # non-interaction model...
# AICc.out <- data.frame()
# 
# # 25-yr windows
# for(j in 1:length(vars)){
#   # j <- 1
#   
#   temp <- roll.dat %>%
#     filter(name == vars[j])
#   
#   for(i in 1977:2001){
#     # i <- 1977
#     
#     temp$era <- ifelse(temp$year <= i, "era1", "era2")
#     
#     mod <- lm(temp$trend ~ temp$value + temp$era)
#     AICc.out <- rbind(AICc.out,
#                       data.frame(variable=vars[j],
#                                  year=i,
#                                  AICc=AICc(mod)))
#     
#   }
# }
# 
# ggplot(AICc.out, aes(year, AICc)) +
#   theme_bw() +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y")
# 
# # now! just fit a single model to 25-year periods..
# AICc.out <- data.frame()
# 
# # 25-yr windows
# for(j in 1:length(vars)){
#   # j <- 1
#   
#   temp <- roll.dat %>%
#     filter(name == vars[j])
#   
#   for(i in 1977:2001){
#     # i <- 1977
#     temp2 <- temp %>%
#       filter(year %in% (i-12):(i+12))
#     mod <- lm(scale(temp2$trend) ~ temp2$value)
#     
#     AICc.out <- rbind(AICc.out,
#                       data.frame(variable=vars[j],
#                                  year=i,
#                                  AICc=AICc(mod)))
#     
#   }
# }
# 
# ggplot(AICc.out, aes(year, AICc)) +
#   theme_bw() +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y")
# #################################
# # add more years to the rolling AICc analysis
# 
# # find best model for 1950-2013
# model.data = data.frame()
# 
# # fit models & store results
# for(R in levels.R) {
#   for(m in 1) {  # allowing up to 1 trends
#     dfa.model = list(A="zero", R=R, m=m)
#     kemz = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1950:2013], model=dfa.model, control=cntl.list, 
#                  form="dfa", z.score=TRUE)
#     model.data = rbind(model.data,
#                        data.frame(R=R,
#                                   m=m,
#                                   logLik=kemz$logLik,
#                                   K=kemz$num.params,
#                                   AICc=kemz$AICc,
#                                   stringsAsFactors=FALSE))
#     assign(paste("kemz", m, R, sep="."), kemz)
#   } # end m loop
# } # end R loop
# 
# # calculate delta-AICc scores, sort in descending order, and compare
# model.data$dAICc <- model.data$AICc-min(model.data$AICc)
# model.data <- model.data %>%
#   arrange(dAICc)
# 
# model.data
# 
# # unconstrained!
# model.list = list(A="zero", m=1, R="unconstrained")
# mod = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1951:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# 
# stan.data <- data.frame(year=1951:2013,
#                         trend=as.vector(mod$states),
#                         AO.jfm=dat$AO.jfm[dat$year %in% 1951:2013])
# # load pdo/npgo and run the same analysis
# dat2 <- read.csv("data/winter pdo-npgo.csv")
# 
# # plot to check
# check <- dat2 %>%
#   pivot_longer(cols=-year)
# 
# ggplot(check, aes(year, value, color=name)) + 
#   theme_bw() +
#   geom_line()
# 
# stan.data <- left_join(stan.data, dat2)
# 
# # an NPI!
# dat3 <- read.csv("data/winter.NPI.csv")
# 
# stan.data <- left_join(stan.data, dat3)
# 
# # add ALBSA
# dat4 <- read.csv("data/monthly albsa.csv")
# stan.data <- left_join(stan.data, dat4)
# 
# # add mean Arctic SLP
# dat5 <- read.csv("data/mean winter arctic slp.csv")
# stan.data <- left_join(stan.data, dat5)
# 
# 
# roll.dat <- stan.data %>%
#   pivot_longer(cols=c(-year, -trend))
# 
# vars <- unique(roll.dat$name)
# AICc.out <- data.frame()
# 
# # 25-yr windows
# for(j in 1:length(vars)){
#   # j <- 1
#   
#   temp <- roll.dat %>%
#     filter(name == vars[j])
#   
#   for(i in 1963:2001){
#     # i <- 1977
#     
#     temp$era <- ifelse(temp$year <= i, "era1", "era2")
#     
#     mod <- lm(temp$trend ~ temp$value * temp$era)
#     AICc.out <- rbind(AICc.out,
#                       data.frame(variable=vars[j],
#                                  year=i,
#                                  AICc=AICc(mod)))
#     
#   }
# }
# 
# ggplot(AICc.out, aes(year, AICc)) +
#   theme_bw() +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y")
# 
# ggsave("figs/large-scale modes and bering climate dfa AICc.png", width=8, height=7, units='in')

# #################################
# # for comparison, look at the same analysis for GOA!!
# dat <- read.csv("data/GOA data/GOA.climate.csv", row.names=1)
# 
# unique(dat$key)
# # subset
# keep <- c("NDJ.grad", "FMA.FW", "FMA.WS", "MJJ.UW", "FMA.ao", "FMA.SSH")
# 
# dfa.dat <- dat %>%
#   filter(key %in% keep) %>% 
#   pivot_wider(names_from="key", values_from = "value")
# 
# years <- dfa.dat$year
# 
# dfa.dat <- dfa.dat %>%
#   select(-year)
# 
# dfa.dat <- as.matrix(t(dfa.dat))
# colnames(dfa.dat) <- years
# View(dfa.dat)
# 
# # find best error structure for 1-trend model
# 
# # changing convergence criterion to ensure convergence
# cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
# 
# # set up forms of R matrices
# levels.R = c("diagonal and equal",
#              "diagonal and unequal",
#              "equalvarcov",
#              "unconstrained")
# model.data = data.frame()
# 
# # fit models & store results
# for(R in levels.R) {
#   for(m in 1) {  # allowing up to 1 trends
#     dfa.model = list(A="zero", R=R, m=m)
#     kemz = MARSS(dfa.dat, model=dfa.model, control=cntl.list, 
#                  form="dfa", z.score=TRUE)
#     model.data = rbind(model.data,
#                        data.frame(R=R,
#                                   m=m,
#                                   logLik=kemz$logLik,
#                                   K=kemz$num.params,
#                                   AICc=kemz$AICc,
#                                   stringsAsFactors=FALSE))
#     assign(paste("kemz", m, R, sep="."), kemz)
#   } # end m loop
# } # end R loop
# 
# # calculate delta-AICc scores, sort in descending order, and compare
# model.data$dAICc <- model.data$AICc-min(model.data$AICc)
# model.data <- model.data %>%
#   arrange(dAICc)
# 
# model.data
# 
# # unconstrained!
# model.list = list(A="zero", m=1, R="unconstrained")
# mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# 
# stan.data <- data.frame(year=1950:2012,
#                         trend=as.vector(mod$states))
# # load pdo/npgo and run the same analysis
# dat2 <- read.csv("data/winter pdo-npgo.csv")
# 
# # plot to check
# check <- dat2 %>%
#   pivot_longer(cols=-year)
# 
# ggplot(check, aes(year, value, color=name)) + 
#   theme_bw() +
#   geom_line()
# 
# stan.data <- left_join(stan.data, dat2)
# 
# # an NPI!
# dat3 <- read.csv("data/winter.NPI.csv")
# 
# stan.data <- left_join(stan.data, dat3)
# 
# 
# roll.dat <- stan.data %>%
#   pivot_longer(cols=c(-year, -trend))
# 
# vars <- unique(roll.dat$name)
# AICc.out <- data.frame()
# 
# # 25-yr windows
# for(j in 1:length(vars)){
#   # j <- 1
#   
#   temp <- roll.dat %>%
#     filter(name == vars[j])
#   
#   for(i in 1962:2000){
#     # i <- 1977
#     
#     temp$era <- ifelse(temp$year <= i, "era1", "era2")
#     
#     mod <- lm(temp$trend ~ temp$value * temp$era)
#     AICc.out <- rbind(AICc.out,
#                       data.frame(variable=vars[j],
#                                  year=i,
#                                  AICc=AICc(mod)))
#     
#   }
# }
# 
# ggplot(AICc.out, aes(year, AICc)) +
#   theme_bw() +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y")
# 
# ggsave("figs/large-scale modes and GOA climate dfa AICc.png", width=8, height=7, units='in')

# still quite confusing!

# try the simplest approach - 25 yr rolling correlations between PDO and individual
# time series in each ecosystem!

# for GOA, restrict to the time series known to show changing associations with PDO
# dat <- read.csv("data/GOA data/GOA.climate.csv", row.names=1)
# keep <- c("NDJ.grad", "FMA.FW", "FMA.WS", "FMA.SSH", "Papa")
# 
# dfa.dat <- dat %>%
#   filter(key %in% keep) %>% 
#   pivot_wider(names_from="key", values_from = "value")
# 
# # add PDO
# dat2 <- read.csv("data/winter pdo-npgo.csv")
# 
# dat2 <- dat2 %>%
#   select(-npgo.ndjfm)
# 
# dfa.dat <- left_join(dfa.dat, dat2)
# 
# # add SST
# dat3 <- read.csv("data/GOA data/goa.winter.ao.csv")
# 
#  
# dfa.dat <- left_join(dfa.dat, dat3)
# 
# dfa.dat <- dfa.dat %>%
#   pivot_longer(cols=c(-year, -pdo.ndjfm, -ndjfm.ao))
# 
# # get rolling 25-yr correlations
# 
# goa.cor <- data.frame()
# vars <- unique(dfa.dat$name)
# 
# for(j in 1:length(vars)){
#   # j <- 1
#   temp <- dfa.dat %>%
#     filter(name==vars[j])
# 
# for(i in 1963:2000){
#   # i <- 1990
#   goa.cor <- rbind(goa.cor,
#                    data.frame(year=i,
#                               var=vars[j],
#                               mode="pdo.ndjfm",
#                               cor=cor(temp$pdo.ndjfm[temp$year %in% (i-12):(i+12)],
#                                       temp$value[temp$year %in% (i-12):(i+12)])))
#   
#   
# }}
# 
# # and sst
# for(j in 1:length(vars)){
#   # j <- 1
#   temp <- dfa.dat %>%
#     filter(name==vars[j])
#   
#   for(i in 1963:2000){
#     # i <- 1990
#     goa.cor <- rbind(goa.cor,
#                      data.frame(year=i,
#                                 var=vars[j],
#                                 mode="ndjfm.ao",
#                                 cor=cor(temp$ndjfm.ao[temp$year %in% (i-12):(i+12)],
#                                         temp$value[temp$year %in% (i-12):(i+12)])))
#     
#     
#   }}
# 
# 
# 
# 
# # now, restrict to correlation time series with 
# # absolute values >= 0.5 for at least 1 25-yr window!
# 
# # ff <- function(x) max(abs(x), na.rm=T)>=0.5
# 
# goa.cor$var.mode <- paste(goa.cor$var, goa.cor$mode, sep=".")
# 
# # goa.cor <- plyr::ddply(goa.cor, "var.mode", mutate, keep = ff(cor))
# 
# 
# # rename with plot-friendly names!
# goa.cor$mode <- ifelse(goa.cor$mode=='pdo.ndjfm', "PDO", "SST")
# 
# 
# goa.cor$var <- ifelse(goa.cor$var=="NDJ.grad", "SLP gradient",
#                       ifelse(goa.cor$var=="FMA.FW", "Freshwater discharge",
#                              ifelse(goa.cor$var=="FMA.WS", "Wind stress",
#                                     ifelse(goa.cor$var=="Papa", "Papa advection", "SSH"))))
# 
# goa.plot <- ggplot(goa.cor, aes(year, cor, color=var)) +
#   theme_bw() +
#   geom_line() +
#   facet_wrap(~mode, scales="free_y") +
#   xlab("Center of 25-year window") +
#   ylab("Pearson's correlation") +
#   geom_vline(xintercept = 1988.5, lty=2) +
#   geom_hline(yintercept = 0, color="gray") +
#   scale_color_manual(values=cb[c(2,3,4,6,7)]) +
#   theme(legend.title = element_blank())
# 
# ggsave("figs/rolling correlations - GOA climate vars and PDO-SST.png", width=8, height=4, units='in')

# ok - that's a good example to compare with...now how about the Bering TS?
# load climate data 
dat <- read.csv("data/climate data.csv")

names(dat)

# subset
keep <- c("year", "m4.march.ice", "m5.march.ice", "NW.wind.May.Sep",
          "NW.wind.Oct.Apr", "SE.wind.May.Sep", "SE.wind.Oct.Apr", "south.ao.amj",
          "south.ao.ndjfm", "south.wind.stress.amj")

dfa.dat <- dat %>%
  select(keep) %>%
  pivot_longer(cols=-year)


dat2 <- read.csv("data/winter pdo-npgo.csv")


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
                                mode="pdo.ndjfm",
                                cor=cor(temp$pdo.ndjfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
  }}

# now npgo
for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                var=vars[j],
                                mode="npgo.ndjfm",
                                cor=cor(temp$npgo.ndjfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


# add other large-scale modes!

dat3 <- read.csv("data/climate data.csv")
dat3 <- dat3 %>%
  select(year, AO.jfm)
dfa.dat <- left_join(dfa.dat, dat3)

# # add ALBSA
# dat4 <- read.csv("data/monthly albsa.csv")
# dfa.dat <- left_join(dfa.dat, dat4)
# 
# # add mean Arctic SLP
# dat5 <- read.csv("data/mean winter arctic slp.csv")
# dfa.dat <- left_join(dfa.dat, dat5)

# now rolling AO corrs
# get rolling 25-yr correlations

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                var=vars[j],
                                mode="AO.jfm",
                                cor=cor(temp$AO.jfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


############
# now rolling spring ALBSA corrs
# get rolling 25-yr correlations

# for(j in 1:length(vars)){
#   # j <- 1
#   temp <- dfa.dat %>%
#     filter(name==vars[j])
#   
#   for(i in 1963:2001){
#     # i <- 1990
#     ebs.cor <- rbind(ebs.cor,
#                      data.frame(year=i,
#                                 var=vars[j],
#                                 mode="albsa.mam",
#                                 cor=cor(temp$albsa.mam[temp$year %in% (i-12):(i+12)],
#                                         temp$value[temp$year %in% (i-12):(i+12)])))
#     
#     
#   }}
# 
# 
# ############
# # now rolling winter ALBSA corrs
# # get rolling 25-yr correlations
# 
# for(j in 1:length(vars)){
#   # j <- 1
#   temp <- dfa.dat %>%
#     filter(name==vars[j])
#   
#   for(i in 1963:2001){
#     # i <- 1990
#     ebs.cor <- rbind(ebs.cor,
#                      data.frame(year=i,
#                                 var=vars[j],
#                                 mode="albsa.djf",
#                                 cor=cor(temp$albsa.djf[temp$year %in% (i-12):(i+12)],
#                                         temp$value[temp$year %in% (i-12):(i+12)])))
#     
#     
#   }}
# 

############
# # now Arctic SLP
# # get rolling 25-yr correlations
## Removing as these are generally weak!!
# 
# for(j in 1:length(vars)){
#   # j <- 1
#   temp <- dfa.dat %>%
#     filter(name==vars[j])
#   
#   for(i in 1963:2001){
#     # i <- 1990
#     ebs.cor <- rbind(ebs.cor,
#                      data.frame(year=i,
#                                 var=vars[j],
#                                 mode="Arctic.slp.ndjfm",
#                                 cor=cor(temp$arctic.slp.ndjfm[temp$year %in% (i-12):(i+12)],
#                                         temp$value[temp$year %in% (i-12):(i+12)])))
#     
#     
#   }}

ebs.cor$var.mode <- paste(ebs.cor$var, ebs.cor$mode, sep=".")

ebs.cor <- plyr::ddply(ebs.cor, "var.mode", mutate, keep = ff(cor))

# drop sst for now
drop <- grep("sst", ebs.cor$var)

plot.ebs <- ebs.cor[-drop,]

# and get plot-friendly names!
plot.ebs$mode <- ifelse(plot.ebs$mode=="AO.jfm", "AO",
                       ifelse(plot.ebs$mode=='pdo.ndjfm', "PDO", "NPGO"))

plot.ebs$mode.order <- ifelse(plot.ebs$mode=="PDO", 1,
                             ifelse(plot.ebs$mode=="NPGO", 2, 3))

plot.ebs$mode <- reorder(plot.ebs$mode, plot.ebs$mode.order)

ebs.plot <- ggplot(filter(plot.ebs, keep==TRUE), aes(year, cor, color=var)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~mode, scales="free_y") +
  xlab("Center of 25-year window") +
  ylab("Pearson's correlation") +
  geom_vline(xintercept = 1988.5, lty=2) +
  geom_hline(yintercept = 0, color="gray") +
  scale_color_manual(values=cb[c(2,3,4,6,7)]) +
  theme(legend.title = element_blank())


png("figs/GOA and EBS modes and climate rolling corr.png", width=9, height=5, units='in', res=300)
ggpubr::ggarrange(goa.plot,  ebs.plot, labels = c("a) GOA", "b) EBS"), nrow=2, hjust=0, vjust=1.2, align='v')
dev.off()

#######
# now plot mode correlations with sst!
# first ebs
keep <- grep("sst.ndjfm", ebs.cor$var)

plot.ebs <- ebs.cor[keep,]

# and get plot-friendly names!
plot.ebs$mode <- ifelse(plot.ebs$mode=="AO.jfm", "AO",
                        ifelse(plot.ebs$mode=='pdo.ndjfm', "PDO", "NPGO"))

plot.ebs$mode.order <- ifelse(plot.ebs$mode=="PDO", 1,
                              ifelse(plot.ebs$mode=="NPGO", 2, 3))

plot.ebs$mode <- reorder(plot.ebs$mode, plot.ebs$mode.order)

ebs.ao <- ggplot(filter(plot.ebs, keep==TRUE), aes(year, cor, color=mode)) +
  theme_bw() +
  geom_line() +
  xlab("Center of 25-year window") +
  ylab("Pearson's correlation") +
  geom_vline(xintercept = 1988.5, lty=2) +
  geom_hline(yintercept = 0, color="gray") +
  scale_color_manual(values=cb[c(2,3,4,6,7)]) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  ggtitle("b) EBS SST")

# now GOA sst cor!
dat <- read.csv("data/GOA data/goa.winter.ao.csv", row.names=1)
dfa.dat <- dat %>%
  mutate(year=as.numeric(row.names(dat))) %>%
  filter(year %in% 1951:2013)
  

dat2 <- read.csv("data/winter pdo-npgo.csv")


dfa.dat <- left_join(dfa.dat, dat2)

dat3 <- read.csv("data/climate data.csv")
dat3 <- dat3 %>%
  select(year, AO.jfm)

dfa.dat <- left_join(dfa.dat, dat3)

dfa.dat <- dfa.dat %>%
  pivot_longer(cols=c(-ndjfm.ao, -year))

# get rolling 25-yr correlations

goa.cor <- data.frame()
modes <- unique(dfa.dat$name)

for(j in 1:length(modes)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==modes[j])
  
  for(i in 1963:2001){
    # i <- 1990
    goa.cor <- rbind(goa.cor,
                     data.frame(year=i,
                                mode=modes[j],
                                cor=cor(temp$ndjfm.ao[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
  }}


# now, restrict to correlation time series with 
# absolute values >= 0.5 for at least 1 25-yr window!

ff <- function(x) max(abs(x), na.rm=T)>=0.5

goa.cor <- plyr::ddply(goa.cor, "mode", mutate, keep = ff(cor))


# rename with plot-friendly names!
goa.cor$mode <- ifelse(goa.cor$mode=="AO.jfm", "AO",
                       ifelse(goa.cor$mode=='pdo.ndjfm', "PDO", "NPGO"))

goa.cor$mode.order <- ifelse(goa.cor$mode=="PDO", 1,
                             ifelse(goa.cor$mode=="NPGO", 2, 3))

goa.cor$mode <- reorder(goa.cor$mode, goa.cor$mode.order)


goa.ao <- ggplot(filter(goa.cor, keep==TRUE), aes(year, cor, color=mode)) +
  theme_bw() +
  geom_line() +
  xlab("Center of 25-year window") +
  ylab("Pearson's correlation") +
  geom_vline(xintercept = 1988.5, lty=2) +
  geom_hline(yintercept = 0, color="gray") +
  scale_color_manual(values=cb[c(2,3,4,6,7)]) +
  theme(legend.title = element_blank(), legend.position = 'top') + 
  ggtitle("a) GOA SST")

png("figs/GOA and EBS sst - modes rolling corr.png", width=6, height=3, units='in', res=300)
ggpubr::ggarrange(goa.ao,  ebs.ao, nrow=1)
dev.off()

###################################
# use Bayesian regression to compare predictive skill of SST for other variables pre/post 1988/89
# fit stan models to AO-trend 1 relationship
library(tidyverse)
library(rstan)
library(ggplot2)
library(rstanarm)
library(bayesplot)

# # for GOA, restrict to the time series known to show changing associations with PDO
# dat <- read.csv("data/GOA data/GOA.climate.csv", row.names=1)
# keep <- c("NDJ.grad", "FMA.FW", "FMA.WS", "FMA.SSH", "Papa")
# 
# stan.data <- dat %>%
#   filter(key %in% keep) %>% 
#   pivot_wider(names_from="key", values_from = "value")
# 
# dat <- read.csv("data/GOA data/goa.winter.ao.csv", row.names=1)
# dat <- dat %>%
#   mutate(year=as.numeric(row.names(dat)))
# 
# stan.data <- left_join(stan.data, dat) %>%
#   filter(year <= 2013) %>%
#   pivot_longer(cols=c(-year, -ndjfm.ao))
# 
# stan.data$era <- ifelse(stan.data$year %in% 1950:1988, "1950-1988", "1989-2012")
# 
# # AO.stan <- stan_glm(trend ~ era + AO.jfm + AO.jfm:era,
#                             # data = stan.data,
#                             # chains = 4, cores = 4, thin = 1, seed=421,
#                             # warmup = 1000, iter = 4000, refresh = 0,
#                             # prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                             # prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                             # prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# scatter <- ggplot(stan.data, aes(ndjfm.ao, value, color=era)) +
#   theme_bw() +
#   geom_point() +
#   scale_color_manual(values=cb[2:4]) +
#   geom_smooth(method="lm", se=F) +
#   xlab("SST (NDJFM)") +
#   facet_wrap(~name, scales="free_y") +
#   theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')
# 
# ggsave("figs/GOA sst vs climate scatter.png", width = 7, height = 4, units="in")
# 
# # scale values!
# stan.data <- plyr::ddply(stan.data, "name", mutate, scale.value = scale(value))
# 
# ggplot(stan.data, aes(scale.value)) +
#   geom_histogram() +
#   facet_wrap(~name)
# 
# # stan era-specific regressions
# FMA.FW <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                     data = stan.data[stan.data$name=="FMA.FW",],
#                     chains = 4, cores = 4, thin = 1,
#                     warmup = 1000, iter = 4000, refresh = 0,
#                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# FMA.SSH <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                    data = stan.data[stan.data$name=="FMA.SSH",],
#                    chains = 4, cores = 4, thin = 1,
#                    warmup = 1000, iter = 4000, refresh = 0,
#                    prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# FMA.WS <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                    data = stan.data[stan.data$name=="FMA.WS",],
#                    chains = 4, cores = 4, thin = 1,
#                    warmup = 1000, iter = 4000, refresh = 0,
#                    prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# NDJ.grad <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                     data = stan.data[stan.data$name=="NDJ.grad",],
#                     chains = 4, cores = 4, thin = 1,
#                     warmup = 1000, iter = 4000, refresh = 0,
#                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# Papa <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                      data = stan.data[stan.data$name=="Papa",],
#                      chains = 4, cores = 4, thin = 1,
#                      warmup = 1000, iter = 4000, refresh = 0,
#                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# ##############
# # and plot!
# 
# lst <- list(FMA.FW, FMA.SSH, FMA.WS, NDJ.grad, Papa)
# 
# 
# # extract intercepts
# lst.int <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2013"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_indv_arm <- plyr::rbind.fill(lst.int)
# 
# mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")
# 
# ## extract slopes
# lst.slope <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("ndjfm.ao", "era1989-2013:ndjfm.ao"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_slope <- plyr::rbind.fill(lst.slope)
# mdf_slope <- reshape2::melt(coef_slope, id.vars = "key")
# 
# 
# # plot intercepts
# int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2013")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Intercept (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(int)
# 
# ggsave("figs/era intercepts - winter SST vs GOA climate.png", width=10, height=8, units="in")
# 
# # plot slopes
# slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Slope (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(slope)
# 
# ggsave("figs/era slopes - winter SST vs GOA climate.png", width=10, height=8, units='in')
# 
# 
# # and loop through different candidate thresholds with LOOIC model selection
# 
# # try looping through candidate threshold years and using LOOIC to find most-supported threshold
# thresholds <- 1964:1997 # minimum = 15 years
# 
# looic <- data.frame()
# temp <- na.omit(stan.data)
# 
# for(i in 1:length(thresholds)){
#   
#   temp$thresh.era <- ifelse(temp$year <= thresholds[i], 1, 2)
#   
#   
#   mod <- stan_glm(value ~ thresh.era + ndjfm.ao + ndjfm.ao:thresh.era,
#                   data = filter(temp, name=="NDJ.grad"),
#                   chains = 4, cores = 4, thin = 1,
#                   warmup = 1000, iter = 4000, refresh = 0,
#                   prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
#   
#   support <- loo(mod)
#   
#   looic <- rbind(looic,
#                  data.frame(threshold=thresholds[i],
#                             wind.looic=support$estimates[3,1],
#                             wind.se=support$estimates[3,2]))
# }
# 
# looic$conf.high <- looic$wind.looic+1.96*looic$wind.se
# looic$conf.low <- looic$wind.looic-1.96*looic$wind.se
# 
# ggplot(looic, aes(threshold, wind.looic)) +
#   theme_bw() +
#   geom_line(color=cb[2]) +
#   geom_ribbon(aes(x=threshold, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1, fill=cb[2])
# 
# ggsave("figs/LOOIC threshold GOA SLP gradient vs SST.png", width=6, height=4, units='in')
# 
# ########
# 
# #########################################
# # dfa trend for shared variability??
# dfa.dat <- stan.data %>%
#   select(year, name, value) %>%
#   pivot_wider(names_from = name, values_from = value)
# 
# dfa.dat <- as.matrix(t(select(dfa.dat, -year)))
# colnames(dfa.dat) <- 1950:2012
# 
# 
# # find best error structure for 1-trend model
# 
# # changing convergence criterion to ensure convergence
# cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
# 
# # set up forms of R matrices
# levels.R = c("diagonal and equal",
#              "diagonal and unequal",
#              "equalvarcov",
#              "unconstrained")
# model.data = data.frame()
# 
# # having trouble with convergence - not surprisingly! Upping maxit
# cntl.list = list(minit=200, maxit=40000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
# 
# # fit models & store results
# for(R in levels.R) {
#   for(m in 1) {  # allowing up to 1 trends
#     dfa.model = list(A="zero", R=R, m=m)
#     kemz = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1989:2012], model=dfa.model, control=cntl.list,
#                  form="dfa", z.score=TRUE)
#     model.data = rbind(model.data,
#                        data.frame(R=R,
#                                   m=m,
#                                   logLik=kemz$logLik,
#                                   K=kemz$num.params,
#                                   AICc=kemz$AICc,
#                                   stringsAsFactors=FALSE))
#     assign(paste("kemz", m, R, sep="."), kemz)
#   } # end m loop
# } # end R loop
# 
# # calculate delta-AICc scores, sort in descending order, and compare
# model.data$dAICc <- model.data$AICc-min(model.data$AICc)
# model.data <- model.data %>%
#   arrange(dAICc)
# 
# era2.models <- model.data
# 
# era1.models; era2.models
# # equalvarcov is best for each...but for era2 only diagonal and unequal / unconstrained show non-0 loadings!
# 
# # fit models to each era
# model.list = list(A="zero", m=1, R="equalvarcov")
# mod1 = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1950:1988], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# model.list = list(A="zero", m=1, R="diagonal and unequal")
# mod2 = MARSS(dfa.dat[,colnames(dfa.dat) %in% 1989:2012], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# 
# # get CI and plot loadings...
# mod1.CI <- MARSSparamCIs(mod1)
# 
# mod2.CI <- MARSSparamCIs(mod2)
# 
# plot.CI <- data.frame(names=rep(rownames(dfa.dat),2),
#                       era=rep(c("1950-1988", "1989-2012"), each=nrow(dfa.dat)),
#                       mean=c(mod1.CI$par$Z, mod2.CI$par$Z),
#                       upCI=c(mod1.CI$par.upCI$Z,mod2.CI$par.upCI$Z),
#                       lowCI=c(mod1.CI$par.lowCI$Z,mod2.CI$par.lowCI$Z))
# 
# plot.CI$names <- reorder(plot.CI$names, rep(mod1.CI$par$Z, 2))
# dodge <- position_dodge(width=0.9)
# 
# # rename variables for plotting!
# plot.CI$names <- ifelse(plot.CI$names=="NDJ.grad", "SLP gradient",
#                       ifelse(plot.CI$names=="FMA.FW", "Freshwater discharge",
#                              ifelse(plot.CI$names=="FMA.WS", "Wind stress",
#                                     ifelse(plot.CI$names=="Papa", "Papa advection", "SSH"))))
# 
# ggplot(plot.CI, aes(x=names, y=mean, fill=era)) +
#   geom_bar(position=dodge, stat="identity") +
#   geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
#   ylab("Loading") +
#   xlab("") +
#   theme_bw() +
#   theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
#   geom_hline(yintercept = 0) +
#   scale_fill_manual(values=c(cb[2], cb[3]))
# 
# ggsave("figs/non-sst GOA era-specific climate dfa loadings.png", width=4, height=6, units='in')
# 
# 
# # now make new stan data objects for era regression of sst-dfa relationships
# dat <- read.csv("data/GOA data/goa.winter.ao.csv", row.names=1)
# dat <- dat %>%
#   mutate(year=as.numeric(row.names(dat))) %>%
#   filter(year %in% 1950:2012)
# 
# 
# stan.new <- data.frame(year=1950:2012,
#                        ndjfm.ao=dat$ndjfm.ao,
#                        name="dfa.trend",
#                        value=c(mod1$states, mod2$states))
# stan.new$era <- ifelse(stan.new$year <= 1988, "1950-1988", "1989-2012")
# 
# # and run regression
# dfa.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
#                  data = stan.new,
#                  chains = 4, cores = 4, thin = 1,
#                  warmup = 1000, iter = 4000, refresh = 0,
#                  prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                  prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                  prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# ##############
# # and plot!
# 
# lst <- list(dfa.stan)
# 
# 
# # extract intercepts
# lst.int <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2012"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_indv_arm <- plyr::rbind.fill(lst.int)
# 
# mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")
# 
# ## extract slopes
# lst.slope <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("ndjfm.ao", "era1989-2012:ndjfm.ao"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_slope <- plyr::rbind.fill(lst.slope)
# mdf_slope <- reshape2::melt(coef_slope, id.vars = "key")
# 
# 
# # plot intercepts
# int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3]), labels=c("1950-1988", "1989-2013")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Intercept (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(int)
# 
# ggsave("figs/era intercepts - winter SST vs GOA climate.png", width=10, height=8, units="in")
# 
# # plot slopes
# slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2012")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Slope (scaled anomaly)",
#        y = "Posterior density")
# print(slope)
# 
# ggsave("figs/era slopes - winter SST vs GOA climate dfa trend.png", width=3, height=3, units='in')
# 
# # era probabilities of being > 0
# 
# probs <- mdf_slope %>%
#   group_by(variable) %>%
#   summarize(prob.greater=(sum(value>0)/length(value))) # 98.5% in era1, 57.0% in era2
# 
# 
# # so here's the problem - the dfa model in era 2 is mostly tracking ssh, which remains correlated with sst - not apples to apples!
# 
# # try a dfa model for the whole TS
# model.data = data.frame()
# 
# # fit models & store results
# for(R in levels.R) {
#   for(m in 1) {  # allowing up to 1 trends
#     dfa.model = list(A="zero", R=R, m=m)
#     kemz = MARSS(dfa.dat, model=dfa.model, control=cntl.list,
#                  form="dfa", z.score=TRUE)
#     model.data = rbind(model.data,
#                        data.frame(R=R,
#                                   m=m,
#                                   logLik=kemz$logLik,
#                                   K=kemz$num.params,
#                                   AICc=kemz$AICc,
#                                   stringsAsFactors=FALSE))
#     assign(paste("kemz", m, R, sep="."), kemz)
#   } # end m loop
# } # end R loop
# 
# # calculate delta-AICc scores, sort in descending order, and compare
# model.data$dAICc <- model.data$AICc-min(model.data$AICc)
# model.data <- model.data %>%
#   arrange(dAICc)
# 
# model.data
# 
# # equalvarcov is best
# 
# # fit model
# model.list = list(A="zero", m=1, R="equalvarcov")
# mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# 
# # get CI and plot loadings...
# mod.CI <- MARSSparamCIs(mod)
# 
# plot.CI <- data.frame(names=rownames(dfa.dat),
#                       mean=mod.CI$par$Z,
#                       upCI=mod.CI$par.upCI$Z,
#                       lowCI=mod.CI$par.lowCI$Z)
# 
# dodge <- position_dodge(width=0.9)
# 
# # rename variables for plotting!
# plot.CI$names <- ifelse(plot.CI$names=="NDJ.grad", "SLP gradient",
#                         ifelse(plot.CI$names=="FMA.FW", "Freshwater discharge",
#                                ifelse(plot.CI$names=="FMA.WS", "Wind stress",
#                                       ifelse(plot.CI$names=="Papa", "Papa advection", "SSH"))))
# 
# plot.CI$names <- reorder(plot.CI$names, mod.CI$par$Z)
# 
# ggplot(plot.CI, aes(x=names, y=mean)) +
#   geom_bar(position=dodge, stat="identity", fill=cb[2]) +
#   geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
#   ylab("Loading") +
#   xlab("") +
#   theme_bw() +
#   theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
#   geom_hline(yintercept = 0)
# 
# ggsave("figs/non-sst GOA full TS climate dfa loadings.png", width=2.5, height=4, units='in')
# 
# 
# # now make new stan data objects for era regression of sst-dfa relationships
# dat <- read.csv("data/GOA data/goa.winter.ao.csv", row.names=1)
# dat <- dat %>%
#   mutate(year=as.numeric(row.names(dat))) %>%
#   filter(year %in% 1950:2012)
# 
# 
# stan.new <- data.frame(year=1950:2012,
#                        ndjfm.ao=dat$ndjfm.ao,
#                        name="dfa.trend",
#                        value=as.vector(mod$states))
# 
# stan.new$era <- ifelse(stan.new$year <= 1988, "1950-1988", "1989-2012")
# 
# # and run regression
# dfa.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
#                      data = stan.new,
#                      chains = 4, cores = 4, thin = 1,
#                      warmup = 1000, iter = 4000, refresh = 0,
#                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# ##############
# # and plot!
# 
# lst <- list(dfa.stan)
# 
# 
# # extract intercepts
# lst.int <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2012"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_indv_arm <- plyr::rbind.fill(lst.int)
# 
# mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")
# 
# ## extract slopes
# lst.slope <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("ndjfm.ao", "era1989-2012:ndjfm.ao"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_slope <- plyr::rbind.fill(lst.slope)
# mdf_slope <- reshape2::melt(coef_slope, id.vars = "key")
# 
# 
# # plot intercepts
# int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3]), labels=c("1950-1988", "1989-2013")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Intercept (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(int)
# 
# ggsave("figs/era intercepts - winter SST vs GOA dfa climate - full TS.png", width=10, height=8, units="in")
# 
# # plot slopes
# slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2012")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Slope (scaled anomaly)",
#        y = "Posterior density")
# print(slope)
# 
# ggsave("figs/era slopes - winter SST vs GOA climate dfa trend full TS.png", width=3, height=3, units='in')
# 
# # era probabilities of being > 0
# 
# probs <- mdf_slope %>%
#   group_by(variable) %>%
#   summarize(prob.greater=(sum(value>0)/length(value))) # 99.0% in era1, 70.8% in era2

# now...the same for EBS!!
dat <- read.csv("data/climate data.csv")

names(dat)

# subset (no sst)
wind <- c( "NW.wind.May.Sep", "NW.wind.Oct.Apr", "SE.wind.May.Sep", "SE.wind.Oct.Apr", "south.wind.stress.amj")

ice <- c("ice.area.jfma","m4.march.ice", "m5.march.ice")

ice.dat <- dat %>%
  select(ice)

ice.dat <- as.matrix(t(ice.dat))

wind.dat <- dat %>%
  select(wind)

wind.dat <- as.matrix(t(wind.dat))

colnames(ice.dat) <- colnames(wind.dat) <- dat$year


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
  for(m in 1:2) {  # allowing up to 2 trends for wind
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(wind.dat[,colnames(ice.dat) %in% 1951:2013], model=dfa.model, control=cntl.list,
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

ice.mod.data <- model.data # unconstrained produces no loadings!
wind.mod.data <- model.data # also equal var covar - 1 trend is best!

# fit best models and plot.... 
# fit model
model.list = list(A="zero", m=1, R="diagonal and unequal")
ice.mod = MARSS(ice.dat[,colnames(ice.dat) %in% 1951:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
model.list = list(A="zero", m=1, R="equalvarcov")
wind.mod = MARSS(wind.dat[,colnames(wind.dat) %in% 1951:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# get CI and plot loadings...

ice.CI <- MARSSparamCIs(ice.mod)

ice.plot.CI <- data.frame(names=rownames(ice.dat),
                      mean=ice.CI$par$Z,
                      upCI=ice.CI$par.upCI$Z,
                      lowCI=ice.CI$par.lowCI$Z)

dodge <- position_dodge(width=0.9)

# rename variables for plotting!
# plot.CI$names <- ifelse(plot.CI$names=="NDJ.grad", "SLP gradient",
#                         ifelse(plot.CI$names=="FMA.FW", "Freshwater discharge",
#                                ifelse(plot.CI$names=="FMA.WS", "Wind stress",
#                                       ifelse(plot.CI$names=="Papa", "Papa advection", "SSH"))))

ice.plot.CI$names <- reorder(ice.plot.CI$names, ice.CI$par$Z)

ice.loadings.plot <- ggplot(ice.plot.CI, aes(x=names, y=mean)) +
  geom_bar(position=dodge, stat="identity", fill=cb[2]) +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0)

ggsave("figs/EBS ice dfa loadings.png", width=2.5, height=4, units='in')

wind.CI <- MARSSparamCIs(wind.mod)

wind.plot.CI <- data.frame(names=rownames(wind.dat),
                          mean=wind.CI$par$Z,
                          upCI=wind.CI$par.upCI$Z,
                          lowCI=wind.CI$par.lowCI$Z)

dodge <- position_dodge(width=0.9)

# rename variables for plotting!
# plot.CI$names <- ifelse(plot.CI$names=="NDJ.grad", "SLP gradient",
#                         ifelse(plot.CI$names=="FMA.FW", "Freshwater discharge",
#                                ifelse(plot.CI$names=="FMA.WS", "Wind stress",
#                                       ifelse(plot.CI$names=="Papa", "Papa advection", "SSH"))))

wind.plot.CI$names <- reorder(wind.plot.CI$names, wind.CI$par$Z)

wind.loadings.plot <- ggplot(wind.plot.CI, aes(x=names, y=mean)) +
  geom_bar(position=dodge, stat="identity", fill=cb[2]) +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0)

ggsave("figs/EBS wind dfa loadings.png", width=2.5, height=4, units='in')

# plot trend
ice.trend <- data.frame(t=1951:2013,
                         estimate=as.vector(ice.mod$states),
                         conf.low=as.vector(ice.mod$states)-1.96*as.vector(ice.mod$states.se),
                         conf.high=as.vector(ice.mod$states)+1.96*as.vector(ice.mod$states.se))


ice.trend.plot <- ggplot(ice.trend, aes(t, estimate)) +
  theme_bw() +
  geom_line(color=cb[2]) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1, fill=cb[2]) + xlab("") + ylab("Trend")

ggsave("figs/EBS ice dfa trend.png", width=4, height=2.5, units='in')

wind.trend <- data.frame(t=1951:2013,
                        estimate=as.vector(wind.mod$states),
                        conf.low=as.vector(wind.mod$states)-1.96*as.vector(wind.mod$states.se),
                        conf.high=as.vector(wind.mod$states)+1.96*as.vector(wind.mod$states.se))


wind.trend.plot <- ggplot(wind.trend, aes(t, estimate)) +
  theme_bw() +
  geom_line(color=cb[2]) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1, fill=cb[2]) + xlab("") + ylab("Trend")

ggsave("figs/EBS wind dfa trend.png", width=4, height=2.5, units='in')

# and make a combined plot
png("figs/EBS ice wind dfa plots.png", 8, 6, units='in', res=300)
ggpubr::ggarrange(ice.loadings.plot, ice.trend.plot,
                  wind.loadings.plot, wind.trend.plot,
                  ncol=2, nrow=2, labels=c("a)", "b)", "c)", "d)"),
                  widths = c(0.7, 1))
dev.off()

# now! 25-year rolling correlations to evaluate changing relationships with PDO/SST/AO

# get rolling 25-yr correlations

# put together data frame of DFA trends, sst, and large-scale modes
cor.dat <- data.frame(year = 1951:2013,
                      wind.trend = wind.trend$estimate,
                      ice.trend = ice.trend$estimate)

other.dat <- dat %>%
  select(year, south.ao.ndjfm, AO.jfm)

cor.dat <- left_join(cor.dat, other.dat)

other.dat <- read.csv("data/winter pdo-npgo.csv")

other.dat <- other.dat %>%
  select(-npgo.ndjfm)

cor.dat <- left_join(cor.dat, other.dat)

ebs.cor <- data.frame()
modes <- names(cor.dat)[4:6]

for(j in 1:length(modes)){
  # j <- 1
  temp <- cor.dat %>%
    select(year, wind.trend, ice.trend, modes[j])
  
  for(i in 1963:2001){
    # i <- 1990
    ebs.cor <- rbind(ebs.cor,
                     data.frame(year=i,
                                mode=modes[j],
                                wind.cor=cor(temp$wind.trend[temp$year %in% (i-12):(i+12)],
                                        temp[temp$year %in% (i-12):(i+12),4]),
                     ice.cor=cor(temp$ice.trend[temp$year %in% (i-12):(i+12)],
                                  temp[temp$year %in% (i-12):(i+12),4])))
    
  }}


# now, restrict to correlation time series with 
# absolute values >= 0.5 for at least 1 25-yr window!
# 
# ff <- function(x) max(abs(x), na.rm=T)>=0.5
# 
# goa.cor <- plyr::ddply(goa.cor, "mode", mutate, keep = ff(cor))


# rename with plot-friendly names!
ebs.cor$mode <- ifelse(ebs.cor$mode=="AO.jfm", "AO",
                       ifelse(ebs.cor$mode=='pdo.ndjfm', "PDO", "SST"))

# goa.cor$mode.order <- ifelse(goa.cor$mode=="PDO", 1,
#                              ifelse(goa.cor$mode=="NPGO", 2, 3))
# 
# goa.cor$mode <- reorder(goa.cor$mode, goa.cor$mode.order)

ebs.cor <- ebs.cor %>%
  pivot_longer(cols=c(-year, -mode))

ebs.cor.plot <- ggplot(ebs.cor, aes(year, value, color=name)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~mode) +
  xlab("Center of 25-year window") +
  ylab("Pearson's correlation") +
  geom_vline(xintercept = 1988.5, lty=2) +
  geom_hline(yintercept = 0, color="gray") +
  scale_color_manual(values=cb[c(2,4)], labels=c("Ice trend", "Wind trend")) +
  theme(legend.title = element_blank(), legend.position = 'top') 

ggsave("figs/EBS wind and ice correlations to SST PDO AO.png", width=7, height=3, units="in")

# plot # of years post-88/89 data in each window vs. correlation!

# and, out of curiosity - regression coefficients from rolling windows
ebs.regr <- data.frame()
modes <- names(cor.dat)[4:6]

for(j in 1:length(modes)){
  # j <- 1
  temp <- cor.dat %>%
    select(year, wind.trend, ice.trend, modes[j])
  
  for(i in 1963:2001){
    # i <- 1990
    
    wind.mod <- lm(temp$wind.trend[temp$year %in% (i-12):(i+12)] ~ temp[temp$year %in% (i-12):(i+12),4])
    ice.mod <- lm(temp$ice.trend[temp$year %in% (i-12):(i+12)] ~ temp[temp$year %in% (i-12):(i+12),4])    
    
    ebs.regr <- rbind(ebs.regr,
                     data.frame(year=i,
                                mode=modes[j],
                                wind.regr=wind.mod$coefficients[2],
                                ice.regr=ice.mod$coefficients[2]))
    
  }}


# rename with plot-friendly names!
ebs.regr$mode <- ifelse(ebs.regr$mode=="AO.jfm", "AO",
                       ifelse(ebs.regr$mode=='pdo.ndjfm', "PDO", "SST"))

# goa.cor$mode.order <- ifelse(goa.cor$mode=="PDO", 1,
#                              ifelse(goa.cor$mode=="NPGO", 2, 3))
# 
# goa.cor$mode <- reorder(goa.cor$mode, goa.cor$mode.order)

ebs.regr <- ebs.regr %>%
  pivot_longer(cols=c(-year, -mode))

ebs.regr.plot <- ggplot(ebs.regr, aes(year, value, color=name)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~mode, scales="free") +
  xlab("Center of 25-year window") +
  ylab("Pearson's correlation") +
  geom_vline(xintercept = 1988.5, lty=2) +
  geom_hline(yintercept = 0, color="gray") +
  scale_color_manual(values=cb[c(2,4)], labels=c("Ice trend", "Wind trend")) +
  theme(legend.title = element_blank(), legend.position = 'top') 

# qualitatitvely similar!
ggsave("figs/EBS wind and ice regressions on SST PDO AO.png", width=7, height=3, units="in")

# now, restrict to correlation time series with 
# absolute values >= 0.5 for at least 1 25-yr window!
# 
# ff <- function(x) max(abs(x), na.rm=T)>=0.5
# 
# goa.cor <- plyr::ddply(goa.cor, "mode", mutate, keep = ff(cor))


# rename with plot-friendly names!
ebs.cor$mode <- ifelse(ebs.cor$mode=="AO.jfm", "AO",
                       ifelse(ebs.cor$mode=='pdo.ndjfm', "PDO", "SST"))

# goa.cor$mode.order <- ifelse(goa.cor$mode=="PDO", 1,
#                              ifelse(goa.cor$mode=="NPGO", 2, 3))
# 
# goa.cor$mode <- reorder(goa.cor$mode, goa.cor$mode.order)

ebs.cor <- ebs.cor %>%
  pivot_longer(cols=c(-year, -mode))

ebs.cor.plor <- ggplot(ebs.cor, aes(year, value, color=name)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~mode) +
  xlab("Center of 25-year window") +
  ylab("Pearson's correlation") +
  geom_vline(xintercept = 1988.5, lty=2) +
  geom_hline(yintercept = 0, color="gray") +
  scale_color_manual(values=cb[c(2,4)], labels=c("Ice trend", "Wind trend")) +
  theme(legend.title = element_blank(), legend.position = 'top') 


# now Bayes!
# now make new stan data objects for era regression of sst-dfa relationships
dat <- read.csv("data/climate data.csv")

dat <- dat %>%
  select(year, "south.ao.ndjfm", "AO.jfm") %>%
  filter(year %in% 1951:2013)

# load PDO
d2 <- read.csv("data/winter pdo-npgo.csv")

d2 <- d2 %>%
  select(-npgo.ndjfm)

dat <- left_join(dat, d2)


stan.new <- data.frame(year=1951:2013,
                       ndjfm.ao=dat$south.ao.ndjfm,
                       ndjfm.pdo=dat$pdo.ndjfm,
                       jfm.ao=dat$AO.jfm,
                       name=rep(c("ice.trend", "wind.trend"), each=length(1951:2013)),
                       value=c(ice.mod$states, wind.mod$states))

stan.new$era <- ifelse(stan.new$year <= 1988, "1951-1988", "1989-2013")

 #  THIS MODEL SELECTION BELOW SEEMS TO MOSTLY TRACK SECULAR CHANGES IN WIND TREND; SO DROPPING THIS FOR NOW
# # try looping through candidate threshold years and using LOOIC to find most-supported threshold
# # and degree of overlap between eras !
# thresholds <- 1965:1998 # minimum = 15 years
# 
# model.compare <- data.frame()
# temp <- stan.new
# 
# 
# for(i in 1:length(thresholds)){
# #  i <- 1
#   temp$thresh.era <- ifelse(temp$year <= thresholds[i], 1, 2)
#   
#   
#   mod <- stan_glm(value ~ thresh.era + ndjfm.ao + ndjfm.ao:thresh.era,
#                         data = filter(temp, name=="wind.trend"),
#                         chains = 4, cores = 4, thin = 1,
#                         warmup = 1000, iter = 4000, refresh = 0,
#                         prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                         prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                         prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
#   
#   support <- loo(mod)
#   
#   # and overlap
#   lst <- list(mod)
#   
#   lst.slope <- lapply(lst, function(x) {
#     beta <- as.matrix(x, pars = c("ndjfm.ao", "thresh.era:ndjfm.ao"))
#     data.frame(name = unique(x$data$name),
#                era1 = beta[ , 1],
#                era2 = beta[ , 1] + beta[ , 2])
#   })
#   coef_slope <- plyr::rbind.fill(lst.slope)
#   mdf_slope <- reshape2::melt(coef_slope, id.vars = "name")
#   
# 
#     # calculate pairwise overlaps in slopes
#     temp_overlap = overlapping::overlap(x = list(slope1 = coef_slope$era1, slope2=coef_slope$era2))
# 
#   model.compare <- rbind(model.compare,
#                  data.frame(response="wind.trend",
#                             mode="sst",
#                             threshold=thresholds[i],
#                             looic=support$estimates[3,1],
#                             looic.se=support$estimates[3,2],
#                             overlap=temp_overlap$OV))
#   #####################
#   # and the same for PDO
#   mod <- stan_glm(value ~ thresh.era + ndjfm.pdo + ndjfm.pdo:thresh.era,
#                   data = filter(temp, name=="wind.trend"),
#                   chains = 4, cores = 4, thin = 1,
#                   warmup = 1000, iter = 4000, refresh = 0,
#                   prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
#   
#   support <- loo(mod)
#   
#   # and overlap
#   lst <- list(mod)
#   
#   lst.slope <- lapply(lst, function(x) {
#     beta <- as.matrix(x, pars = c("ndjfm.pdo", "thresh.era:ndjfm.pdo"))
#     data.frame(name = unique(x$data$name),
#                era1 = beta[ , 1],
#                era2 = beta[ , 1] + beta[ , 2])
#   })
#   coef_slope <- plyr::rbind.fill(lst.slope)
#   mdf_slope <- reshape2::melt(coef_slope, id.vars = "name")
#   
#   
#   # calculate pairwise overlaps in slopes
#   temp_overlap = overlapping::overlap(x = list(slope1 = coef_slope$era1, slope2=coef_slope$era2))
#   
#   model.compare <- rbind(model.compare,
#                          data.frame(response="wind.trend",
#                                     mode="pdo",
#                                     threshold=thresholds[i],
#                                     looic=support$estimates[3,1],
#                                     looic.se=support$estimates[3,2],
#                                     overlap=temp_overlap$OV))
#   
#   #####################
#   # and AO!
#   mod <- stan_glm(value ~ thresh.era + jfm.ao + jfm.ao:thresh.era,
#                   data = filter(temp, name=="wind.trend"),
#                   chains = 4, cores = 4, thin = 1,
#                   warmup = 1000, iter = 4000, refresh = 0,
#                   prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
#   
#   support <- loo(mod)
#   
#   # and overlap
#   lst <- list(mod)
#   
#   lst.slope <- lapply(lst, function(x) {
#     beta <- as.matrix(x, pars = c("jfm.ao", "thresh.era:jfm.ao"))
#     data.frame(name = unique(x$data$name),
#                era1 = beta[ , 1],
#                era2 = beta[ , 1] + beta[ , 2])
#   })
#   coef_slope <- plyr::rbind.fill(lst.slope)
#   mdf_slope <- reshape2::melt(coef_slope, id.vars = "name")
#   
#   
#   # calculate pairwise overlaps in slopes
#   temp_overlap = overlapping::overlap(x = list(slope1 = coef_slope$era1, slope2=coef_slope$era2))
#   
#   model.compare <- rbind(model.compare,
#                          data.frame(response="wind.trend",
#                                     mode="ao",
#                                     threshold=thresholds[i],
#                                     looic=support$estimates[3,1],
#                                     looic.se=support$estimates[3,2],
#                                     overlap=temp_overlap$OV))
#   
#   #####################
#   # finally, AO and ice!
#   mod <- stan_glm(value ~ thresh.era + jfm.ao + jfm.ao:thresh.era,
#                   data = filter(temp, name=="ice.trend"),
#                   chains = 4, cores = 4, thin = 1,
#                   warmup = 1000, iter = 4000, refresh = 0,
#                   prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                   prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
#   
#   support <- loo(mod)
#   
#   # and overlap
#   lst <- list(mod)
#   
#   lst.slope <- lapply(lst, function(x) {
#     beta <- as.matrix(x, pars = c("jfm.ao", "thresh.era:jfm.ao"))
#     data.frame(name = unique(x$data$name),
#                era1 = beta[ , 1],
#                era2 = beta[ , 1] + beta[ , 2])
#   })
#   coef_slope <- plyr::rbind.fill(lst.slope)
#   mdf_slope <- reshape2::melt(coef_slope, id.vars = "name")
#   
#   
#   # calculate pairwise overlaps in slopes
#   temp_overlap = overlapping::overlap(x = list(slope1 = coef_slope$era1, slope2=coef_slope$era2))
#   
#   model.compare <- rbind(model.compare,
#                          data.frame(response="ice.trend",
#                                     mode="ao",
#                                     type="non.stationary",
#                                     threshold=thresholds[i],
#                                     looic=support$estimates[3,1],
#                                     looic.se=support$estimates[3,2],
#                                     overlap=temp_overlap$OV))
# }
# 
# 
# model.compare$conf.high <- model.compare$looic+1.96*model.compare$looic.se
# model.compare$conf.low <- model.compare$looic-1.96*model.compare$looic.se
# 
# # and get LOOIC for the stationary models
# mod <- stan_glm(value ~ jfm.ao,
#                 data = filter(temp, name=="wind.trend"),
#                 chains = 4, cores = 4, thin = 1,
#                 warmup = 1000, iter = 4000, refresh = 0,
#                 prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                 prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                 prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# support <- loo(mod)
# 
# model.compare <- rbind(model.compare,
#                        data.frame(response="wind.trend",
#                                   mode="ao",
#                                   type="stationary",
#                                   threshold=NA,
#                                   looic=support$estimates[3,1],
#                                   looic.se=support$estimates[3,2],
#                                   overlap=NA,
#                                   conf.high=support$estimates[3,1]+1.96*support$estimates[3,2],
#                                   conf.low=support$estimates[3,1]-1.96*support$estimates[3,2]))
# 
# mod <- stan_glm(value ~ ndjfm.pdo,
#                 data = filter(temp, name=="wind.trend"),
#                 chains = 4, cores = 4, thin = 1,
#                 warmup = 1000, iter = 4000, refresh = 0,
#                 prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                 prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                 prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# support <- loo(mod)
# 
# model.compare <- rbind(model.compare,
#                        data.frame(response="wind.trend",
#                                   mode="pdo",
#                                   type="stationary",
#                                   threshold=NA,
#                                   looic=support$estimates[3,1],
#                                   looic.se=support$estimates[3,2],
#                                   overlap=NA,
#                                   conf.high=support$estimates[3,1]+1.96*support$estimates[3,2],
#                                   conf.low=support$estimates[3,1]-1.96*support$estimates[3,2]))
# 
# mod <- stan_glm(value ~ ndjfm.ao,
#                 data = filter(temp, name=="wind.trend"),
#                 chains = 4, cores = 4, thin = 1,
#                 warmup = 1000, iter = 4000, refresh = 0,
#                 prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                 prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                 prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# support <- loo(mod)
# 
# model.compare <- rbind(model.compare,
#                        data.frame(response="wind.trend",
#                                   mode="sst",
#                                   type="stationary",
#                                   threshold=NA,
#                                   looic=support$estimates[3,1],
#                                   looic.se=support$estimates[3,2],
#                                   overlap=NA,
#                                   conf.high=support$estimates[3,1]+1.96*support$estimates[3,2],
#                                   conf.low=support$estimates[3,1]-1.96*support$estimates[3,2]))
# 
# stationary.compare <- model.compare %>%
#   filter(response=="wind.trend", type=="stationary")
# 
# model.compare$stationary.looic <- stationary.compare$looic[match(model.compare$mode, stationary.compare$mode)]
# model.compare$stationary.high <- stationary.compare$conf.high[match(model.compare$mode, stationary.compare$mode)] 
# model.compare$stationary.low <- stationary.compare$conf.low[match(model.compare$mode, stationary.compare$mode)]
# 
# 
# 
# 
# 
# ggplot(filter(model.compare, response=="wind.trend", type=="non.stationary"), aes(threshold, looic)) +
#   theme_bw() +
#   geom_line(color=cb[2]) +
#   geom_ribbon(aes(x=threshold, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1, fill=cb[2]) +
#   facet_wrap(~mode) +
#   geom_line(aes(y=stationary.looic), color=cb[3], lty=2) +
#   geom_ribbon(aes(ymin=stationary.low, ymax=stationary.high),
#               linetype=2, alpha=0.1, fill=cb[3]) +
#   theme(legend.position = "top")
#   
# 
# ggsave("figs/LOOIC threshold EBS wind vs SST PDO AO.png", width=8, height=4, units='in')
# 
# 
# # scatter plots / intercepts / slopes for both 1988/89 and the best-supported threshold (1998)

# first, plot the scatter plots

scatter.88.89.ice <- ggplot(scatter.new, aes(mode.value, value, color=era)) +
  theme_bw() +
  geom_point() +
  scale_color_manual(values=cb[2:3]) +
  geom_smooth(method="lm", se=F) +
  xlab("Climate value") + ylab("Wind anomaly") +
  facet_wrap(~mode, scales="free_x") +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')

ggsave("figs/EBS ice trend climate mode scatter plote pre-post 88-89.png", width=6, height=3, units = 'in')


scatter.new <- stan.new %>% 
  filter(name=="wind.trend") %>%
  select(-name, -year) %>%
  pivot_longer(cols=c(ndjfm.ao, ndjfm.pdo, jfm.ao), names_to = "mode", values_to = "mode.value")


scatter.98.99.wind <- ggplot(scatter.new, aes(mode.value, value, color=era)) +
  theme_bw() +
  geom_point() +
  scale_color_manual(values=cb[2:3]) +
  geom_smooth(method="lm", se=F) +
  xlab("Climate value") + ylab("Wind anomaly") +
  facet_wrap(~mode, scales="free_x") +
  theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top')

ggsave("figs/EBS wind trend climate mode scatter plote pre-post 88-89.png", width=6, height=3, units = 'in')

# stan models
# and run regression - first for sst
ice.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
                     data = filter(stan.new, name=="ice.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

wind.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
                      data = filter(stan.new, name=="wind.trend"),
                      chains = 4, cores = 4, thin = 1,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(ice.stan, wind.stan)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2013"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm.ao <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("ndjfm.ao", "era1989-2013:ndjfm.ao"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope <- plyr::rbind.fill(lst.slope)
mdf_slope.ao <- reshape2::melt(coef_slope, id.vars = "key")

#####################################
# now PDO
ice.stan <- stan_glm(value ~ era + ndjfm.pdo + ndjfm.pdo:era,
                     data = filter(stan.new, name=="ice.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

wind.stan <- stan_glm(value ~ era + ndjfm.pdo + ndjfm.pdo:era,
                      data = filter(stan.new, name=="wind.trend"),
                      chains = 4, cores = 4, thin = 1,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(ice.stan, wind.stan)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2013"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm.pdo <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("ndjfm.pdo", "era1989-2013:ndjfm.pdo"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope <- plyr::rbind.fill(lst.slope)
mdf_slope.pdo <- reshape2::melt(coef_slope, id.vars = "key")

#################################
# and AO!
ice.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
                     data = filter(stan.new, name=="ice.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

wind.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
                      data = filter(stan.new, name=="wind.trend"),
                      chains = 4, cores = 4, thin = 1,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(ice.stan, wind.stan)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2013"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm.ao <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("ndjfm.ao", "era1989-2013:ndjfm.ao"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_slope <- plyr::rbind.fill(lst.slope)
mdf_slope.ao <- reshape2::melt(coef_slope, id.vars = "key")

#############################################
# plot intercepts
int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3]), labels=c("1950-1988", "1989-2013")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int)

# ggsave("figs/era intercepts - winter SST vs EBS ice and wind DFA.png", width=6, height=3, units="in")

# plot slopes
slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2012")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(slope)







# now, putting together the various plots for an exploratory look...
library(magick)
img1 <- image_read("figs/EBS ice trend climate mode scatter plote pre-post 88-89.png")
img2 <- image_read("figs/era-specific PDO and climate - vertical.tiff")
# img <- c(image_scale(img1, "80%"), image_scale(img2, "120%"))
img <- c(img1, img2)

stack <- image_append(image_scale(img, "100%"), stack = TRUE)
image_write(stack, path = "figs/combined Fig2.tiff", format = "tiff")

image_write(stack, path = "figs/combined Fig2.png", format = "png")



## OK, I get it! the improved LOOIC scores for later thresholds are driven by changing mean values in the 
# wind trend! so overlap is what we want to use!!!

# think I will back away from the goal of testing 1988/1989 

# last step for today - compare dfa loadings between the two eras...

dat <- read.csv("data/climate data.csv")

names(dat)

# load PDO
d2 <- read.csv("data/winter pdo-npgo.csv")

d2 <- d2 %>%
  select(-npgo.ndjfm)

dat <- left_join(dat, d2)

# subset 
wind <- c( "NW.wind.May.Sep", "NW.wind.Oct.Apr", "SE.wind.May.Sep", "SE.wind.Oct.Apr", "south.wind.stress.amj")

ice <- c("m4.march.ice", "m5.march.ice") # dropping "ice.area.jfma" b/c is begins too late for a meaningful era comparison

modes <- c("south.ao.ndjfm", "AO.jfm", "pdo.ndjfm")

dfa.dat <- dat %>%
  select(c(wind, ice, modes))

dfa.dat <- as.matrix(t(dfa.dat))

colnames(dfa.dat) <- dat$year


# find best error structure for 1- or 2-trend model

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

# set up forms of R matrices

levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained") # dropping unconstrained as it didn't converge for era1!

# fit models & store results
model.data = data.frame() 

# restrict to local variables only
local.dat <- dfa.dat[1:8,]

for(R in levels.R) {
  for(m in 1:2) { 
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(local.dat[,colnames(local.dat) %in% 1989:2013], model=dfa.model, control=cntl.list,
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

era1 <- model.data # 1 trend, diag and unequal is the best for era1! - I restricted to 1964-1988, as the
# rolling correlations on the full time series suggest changing relationships at the beginning of the time series
era2 <- model.data # 2 trend, diag and unequal is the best!

# fit best models and plot.... 
# fit model
model.list = list(A="zero", m=1, R="diagonal and unequal")
era1.mod = MARSS(local.dat[,colnames(local.dat) %in% 1964:1988], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# plot to assess loadings!
era1.CI <- MARSSparamCIs(era1.mod)

era1.plot.CI <- data.frame(names=rownames(local.dat),
                          mean=era1.CI$par$Z,
                          upCI=era1.CI$par.upCI$Z,
                          lowCI=era1.CI$par.lowCI$Z)

dodge <- position_dodge(width=0.9)

era1.plot.CI$names <- reorder(era1.plot.CI$names, era1.CI$par$Z)

era1.loadings.plot <- ggplot(era1.plot.CI, aes(x=names, y=mean)) +
  geom_bar(position=dodge, stat="identity", fill=cb[2]) +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0)

# this model looks fine! compare with era2 model fit the same way!

era2.mod = MARSS(local.dat[,colnames(local.dat) %in% 1989:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# plot to compare loadings!
era2.CI <- MARSSparamCIs(era2.mod)

era2.plot.CI <- data.frame(names=rownames(local.dat),
                           mean=era2.CI$par$Z,
                           upCI=era2.CI$par.upCI$Z,
                           lowCI=era2.CI$par.lowCI$Z)

dodge <- position_dodge(width=0.9)

era2.plot.CI$era <- "era2"
era2.plot.CI$plot.order <- era1.plot.CI$mean

era1.plot.CI$era <- "era1"
era1.plot.CI$plot.order <- era1.plot.CI$mean


plot.both <- rbind(era1.plot.CI, era2.plot.CI)
plot.both$era <- as.factor(plot.both$era)

plot.both$names <- reorder(plot.both$names, plot.both$plot.order)

both.eras.loadings.plot <- ggplot(plot.both, aes(x=names, y=mean, fill=era)) +
  geom_bar(position=dodge, stat="identity") +
  scale_fill_manual(values=cb[2:3]) +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0)

# generally similar...SE wind May.Sep flips sign, NW wind Oct.Apr drops out...loadings on ice and SST become stronder (?)

# compare the strength of correlations by era!
cor1 <- cor(t(local.dat[,colnames(local.dat) %in% 1964:1988]))
cor2 <- cor(t(local.dat[,colnames(local.dat) %in% 1989:2013]))


cors <- data.frame(era1=cor1[lower.tri(cor1)],
                   era2=cor2[lower.tri(cor2)])


ggplot(cors, aes(era1, era2)) +
  theme_bw() +
  geom_point() +
  geom_abline(slope=1, intercept = 0)

cors <- cors %>%
  pivot_longer(cols=c(era1, era2))

ggplot(cors, aes(abs(value))) +
  theme_bw() +
  geom_histogram(fill=cb[2], color="black", bins=10) + 
    facet_wrap(~name, ncol=1)

ggsave("figs/EBS climate cross-correlation strength.png", width=4, height=6, units='in')
# so some indication of a reduction in overall strength, but nothing like what we see in the GOA analysis

# look at the variance explained by PC on the two eras!
pca1 <- prcomp(t(local.dat[,colnames(local.dat) %in% 1964:1988]), scale=T)
pca2 <- prcomp(t(local.dat[,colnames(local.dat) %in% 1989:2013]), scale=T)

pca1$sdev[1:5]^2/sum(pca1$sdev^2)
pca2$sdev[1:5]^2/sum(pca2$sdev^2)

plot(pca1)
plot(pca2)

# does seem to be an increased importance of PC2 in the second era!


############################################
# finally, (!), let's fit the best model for era 2 to each era...

# best model for era 2 is 2-trend, diagonal and unequal
model.list = list(A="zero", m=2, R="diagonal and unequal")
era1.mod = MARSS(local.dat[,colnames(local.dat) %in% 1964:1988], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
era2.mod = MARSS(local.dat[,colnames(local.dat) %in% 1989:2013], model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# and rotate the loadings
Z.est = coef(era1.mod, type="matrix")$Z
H.inv = varimax(coef(era1.mod, type="matrix")$Z)$rotmat
Z.rot.era1 = as.data.frame(Z.est %*% H.inv)

Z.est = coef(era2.mod, type="matrix")$Z
H.inv = varimax(coef(era2.mod, type="matrix")$Z)$rotmat
Z.rot.era2 = as.data.frame(Z.est %*% H.inv)

plot.trend1 <- data.frame(time.series=rownames(local.dat),
                          era1=Z.rot.era1$V1,
                          era2=Z.rot.era2$V1,
                          plot.order=Z.rot.era1$V1)

plot.trend1 <- plot.trend1 %>%
  pivot_longer(cols=c(-time.series, -plot.order))

plot.trend1$time.series <- reorder(plot.trend1$time.series, plot.trend1$plot.order)

ggplot(plot.trend1, aes(x=time.series, y=value, fill=name)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=cb[2:3]) + 
  geom_hline(yintercept = 0, col="dark grey") +
  coord_flip() + 
  ggtitle("Trend 1")

# loadings appear stronger in era 2 - plot the cross-correlation distribution to check'
# cor1 <- cor(t(dfa.dat[,colnames(dfa.dat) %in% 1951:1988]), use="p")
cor2 <- cor(t(dfa.dat[,colnames(local.dat) %in% 1989:2013]), use="p")

# and trend 2!
plot.trend2 <- data.frame(time.series=rownames(local.dat),
                          era1=Z.rot.era1$V2,
                          era2=Z.rot.era2$V2,
                          plot.order=Z.rot.era1$V2)

plot.trend2 <- plot.trend2 %>%
  pivot_longer(cols=c(-time.series, -plot.order))

plot.trend1$time.series <- reorder(plot.trend1$time.series, plot.trend1$plot.order)


ggplot(plot.trend2, aes(x=time.series, y=value, fill=name)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=cb[2:3]) + 
  geom_hline(yintercept = 0, col="dark grey") +
  coord_flip() +
  ggtitle("Trend 2")

# loadings appear stronger in era 2, but I'm not sure I understand what's going on!
# will try the rolling correlation approach I used in the GOA Ecology paper
# object for model data
model.data = data.frame()
# restrict to local TS
local.dat <- dfa.dat[1:8,]

# fit models
for(R in levels.R) {
  for(m in 1:2) {
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(local.dat, model=dfa.model, control=cntl.list,
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


###
arrange(model.data, AICc)

# save model table
write.csv(arrange(model.data, AICc), "DFA model selection environment all years.csv")
# unconstrained 1 adn 2 trend the best, but I'm going to fit the diagonal and unequal for now....
#########
# fit the best model to the entire time series
# and calculate the moving correlations for each TS!

model.list = list(A="zero", m=1, R="unconstrained")
mod <- MARSS(local.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
 # stopped here!!
trend1 <- as.data.frame(matrix(nrow=39, ncol=8))
colnames(trend1) <- rownames(local.dat)

for(i in 1951:1989){
  # i <- 1950
  temp <- local.dat[,colnames(local.dat) %in% i:(i+24)]
  
  for(ii in 1:8){ # loop through each time series/variable
    
    trend1[(i-1950),ii] <- cor(temp[ii,], mod$states[1,(i-1950):(i-1950+24)], use="p")
    
  } }

# now  plot

plot1 <- gather(trend1)
plot1$year <- 1963:2001


ggplot(plot1, aes(year, value)) +
  facet_wrap(~key, nrow=2, ncol=4, scales="free_y") + geom_hline(yintercept = 0) + theme_bw() +
  geom_line(color=cb[2]) + 
  xlab("") + ylab("Correlation") + theme(axis.title.x = element_blank()) + 
  xlim(1963,2001) + 
  geom_vline(xintercept = 1988.5, lty=2)


# check loadings for this unconstrained model!
# plot to assess loadings!
all.CI <- MARSSparamCIs(mod)

all.plot.CI <- data.frame(names=rownames(dfa.dat),
                           mean=all.CI$par$Z,
                           upCI=all.CI$par.upCI$Z,
                           lowCI=all.CI$par.lowCI$Z)

dodge <- position_dodge(width=0.9)


all.plot.CI$names <- reorder(all.plot.CI$names, all.CI$par$Z)

ggplot(all.plot.CI, aes(x=names, y=mean)) +
  geom_bar(position=dodge, stat="identity", fill=cb[2]) +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0)


# get CI and plot loadings...

era1.CI <- MARSSparamCIs(era1.mod)

era1.plot.CI <- data.frame(names=rownames(dfa.dat),
                          mean=era1.CI$par$Z,
                          upCI=era1.CI$par.upCI$Z,
                          lowCI=era1.CI$par.lowCI$Z,
                          era="1951-1988")

era2.CI <- MARSSparamCIs(era2.mod)

era2.plot.CI <- data.frame(names=rownames(dfa.dat),
                           mean=era2.CI$par$Z,
                           upCI=era2.CI$par.upCI$Z,
                           lowCI=era2.CI$par.lowCI$Z,
                           era="1989-2013")

dodge <- position_dodge(width=0.9)

# rename variables for plotting!
# plot.CI$names <- ifelse(plot.CI$names=="NDJ.grad", "SLP gradient",
#                         ifelse(plot.CI$names=="FMA.FW", "Freshwater discharge",
#                                ifelse(plot.CI$names=="FMA.WS", "Wind stress",
#                                       ifelse(plot.CI$names=="Papa", "Papa advection", "SSH"))))

both.plot.CI <- rbind(era1.plot.CI, era2.plot.CI)
both.plot.CI$era1.loading <- era1.plot.CI$mean[match(both.plot.CI$names, era1.plot.CI$names)]

both.plot.CI$names <- reorder(both.plot.CI$names, both.plot.CI$era1.loading)

plot <- ggplot(both.plot.CI, aes(x=names, y=mean, fill=era)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme_bw() +
  scale_fill_manual(values = cb[2:3]) +
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), 
        legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0)


wind.CI <- MARSSparamCIs(wind.mod)

wind.plot.CI <- data.frame(names=rownames(wind.dat),
                           mean=wind.CI$par$Z,
                           upCI=wind.CI$par.upCI$Z,
                           lowCI=wind.CI$par.lowCI$Z)

dodge <- position_dodge(width=0.9)

# rename variables for plotting!
# plot.CI$names <- ifelse(plot.CI$names=="NDJ.grad", "SLP gradient",
#                         ifelse(plot.CI$names=="FMA.FW", "Freshwater discharge",
#                                ifelse(plot.CI$names=="FMA.WS", "Wind stress",
#                                       ifelse(plot.CI$names=="Papa", "Papa advection", "SSH"))))

wind.plot.CI$names <- reorder(wind.plot.CI$names, wind.CI$par$Z)

wind.loadings.plot <- ggplot(wind.plot.CI, aes(x=names, y=mean)) +
  geom_bar(position=dodge, stat="identity", fill=cb[2]) +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=45, hjust=1,  size=12), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0)

ggsave("figs/EBS wind dfa loadings.png", width=2.5, height=4, units='in')

# plot trend
ice.trend <- data.frame(t=1951:2013,
                        estimate=as.vector(ice.mod$states),
                        conf.low=as.vector(ice.mod$states)-1.96*as.vector(ice.mod$states.se),
                        conf.high=as.vector(ice.mod$states)+1.96*as.vector(ice.mod$states.se))


ice.trend.plot <- ggplot(ice.trend, aes(t, estimate)) +
  theme_bw() +
  geom_line(color=cb[2]) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1, fill=cb[2]) + xlab("") + ylab("Trend")

ggsave("figs/EBS ice dfa trend.png", width=4, height=2.5, units='in')

wind.trend <- data.frame(t=1951:2013,
                         estimate=as.vector(wind.mod$states),
                         conf.low=as.vector(wind.mod$states)-1.96*as.vector(wind.mod$states.se),
                         conf.high=as.vector(wind.mod$states)+1.96*as.vector(wind.mod$states.se))


wind.trend.plot <- ggplot(wind.trend, aes(t, estimate)) +
  theme_bw() +
  geom_line(color=cb[2]) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1, fill=cb[2]) + xlab("") + ylab("Trend")

ggsave("figs/EBS wind dfa trend.png", width=4, height=2.5, units='in')

# and make a combined plot
png("figs/EBS ice wind dfa plots.png", 8, 6, units='in', res=300)
ggpubr::ggarrange(ice.loadings.plot, ice.trend.plot,
                  wind.loadings.plot, wind.trend.plot,
                  ncol=2, nrow=2, labels=c("a)", "b)", "c)", "d)"),
                  widths = c(0.7, 1))
dev.off()



########################################
# ggplot(stan.data, aes(scale.value)) +
#   geom_histogram() +
#   facet_wrap(~name)
# 
# # stan era-specific regressions
# FMA.FW <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                    data = stan.data[stan.data$name=="FMA.FW",],
#                    chains = 4, cores = 4, thin = 1,
#                    warmup = 1000, iter = 4000, refresh = 0,
#                    prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# FMA.SSH <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                     data = stan.data[stan.data$name=="FMA.SSH",],
#                     chains = 4, cores = 4, thin = 1,
#                     warmup = 1000, iter = 4000, refresh = 0,
#                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# FMA.WS <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                    data = stan.data[stan.data$name=="FMA.WS",],
#                    chains = 4, cores = 4, thin = 1,
#                    warmup = 1000, iter = 4000, refresh = 0,
#                    prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                    prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# NDJ.grad <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                      data = stan.data[stan.data$name=="NDJ.grad",],
#                      chains = 4, cores = 4, thin = 1,
#                      warmup = 1000, iter = 4000, refresh = 0,
#                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# 
# Papa <- stan_glm(scale.value ~ era + ndjfm.ao + ndjfm.ao:era,
#                  data = stan.data[stan.data$name=="Papa",],
#                  chains = 4, cores = 4, thin = 1,
#                  warmup = 1000, iter = 4000, refresh = 0,
#                  prior = normal(location = 0, scale = 5, autoscale = FALSE),
#                  prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
#                  prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))
# ##############
# # and plot!
# 
# lst <- list(FMA.FW, FMA.SSH, FMA.WS, NDJ.grad, Papa)
# 
# 
# # extract intercepts
# lst.int <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2013"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_indv_arm <- plyr::rbind.fill(lst.int)
# 
# mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")
# 
# ## extract slopes
# lst.slope <- lapply(lst, function(x) {
#   beta <- as.matrix(x, pars = c("ndjfm.ao", "era1989-2013:ndjfm.ao"))
#   data.frame(key = unique(x$data$name),
#              era1 = beta[ , 1],
#              era2 = beta[ , 1] + beta[ , 2])
# })
# coef_slope <- plyr::rbind.fill(lst.slope)
# mdf_slope <- reshape2::melt(coef_slope, id.vars = "key")
# 
# 
# # plot intercepts
# int <- ggplot(mdf_indv_arm, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2013")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Intercept (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(int)
# 
# ggsave("figs/era intercepts - winter SST vs GOA climate.png", width=10, height=8, units="in")
# 
# # plot slopes
# slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
#   theme_bw() +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1964-1988", "1989-2013", "2014-2019")) +
#   theme(legend.title = element_blank(), legend.position = 'top') +
#   geom_vline(xintercept = 0, lty = 2) +
#   labs(x = "Slope (scaled anomaly)",
#        y = "Posterior density") +
#   facet_wrap( ~ key, scales="free")
# print(slope)
# 
# 


# ggplot(filter(model.compare, response=="wind.trend"), aes(threshold, overlap)) +
#   theme_bw() +
#   geom_line(color=cb[2]) +
#   facet_wrap(~mode)
# 
# ggsave("figs/overlap threshold EBS wind vs SST PDO AO.png", width=8, height=4, units='in')
# 
# 
# ggplot(filter(model.compare, response=="ice.trend"), aes(threshold, looic)) +
#   theme_bw() +
#   geom_line(color=cb[2]) +
#   geom_ribbon(aes(x=threshold, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1, fill=cb[2]) +
#   facet_wrap(~mode, scales="free_y") # no real support for a change here!
# 

ggplot(filter(model.compare, response=="ice.trend"), aes(threshold, overlap)) +
  theme_bw() +
  geom_line(color=cb[2]) +
  facet_wrap(~mode, scales="free_y") # but a clear decline here!
# 

# and run regression
ice.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
                     data = filter(stan.new, name=="ice.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

wind.stan <- stan_glm(value ~ era + ndjfm.ao + ndjfm.ao:era,
                     data = filter(stan.new, name=="wind.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(ice.stan, wind.stan)


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
  beta <- as.matrix(x, pars = c("ndjfm.ao", "era1989-2013:ndjfm.ao"))
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
  scale_fill_manual(values = c(cb[2], cb[3]), labels=c("1950-1988", "1989-2013")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int)

ggsave("figs/era intercepts - winter SST vs EBS ice and wind DFA.png", width=6, height=3, units="in")

# plot slopes
slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2012")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(slope)

ggsave("figs/era slopes - winter SST vs EBS climate dfa wind and ice.png", width=6, height=3, units='in')

# era probabilities of being > 0

probs <- mdf_slope %>%
  group_by(key,variable) %>%
  summarize(prob.greater=(sum(value>0)/length(value))) # 98.5% in era1, 57.0% in era2

##############
# now try the same with AO / PDO / NPGO!
# doing this super-fast and super-clunky!
dat <- read.csv("data/climate data.csv")

dat <- dat %>%
  select(year, AO.jfm) %>%
  filter(year %in% 1951:2013)


stan.new <- data.frame(year=1951:2013,
                       AO=dat$AO.jfm,
                       name=rep(c("ice.trend", "wind.trend"), each=length(1951:2013)),
                       value=c(ice.mod$states, wind.mod$states))

stan.new$era <- ifelse(stan.new$year <= 1988, "1950-1988", "1989-2012")

# and run regression
ice.stan <- stan_glm(value ~ era + AO + AO:era,
                     data = filter(stan.new, name=="ice.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

wind.stan <- stan_glm(value ~ era + AO + AO:era,
                      data = filter(stan.new, name=="wind.trend"),
                      chains = 4, cores = 4, thin = 1,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(ice.stan, wind.stan)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2012"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("AO", "era1989-2012:AO"))
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
  scale_fill_manual(values = c(cb[2], cb[3]), labels=c("1950-1988", "1989-2013")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int)

ggsave("figs/era intercepts - JFM AO vs EBS ice and wind DFA.png", width=6, height=3, units="in")

# plot slopes
slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2012")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(slope)

ggsave("figs/era slopes - JFM AO vs EBS climate dfa wind and ice.png", width=6, height=3, units='in')

# era probabilities of being > 0

probs <- mdf_slope %>%
  group_by(key,variable) %>%
  summarize(prob.greater=(sum(value<0)/length(value))) # wind 49.5% neg in era1, 99.6% neg in era2

########################
# PDO!
dat <- read.csv("data/winter pdo-npgo.csv")

dat <- dat %>%
  filter(year %in% 1951:2013)

stan.new <- data.frame(year=1951:2013,
                       PDO=dat$pdo.ndjfm,
                       name=rep(c("ice.trend", "wind.trend"), each=length(1951:2013)),
                       value=c(ice.mod$states, wind.mod$states))

stan.new$era <- ifelse(stan.new$year <= 1988, "1950-1988", "1989-2012")

# and run regression
ice.stan <- stan_glm(value ~ era + PDO + PDO:era,
                     data = filter(stan.new, name=="ice.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

wind.stan <- stan_glm(value ~ era + PDO + PDO:era,
                      data = filter(stan.new, name=="wind.trend"),
                      chains = 4, cores = 4, thin = 1,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(ice.stan, wind.stan)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2012"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("PDO", "era1989-2012:PDO"))
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
  scale_fill_manual(values = c(cb[2], cb[3]), labels=c("1950-1988", "1989-2013")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int)

ggsave("figs/era intercepts - NDJFM PDO vs EBS ice and wind DFA.png", width=6, height=3, units="in")

# plot slopes
slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2012")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(slope)

ggsave("figs/era slopes - winter PDO vs EBS climate dfa wind and ice.png", width=6, height=3, units='in')

# era probabilities of being > 0

probs <- mdf_slope %>%
  group_by(key,variable) %>%
  summarize(prob.greater=(sum(value<0)/length(value))) # wind 49.5% neg in era1, 99.6% neg in era2

###
# and npgo
dat <- read.csv("data/winter pdo-npgo.csv")

dat <- dat %>%
  filter(year %in% 1951:2013)

stan.new <- data.frame(year=1951:2013,
                       NPGO=dat$npgo.ndjfm,
                       name=rep(c("ice.trend", "wind.trend"), each=length(1951:2013)),
                       value=c(ice.mod$states, wind.mod$states))

stan.new$era <- ifelse(stan.new$year <= 1988, "1950-1988", "1989-2012")

# and run regression
ice.stan <- stan_glm(value ~ era + NPGO + NPGO:era,
                     data = filter(stan.new, name=="ice.trend"),
                     chains = 4, cores = 4, thin = 1,
                     warmup = 1000, iter = 4000, refresh = 0,
                     prior = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                     prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

wind.stan <- stan_glm(value ~ era + NPGO + NPGO:era,
                      data = filter(stan.new, name=="wind.trend"),
                      chains = 4, cores = 4, thin = 1,
                      warmup = 1000, iter = 4000, refresh = 0,
                      prior = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_intercept = normal(location = 0, scale = 5, autoscale = FALSE),
                      prior_aux = student_t(df = 3, location = 0, scale = 5, autoscale = FALSE))

##############
# and plot!

lst <- list(ice.stan, wind.stan)


# extract intercepts
lst.int <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("(Intercept)", "era1989-2012"))
  data.frame(key = unique(x$data$name),
             era1 = beta[ , 1],
             era2 = beta[ , 1] + beta[ , 2])
})
coef_indv_arm <- plyr::rbind.fill(lst.int)

mdf_indv_arm <- reshape2::melt(coef_indv_arm, id.vars = "key")

## extract slopes
lst.slope <- lapply(lst, function(x) {
  beta <- as.matrix(x, pars = c("NPGO", "era1989-2012:NPGO"))
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
  scale_fill_manual(values = c(cb[2], cb[3]), labels=c("1950-1988", "1989-2013")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Intercept (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(int)

ggsave("figs/era intercepts - NDJFM NPGO vs EBS ice and wind DFA.png", width=6, height=3, units="in")

# plot slopes
slope <- ggplot(mdf_slope, aes(x = value, fill = variable)) +
  theme_bw() +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(cb[2], cb[3], cb[4]), labels=c("1950-1988", "1989-2012")) +
  theme(legend.title = element_blank(), legend.position = 'top') +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Slope (scaled anomaly)",
       y = "Posterior density") +
  facet_wrap( ~ key, scales="free")
print(slope)

ggsave("figs/era slopes - winter NPGO vs EBS climate dfa wind and ice.png", width=6, height=3, units='in')

# era probabilities of being > 0

probs <- mdf_slope %>%
  group_by(key,variable) %>%
  summarize(prob.greater=(sum(value<0)/length(value))) # wind 49.5% neg in era1, 99.6% neg in era2

##########################################################################################################
# old stuff below!
##########################################################################################################
# ggsave("figs/rolling correlations - EBS climate vars and large-scale modes.png", width=6, height=5, units='in')


# combine into one plot!


### rolling correlations between TS and DFA trend

# set up dfa dat again
dfa.dat <- dat %>%
  select(keep)

years <- dfa.dat$year

dfa.dat <- dfa.dat %>%
  select(-year)

dfa.dat <- as.matrix((t(dfa.dat)))
colnames(dfa.dat) <- years

# limit to 2013
dfa.dat <- dfa.dat[,colnames(dfa.dat) <= 2013]

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

model.data # unconstrained is the best

model.list = list(A="zero", m=1, R="unconstrained")
mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# now add trend back to data and calculate rolling correlations
dfa.dat <- as.data.frame((t(dfa.dat)))
dfa.dat$year <- 1951:2013
dfa.dat$trend <- as.vector(mod$states)

dfa.dat <- dfa.dat %>%
  pivot_longer(cols=c(-year, -trend))

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
                                cor=cor(temp$trend[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
  }}

ggplot(ebs.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations with shared DFA trend") +
  geom_vline(xintercept = 1988.5, lty=1) +
  geom_vline(xintercept = 1976.5, lty=3)

# now...look at rolling cors with SST
dfa.dat <- dfa.dat %>%
  select(-trend) %>%
  pivot_wider(names_from = name, values_from = value)

dfa.dat <- dfa.dat %>%
  pivot_longer(cols=c(-year, -south.ao.ndjfm))
  
  
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
                                cor=cor(temp$south.ao.ndjfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
  }}

ggplot(ebs.cor, aes(year, cor)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~var, scales="free_y") +
  ggtitle("Rolling 25-year correlations with NDJFM south SST") +
  geom_vline(xintercept = 1988.5, lty=1) +
  geom_vline(xintercept = 1976.5, lty=3)


head(dfa.dat)

## look at time series (values) plotted out against year...

# # align and plot time series with similar patterns...
# drop <- c("NW.wind.Oct.Apr", "south.wind.stress.amj")
# drop <- ebs.cor$var %in% drop
# ebs.cor <- ebs.cor[!drop,]
# 
# # reverse some ts...
# ebs.cor$cor[ebs.cor$var=="south.ao.amj"] = -ebs.cor$cor[ebs.cor$var=="south.ao.amj"]
# ebs.cor$cor[ebs.cor$var=="south.ao.ndjfm"] = -ebs.cor$cor[ebs.cor$var=="south.ao.ndjfm"]
# 
# 
# ggplot(ebs.cor, aes(year, cor, color=var)) +
#   theme_bw() +
#   geom_line() +
#   ggtitle("Rolling 25-year correlations") +
#   geom_vline(xintercept = 1988.5, lty=1) +
#   geom_vline(xintercept = 1976.5, lty=3)



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

