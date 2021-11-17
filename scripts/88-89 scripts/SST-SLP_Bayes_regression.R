`# exploratory analysis - 
# Bayesian regression of EBS SST on northern hemisphere SLP fields

library(ggplot2)
library(dplyr)
library(plyr)
library(mgcv)
library(rstan)
library(brms)
library(bayesplot)

library(ncdf4)
library(maps)
library(maptools)
library(mapdata)
library(fields)
library(oce)
library(chron)
library(zoo)
library(mapproj)
library(sinkr)

source("./scripts/stan_utils.R")

# to load sinkr, use package devtools and uncomment the following:
# devtools::install_github("marchtaylor/sinkr")

my.col = oceColorsPalette(64, which = 1)

# load slp
dat <- nc_open("data/NCEP.NCAR.slp.nc")

x <- ncvar_get(dat, "longitude")
y <- ncvar_get(dat, "latitude")
slp <- ncvar_get(dat, "slp", verbose = F)
dim(slp) # 144 long, 29 lat, 864 months

# need to reverse latitude for plotting!
y <- rev(y)
slp <- slp[,29:1,]

# first, extract dates
raw <- ncvar_get(dat, "time") # seconds since 1-1-1970
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))
m <- months(d)
yr <- as.numeric(as.character(years(d)))

# make vectors of lat/long and add (with date) as dimnames
lat <- rep(y, length(x))   
lon <- rep(x, each = length(y)) 

# Change data into a matrix with months / cells for rows / columns
slp <- aperm(slp, 3:1)  
slp <- matrix(slp, nrow=dim(slp)[1], ncol=prod(dim(slp)[2:3]))  

dimnames(slp) <- list(as.character(d), paste("N", lat, "E", lon, sep=""))


# (limit to 40 and N for this first iteration)

drop <- lat < 40
lat <- lat[!drop]
lon <- lon[!drop]
slp <- slp[,!drop]


drop <- y < 40
y <- y[!drop]


# plot to confirm that everything is ok
z <- colMeans(slp, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=my.col, xlab = "", ylab = "")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# limit to 1951:2013
slp <- slp[yr %in% 1950:2013,]
m <- m[yr %in% 1950:2013]
yr <- yr[yr %in% 1950:2013]

#  load sst!
dat2 <- read.csv("data/climate data.csv")

dat2 <- dat2 %>%
  select(year, south.sst.ndjfm)

names(dat2)[2] <- "EBS.sst"


# will use winter (NDJFM) SLP 

# define the winter period
win <- c("Nov", "Dec", "Jan", "Feb", "Mar")

# set Nov and Dec equal to the year corresponding to Jan!
win.yr <- ifelse(m %in% c("Nov", "Dec"), yr+1, yr) 

# and restrict both winter year and slp to the winter months
win.yr <- win.yr[m %in% win]
win.slp <- slp[m %in% win,]

# now get annual winter means for each cell
ff <- function(x) tapply(x, win.yr, mean)

win.slp <- apply(win.slp, 2, ff)

# limit both to 1951:2013
win.slp <- win.slp[rownames(win.slp) %in% 1951:2013,]

dat2 <- dat2 %>%
  dplyr::filter(year %in% 1951:2013)


# scale sst and slp
ff <- function(x) as.vector(scale(x))

dat2$EBS.sst <- ff(dat2$EBS.sst)

# scale winter SLP across the N. Hemisphere; if we scale by cell we'll lose signs
win.slp <- (win.slp - mean(win.slp)) / sd(win.slp)

# plot again to check
z <- colMeans(win.slp, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=my.col, xlab = "", ylab = "")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)


# calculate separate regressions in each era!
# make objects to catch results
# slp.out <- interaction.out <- data.frame()

# temp - reloading first 536 model outputs; also fising DLL issue with update() below!

slp.out <- read.csv("./output/slp.out.csv", row.names = 1)
interaction.out <- read.csv("./output/interaction.out.csv", row.names = 1)

dat2$era <- ifelse(dat2$year <= 1988, "1951-1988", "1989-2013")

# setup brms
sst_slp_formula <-  bf(EBS.sst ~ slp*era)


# set up a version of dat2 with one slp time series and check priors
dat2$slp <- win.slp[,1]
get_prior(sst_slp_formula, dat2)

priors_sst_slp <- c(set_prior("normal(0, 3)", class = "b"),
                    set_prior("student_t(3, 0, 3)", class = "Intercept"),
                    set_prior("student_t(3, 0, 3)", class = "sigma"))`


# now loop through each cell
# for(i in 1:ncol(win.slp)){
  i <- 1

  dat2$slp <- win.slp[,i]


  mod <- brm(sst_slp_formula,
             data = dat2,
             prior = priors_sst_slp,
             seed = 1234,
             cores = 4, chains = 4, iter = 1000,
             save_pars = save_pars(all = TRUE),
             control = list(adapt_delta = 0.9, max_treedepth = 10))

  # check_hmc_diagnostics(mod$fit)
  # neff_lowest(mod$fit)
  # rhat_highest(mod$fit)

  # m1 <- as.data.frame(posterior_summary(mod, probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)))
  # 
  # interaction.out <- rbind(interaction.out,
  #              m1["b_slp:era1989M2013",])
  # 
  # slp.out <- rbind(slp.out,
  #                          m1["b_slp",])


# }

# save for continued fitting below

# write.csv(slp.out, "./output/slp.out.csv")
# write.csv(interaction.out, "./output/interaction.out.csv")

# refitting for iterations 537 on and using update to avoid re-compiling over and over!
for(i in 537:ncol(win.slp)){
  # i <- 1
  
  dat2$slp <- win.slp[,i]
  
  
  # mod <- brm(sst_slp_formula,
  #            data = dat2,
  #            prior = priors_sst_slp,
  #            seed = 1234,
  #            cores = 4, chains = 4, iter = 1000,
  #            save_pars = save_pars(all = TRUE),
  #            control = list(adapt_delta = 0.9, max_treedepth = 10))
  
  mod <- update(mod, 
                newdata = dat2)
  
  # check_hmc_diagnostics(mod$fit)
  # neff_lowest(mod$fit)
  # rhat_highest(mod$fit)
  
  m1 <- as.data.frame(posterior_summary(mod, probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)))
  
  interaction.out <- rbind(interaction.out,
                           m1["b_slp:era1989M2013",])
  
  slp.out <- rbind(slp.out,
                   m1["b_slp",])  
  
}


  
  ######################################
  # and a series of plots
  mt.cex <- 1.1
  l.mar <- 3
  l.cex <- 0.8
  l.l <- 1.2
  tc.l <- -0.2
  
  # define projection details
  my.proj <- "orthographic"
  my.orien <- c(45,180,0)
  
  # get limits for plots
  range(slp.out$Estimate)
  lim <- range(interaction.out$Estimate+slp.out$Estimate)
  
  lim <- c(-lim[2], lim[2])
  
  png("figs/ebs Bayes era1 slp-sst.png", 3, 3, units="in", res=300)
  
  par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
      las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)
  
  ylim <- c(40,90)
  
  new.col <- oce.colorsPalette(64)
  
  map("world2Hires", 
      proj=my.proj, parameters = NULL, orient=my.orien,
      xlim=c(0,350), ylim=c(20,90),
      fill=FALSE, lforce="e")
  
  z <- slp.out$Estimate+interaction.out$Estimate  # replace elements NOT corresponding to land with loadings!
  z <- t(matrix(z, length(y)))
  polys <- matrixPoly(x, y, z)
  COLS <- val2col(z, col = new.col, zlim=lim)
  for(i in seq(polys)){
    tmp <- mapproject(polys[[i]],
                      proj=my.proj, parameters = NULL, orient=my.orien)
    polygon(tmp$x, tmp$y, col=COLS[i], border=COLS[i], lwd=0.1)
  }
  
  map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                       'South Korea', 'North Korea', 'Japan', 'Norway', 'Finland', 'Sweden', 'Denmark',
                       'India', 'Nepal', 'Vietnam', 'Iceland', 'Taiwan'), 
      fill=T,add=T, lwd=0.5, col="darkgoldenrod3", proj=my.proj, parameters = NULL, orient=my.orien)
  
  mtext("SLP vs. EBS SST 1951-1988", adj=0.5, cex=0.8, side=1)
  
  dev.off()
  
  
   ## not sure those make any sense!
  
  
## re-fit with linear models
  
  
  
  
  ####
  png("figs/goa era2 slp-sst.png", 3, 3, units="in", res=300)
  
  par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
      las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)
  
  ylim <- c(40,90)
  
  new.col <- oce.colorsPalette(64)
  
  map("world2Hires", 
      proj=my.proj, parameters = NULL, orient=my.orien,
      xlim=c(0,350), ylim=c(20,90),
      fill=FALSE, lforce="e")
  
  z <- GOA.regr2  # replace elements NOT corresponding to land with loadings!
  z <- t(matrix(z, length(y)))
  polys <- matrixPoly(x, y, z)
  COLS <- val2col(z, col = new.col, zlim=c(lim[1], -lim[1]))
  for(i in seq(polys)){
    tmp <- mapproject(polys[[i]],
                      proj=my.proj, parameters = NULL, orient=my.orien)
    polygon(tmp$x, tmp$y, col=COLS[i], border=COLS[i], lwd=0.1)
  }
  
  map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                       'South Korea', 'North Korea', 'Japan', 'Norway', 'Finland', 'Sweden', 'Denmark',
                       'India', 'Nepal', 'Vietnam', 'Iceland', 'Taiwan'), 
      fill=T,add=T, lwd=0.5, col="darkgoldenrod3", proj=my.proj, parameters = NULL, orient=my.orien)
  
  mtext("SLP vs. GOA SST 1989-2013", adj=0.5, cex=0.8, side=1)
  
  dev.off()
  
  ###########
  
  png("figs/ebs era1 slp-sst.png", 3, 3, units="in", res=300)
  
  par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
      las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)
  
  ylim <- c(40,90)
  
  new.col <- oce.colorsPalette(64)
  
  map("world2Hires", 
      proj=my.proj, parameters = NULL, orient=my.orien,
      xlim=c(0,350), ylim=c(20,90),
      fill=FALSE, lforce="e")
  
  z <- EBS.regr1  # replace elements NOT corresponding to land with loadings!
  z <- t(matrix(z, length(y)))
  polys <- matrixPoly(x, y, z)
  COLS <- val2col(z, col = new.col, zlim=c(lim[1], -lim[1]))
  for(i in seq(polys)){
    tmp <- mapproject(polys[[i]],
                      proj=my.proj, parameters = NULL, orient=my.orien)
    polygon(tmp$x, tmp$y, col=COLS[i], border=COLS[i], lwd=0.1)
  }
  
  map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                       'South Korea', 'North Korea', 'Japan', 'Norway', 'Finland', 'Sweden', 'Denmark',
                       'India', 'Nepal', 'Vietnam', 'Iceland', 'Taiwan'), 
      fill=T,add=T, lwd=0.5, col="darkgoldenrod3", proj=my.proj, parameters = NULL, orient=my.orien)
  
  mtext("SLP vs. EBS SST 1950-1988", adj=0.5, cex=0.8, side=1)
  
  dev.off()
  
  
  
  
# calculate differences for each era
GOA.diff <- GOA.regr2 - GOA.regr1
EBS.diff <- EBS.regr2 - EBS.regr1

diff.lim <- range(GOA.diff, EBS.diff) # limit for plotting

