library(ncdf4)
library(maps)
library(maptools)
library(mapdata)
library(fields)
library(oce)
library(chron)
library(zoo)

# figuring things out!
# correlate SLP & recruitment
# this is (potentially) a good diagnostic of atmosphere-ocean modes important to the stock...

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

# now load recruitment

recruit <- read.csv("data/2019.SSB.recruitment.csv")

# to begin with, we'll look at SLP-recruit correlations for the entire era, and 
# for three eras: 1964-1988, 1989-2013, and 2014-2019

# will use winter (NDJFM) SLP for the two years prior to the recruit year

# could also use regression instead of recruitment, but starting with correlation here as 
# that's what we used for salmon-SST relationships in the reversing-PDO paper!

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
cor.all <- cor.64.88 <- cor.89.13 <- cor.14.19 <- NA

# try weighted correlation using inverse of CV as weight??

for(j in 1:ncol(win.slp2)){
  
  cor.all[j] <- weights::wtd.cors(log(recruit$Recruitment), win.slp2[rownames(win.slp2) %in% 1964:2019,j],
                                  weight=1/recruit$CV.Rec)
  
  cor.64.88[j] <- weights::wtd.cors(log(recruit$Recruitment[recruit$Year <= 1988]), 
                                    win.slp2[rownames(win.slp2) %in% 1964:1988,j], 
                                    weight=1/recruit$CV.Rec[recruit$Year <= 1988])  
  
  cor.89.13[j] <- weights::wtd.cors(log(recruit$Recruitment[recruit$Year %in% 1989:2013]), 
                                    win.slp2[rownames(win.slp2) %in% 1989:2013,j], 
                                    weight=1/recruit$CV.Rec[recruit$Year %in% 1989:2013])  
  
  cor.14.19[j] <- weights::wtd.cors(log(recruit$Recruitment[recruit$Year %in% 2014:2019]), 
                                    win.slp2[rownames(win.slp2) %in% 2014:2019,j], 
                                    weight=1/recruit$CV.Rec[recruit$Year %in% 2014:2019])  
  
}

# plot the resulting maps
# set the limit for plotting so that it is identical across panels
# zlim <- range(cor.all, cor.64.88, cor.89.13, cor.14.19) 
# cor.14.19 == NaN! need to figure out why
# just plot the first three for now
zlim <- range(cor.all, cor.64.88, cor.89.13)

# set colors 
new.col <- oce.colorsPalette(64)

png("figs/weighted recruitment-slp correlations.png", 4, 10, units="in", res=300)

par(mfrow=c(4,1), mar=c(1,1,1,1), las=1)

# full time series
z <- cor.all
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(zlim[1], -zlim[1]))
mtext("1964-2019")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1964-1988
z <- cor.64.88
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("1964-1988")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1989-2013
z <- cor.89.13
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("1989-2013")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

dev.off()

#############

# unweighted correlation!

# vectors to catch correlation coefficients
cor.all <- cor.64.88 <- cor.89.13 <- cor.14.19 <- NA

# try weighted correlation using inverse of CV as weight??

for(j in 1:ncol(win.slp2)){
  
  cor.all[j] <- cor(log(recruit$Recruitment), win.slp2[rownames(win.slp2) %in% 1964:2019,j])
  
  cor.64.88[j] <- cor(log(recruit$Recruitment[recruit$Year <= 1988]), 
                                    win.slp2[rownames(win.slp2) %in% 1964:1988,j])  
  
  cor.89.13[j] <- cor(log(recruit$Recruitment[recruit$Year %in% 1989:2013]), 
                                    win.slp2[rownames(win.slp2) %in% 1989:2013,j])  
  
  cor.14.19[j] <- cor(log(recruit$Recruitment[recruit$Year %in% 2014:2019]), 
                                    win.slp2[rownames(win.slp2) %in% 2014:2019,j])  
  
}

# plot the resulting maps
# set the limit for plotting so that it is identical across panels

zlim <- range(cor.all, cor.64.88, cor.89.13, cor.14.19)

# set colors 
new.col <- oce.colorsPalette(64)

png("figs/recruitment-slp correlations.png", 8, 6, units="in", res=300)

par(mfrow=c(2,2), mar=c(1,1,1,1), las=1)

# full time series
z <- cor.all
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
mtext("1964-2019")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1964-1988
z <- cor.64.88
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
mtext("1964-1988")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1989-2013
z <- cor.89.13
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
mtext("1989-2013")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 2014-2019
z <- cor.14.19
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n", zlim=c(-1,1))
mtext("2014-2019")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

dev.off()

## weighted regression


regr.all <- regr.64.88 <- regr.89.13 <- regr.14.19 <- NA

# try regression

for(j in 1:ncol(win.slp2)){
  
  regr.all[j] <- 
    summary(lm(log(recruit$Recruitment) ~ 
                 scale(win.slp2[rownames(win.slp2) %in% 1964:2019,j])))$coeff[2,2]

  
  regr.64.88[j] <- 
    summary(lm(log(recruit$Recruitment[recruit$Year <= 1988]) ~ 
                      scale(win.slp2[rownames(win.slp2) %in% 1964:1988,j])))$coeff[2,2]
  
  regr.89.13[j] <- 
    summary(lm(log(recruit$Recruitment[recruit$Year %in% 1989:2013]) ~
                      scale(win.slp2[rownames(win.slp2) %in% 1989:2013,j])))$coeff[2,2] 
  
  regr.14.19[j] <- 
    summary(lm(log(recruit$Recruitment[recruit$Year %in% 2014:2019]) ~
                      scale(win.slp2[rownames(win.slp2) %in% 2014:2019,j])))$coeff[2,2]  
  
}

# plot the resulting maps
# set the limit for plotting so that it is identical across panels

zlim <- range(regr.all, regr.64.88, regr.89.13, regr.14.19)

range(regr.14.19)

# set colors 
new.col <- oce.colorsPalette(64)

png("figs/recruitment-slp regression.png", 8, 6, units="in", res=300)

par(mfrow=c(2,2), mar=c(1,1,1,1), las=1)

# full time series
z <- regr.all
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("1964-2019")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1964-1988
z <- regr.64.88
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("1964-1988")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 1989-2013
z <- regr.89.13
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("1989-2013")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# 2014-2019
z <- regr.14.19
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting

image(x,y,z, col=new.col, xlab = "", ylab = "", yaxt="n", xaxt="n")
mtext("2014-2019")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

dev.off()