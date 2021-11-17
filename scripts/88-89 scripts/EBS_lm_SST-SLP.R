# exploratory analysis - 
# regression of EBS SST on northern hemisphere SLP fields

library(ggplot2)
library(dplyr)


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

# scale winter SLP 
win.slp <- apply(win.slp, 2, ff)

# plot again to check
z <- colMeans(win.slp, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=my.col, xlab = "", ylab = "")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)


# calculate separate regressions in each era!
# make objects to catch results
out <- data.frame()

dat2$era <- ifelse(dat2$year <= 1988, "1951-1988", "1989-2013")


# now loop through each cell
for(i in 1:ncol(win.slp)){
# i <- 1

dat2$slp <- win.slp[,i]


mod <- lm(EBS.sst ~ slp*era, data = dat2)

temp <- data.frame(slp = summary(mod)$coefficients[2,1],
                   slpxera = summary(mod)$coefficients[4,1])

out <- rbind(out, temp)

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
lim <- range(out$slpxera)

lim <- c(lim[1], -lim[1])

png("figs/ebs Bayes era1 slp-sst.png", 3, 3, units="in", res=300)

par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
    las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)

ylim <- c(40,90)

new.col <- oce.colorsPalette(64)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(0,350), ylim=c(20,90),
    fill=FALSE, lforce="e")

z <- out$slpxera  # replace elements NOT corresponding to land with loadings!
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
