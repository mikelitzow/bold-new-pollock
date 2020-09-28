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
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", xlim=c(160,220), ylim=c(50,70))

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)


# now sst
dat2 <- read.csv("data/climate data.csv")

dat2 <- dat2 %>%
  select(year, south.sst.ndjfm)
names(dat2)[2] <- "EBS.sst"

dat3 <- read.csv("data/GOA data/goa.winter.sst.csv")
names(dat3)[2] <- "GOA.sst"

sst <- left_join(dat2, dat3)

sst$EBS.sst <- scale(sst$EBS.sst)
sst$GOA.sst <- scale(sst$GOA.sst)

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

# limit to EBS!

poly.x <- c(360-169, 360-169, 360-153, 360-153, 360-169) 
poly.y <- c(47, 57, 57, 47, 47)

ebs.slp <- slp

xp <- cbind(poly.x, poly.y)
loc=cbind(lon, lat)
check <- in.poly(loc, xp=xp)

ebs.slp[,!check] <- NA

# plot to check

z <- colMeans(ebs.slp, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", xlim=c(160,220), ylim=c(40,70))

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)

# now define winter
win <- c("Nov", "Dec", "Jan", "Feb", "Mar")

# set Nov and Dec equal to the year corresponding to Jan!
win.yr <- ifelse(m %in% c("Nov", "Dec"), yr+1, yr) 
# and restrict both winter year and slp to the winter months
win.yr <- win.yr[m %in% win]
win.slp <- ebs.slp[m %in% win,]



# now get annual winter means for each cell
ff <- function(x) tapply(x, win.yr, mean)

win.slp <- apply(win.slp, 2, ff)

# drop winter 2020, whixh is incomplete
win.slp <- win.slp[rownames(win.slp) < 2020,]
ebs.win.slp <- rowMeans(win.slp, na.rm = T)


# quick plot!
plot(ebs.win.slp[names(ebs.win.slp) %in% 1951:2019], sst$EBS.sst[sst$year %in% 1951:2019], type="n")
text(ebs.win.slp[names(ebs.win.slp) %in% 1951:1988], sst$EBS.sst[sst$year %in% 1951:1988], labels=1951:1988, col="blue")
text(ebs.win.slp[names(ebs.win.slp) %in% 1989:2012], sst$EBS.sst[sst$year %in% 1989:2012], labels=1989:2012, col="red")


plot.dat <- data.frame(year=1951:2019,
                       slp=ebs.win.slp[names(ebs.win.slp) %in% 1951:2019],
                       sst=sst$EBS.sst[sst$year %in% 1951:2019])

plot.dat$era <- as.factor(ifelse(plot.dat$year < 1989, 1, 
                       ifelse(plot.dat$year %in% 1989:2013, 2, 3)))


ggplot(filter(plot.dat, era != 3), aes(slp, sst, color=era)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method="lm")


plot.dat$slp <- scale(plot.dat$slp)
new.dat <- plot.dat %>%
  pivot_longer(cols=c(-year, -era))

ggplot(new.dat, aes(year, value, color=name)) +
  theme_bw() +
  geom_line() + 
  geom_vline(xintercept = 1988.5, lty=2)

ggplot(new.dat, aes(value, fill=era)) +
  theme_bw() +
  geom_density(alpha=0.2) +
  facet_wrap(~name) +
  xlim(-3,3)

# separate SLP, PDO, and NPGO into era-specific chunks for regression maps
win.slp1 <- win.slp[rownames(win.slp) %in% 1951:1988,]
win.slp2 <- win.slp[rownames(win.slp) %in% 1989:2013,]

GOA1 <- sst$GOA.sst[sst$year %in% 1951:1988]
GOA2 <- sst$GOA.sst[sst$year %in% 1989:2013]

EBS1 <- sst$EBS.sst[sst$year %in% 1951:1988]
EBS2 <- sst$EBS.sst[sst$year %in% 1989:2013]

# calculate separate regressions in each era!
# make objects to catch results
GOA.regr1 <- GOA.regr2 <- EBS.regr1 <- EBS.regr2 <- NA

# now loop through each cell
for(i in 1:ncol(win.slp1)){
  #  i <- 1
  mod <- lm(win.slp1[,i] ~ GOA1)
  GOA.regr1[i] <- summary(mod)$coef[2,1]
  
  mod <- lm(win.slp2[,i] ~ GOA2)
  GOA.regr2[i] <- summary(mod)$coef[2,1]
  
  mod <- lm(win.slp1[,i] ~ EBS1)
  EBS.regr1[i] <- summary(mod)$coef[2,1] 
  
  mod <- lm(win.slp2[,i] ~ EBS2)
  EBS.regr2[i] <- summary(mod)$coef[2,1] 
}


# calculate differences for each era
GOA.diff <- GOA.regr2 - GOA.regr1
EBS.diff <- EBS.regr2 - EBS.regr1

diff.lim <- range(GOA.diff, EBS.diff) # limit for plotting


######################################


map('world2Hires', 'usa', fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)
map('world2Hires', 'Canada',fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)

plot.points <- mapproject(pink.runs$long, pink.runs$lat,projection=my.proj, orientation=my.orien) 
points(plot.points$x, plot.points$y, pch=21, bg ="red", cex=1)
box()  

mtext("a) Pink", adj=0)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(195,240), ylim=c(49,60),
    fill=FALSE, lforce="e")

map('world2Hires', 'usa', fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)
map('world2Hires', 'Canada',fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)

plot.points <- mapproject(sock.runs$long, sock.runs$lat,projection=my.proj, orientation=my.orien) 
points(plot.points$x, plot.points$y, pch=21, bg ="red", cex=1)
box()  
mtext("b) Sockeye", adj=0)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(195,240), ylim=c(49,60),
    fill=FALSE, lforce="e")

map('world2Hires', 'usa', fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)
map('world2Hires', 'Canada',fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)

plot.points <- mapproject(chum.runs$long, chum.runs$lat,projection=my.proj, orientation=my.orien) 
points(plot.points$x, plot.points$y, pch=21, bg ="red", cex=1)
box()  
mtext("c) Chum", adj=0)

# Re-shape mean SST data  to a matrix with latitudes in columns, longitudes in rows
# get mean value for each cell
coast.mean <- colMeans(coast.sst)
# turn into matrix for plotting
z <- t(matrix(coast.mean,length(y)))  
# and change to a set of polygons for projection
polys <- matrixPoly(x, y, z)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(193,235.5), ylim=c(35,65),
    fill=FALSE, lforce="e")

COLS <- val2col(z, col = tim.colors(64))
for(i in seq(polys)){
  tmp <- mapproject(polys[[i]],
                    proj=my.proj, parameters = NULL, orient=my.orien)
  polygon(tmp$x, tmp$y, col=COLS[i], border=COLS[i], lwd=0.1)
}

# make grid for cells used
plot.lat <- lat[keep]
plot.long <- lon[keep]

for(i in 1:length(plot.lat)){
  xgrid <- c(plot.long[i]-1, plot.long[i]+1, plot.long[i]+1, plot.long[i]-1, plot.long[i]-1)
  ygrid <- c(plot.lat[i]+1, plot.lat[i]+1, plot.lat[i]-1, plot.lat[i]-1, plot.lat[i]+1)
  proj.lines <- mapproject(xgrid, ygrid, projection=my.proj, orientation=my.orien) 
  lines(proj.lines$x, proj.lines$y, lwd=0.5)
}

map('world2Hires', 'usa', fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)
map('world2Hires', 'Canada',fill=T, add=T, 
    lwd=0.5, col="lightyellow3", proj=my.proj, parameters = NULL, orient=my.orien)
box()

# add legend strip
mt.cex <- 1.1
l.mar <- 3
l.cex <- 0.8
l.l <- 1.2
tc.l <- -0.2
image.plot(z, legend.only=TRUE, horizontal =TRUE,  legend.lab = "Mean annual SST (ºC)", 
           smallplot = c(0.25,0.78,0.2,0.23), 
           legend.cex=0.8,
           legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0))) 
mtext("d) SST data", adj=0)

dev.off()

######################################
# and a series of plots

# define projection details
my.proj <- "orthographic"
my.orien <- c(45,180,0)

png("figs/goa era1 slp-sst.png", 3, 3, units="in", res=300)

par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
    las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)

ylim <- c(40,90)

new.col <- oce.colorsPalette(64)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(0,350), ylim=c(20,90),
    fill=FALSE, lforce="e")

z <- GOA.regr1  # replace elements NOT corresponding to land with loadings!
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

mtext("SLP vs. GOA SST 1950-1988", adj=0.5, cex=0.8, side=1)

dev.off()

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

#######
png("figs/ebs era2 slp-sst.png", 3, 3, units="in", res=300)

par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
    las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)

ylim <- c(40,90)

new.col <- oce.colorsPalette(64)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(0,350), ylim=c(20,90),
    fill=FALSE, lforce="e")

z <- EBS.regr2  # replace elements NOT corresponding to land with loadings!
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

mtext("SLP vs. EBS SST 1989-2013", adj=0.5, cex=0.8, side=1)

dev.off()

##
# GOA diff
png("figs/GOA era diff slp-sst.png", 3, 3, units="in", res=300)

par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
    las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)

ylim <- c(40,90)

new.col <- oce.colorsPalette(64)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(0,350), ylim=c(20,90),
    fill=FALSE, lforce="e")

z <- GOA.diff  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))
polys <- matrixPoly(x, y, z)
COLS <- val2col(z, col = new.col, zlim=c(-diff.lim[2], diff.lim[2]))
for(i in seq(polys)){
  tmp <- mapproject(polys[[i]],
                    proj=my.proj, parameters = NULL, orient=my.orien)
  polygon(tmp$x, tmp$y, col=COLS[i], border=COLS[i], lwd=0.1)
}


map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan', 'Norway', 'Finland', 'Sweden', 'Denmark',
                     'India', 'Nepal', 'Vietnam', 'Iceland', 'Taiwan'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3", proj=my.proj, parameters = NULL, orient=my.orien)

mtext("GOA: (1989-2013)-(1951-1988)", adj=0.5, cex=0.8, side=1)

dev.off()
##
# EBS diff
png("figs/EBS era diff slp-sst.png", 3, 3, units="in", res=300)

par(mar=c(1,0,0,0),  tcl=tc.l, mgp=c(1.5,0.3,0), 
    las=1, cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)

ylim <- c(40,90)

new.col <- oce.colorsPalette(64)

map("world2Hires", 
    proj=my.proj, parameters = NULL, orient=my.orien,
    xlim=c(0,350), ylim=c(20,90),
    fill=FALSE, lforce="e")

z <- EBS.diff  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))
polys <- matrixPoly(x, y, z)
COLS <- val2col(z, col = new.col, zlim=c(-diff.lim[2], diff.lim[2]))
for(i in seq(polys)){
  tmp <- mapproject(polys[[i]],
                    proj=my.proj, parameters = NULL, orient=my.orien)
  polygon(tmp$x, tmp$y, col=COLS[i], border=COLS[i], lwd=0.1)
}

map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan', 'Norway', 'Finland', 'Sweden', 'Denmark',
                     'India', 'Nepal', 'Vietnam', 'Iceland', 'Taiwan'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3", proj=my.proj, parameters = NULL, orient=my.orien)

mtext("EBS: (1989-2013)-(1951-1988)", adj=0.5, cex=0.8, side=1)

dev.off()

#################
# old 'square' version
png("figs/goa-ebs slp-sst.png", 8, 6, units="in", res=300)
# setup the layout
mt.cex <- 1.1
l.mar <- 3
l.cex <- 0.6
l.l <- 0.2
tc.l <- -0.2


par(mar=c(0.5,0.5,1,2),  tcl=tc.l, mgp=c(1.5,0.3,0), 
    las=1, mfrow=c(3,2), cex.axis=0.8, cex.lab=0.8) # , oma=c(0,1,1,0)

ylim <- c(40,90)


new.col <- oce.colorsPalette(64)
# GOA first era
z <- GOA.regr1  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image.plot(x,y,z, col=new.col, zlim=c(lim[1], -lim[1]), ylim=ylim,
           xlab = "", ylab = "",yaxt="n", xaxt="n", legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0)))

contour(x,y,z, add=T, col="grey", drawlabels=F, lwd=0.5)

map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3")

mtext("SLP vs. GOA SST 1950-1988", adj=0.5, cex=0.8)

# GOA second era
z <- GOA.regr2  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image.plot(x,y,z, col=new.col, zlim=c(lim[1], -lim[1]), ylim=ylim,
           xlab = "", ylab = "",yaxt="n", xaxt="n", legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0)))

contour(x,y,z, add=T, col="grey", drawlabels=F, lwd=0.5)

map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3")

mtext("SLP vs. GOA SST 1989-2013", adj=0.5, cex=0.8)

# EBS first era
z <- EBS.regr1  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image.plot(x,y,z, col=new.col, zlim=c(lim[1], -lim[1]), ylim=ylim,
           xlab = "", ylab = "",yaxt="n", xaxt="n", legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0)))

contour(x,y,z, add=T, col="grey", drawlabels=F, lwd=0.5)

map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3")

mtext("SLP vs. EBS SST 1950-1988", adj=0.5, cex=0.8)

# EBS second era
z <- EBS.regr2  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image.plot(x,y,z, col=new.col, zlim=c(lim[1], -lim[1]), ylim=ylim,
           xlab = "", ylab = "",yaxt="n", xaxt="n", legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0)))

contour(x,y,z, add=T, col="grey", drawlabels=F, lwd=0.5)

map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3")

mtext("SLP vs. EBS SST 1989-2013", adj=0.5, cex=0.8)

# difference first era
z <- era1.diff  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image.plot(x,y,z, col=new.col, zlim=c(-1.7,1.7), ylim=ylim,
           xlab = "", ylab = "",yaxt="n", xaxt="n", legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0)))

contour(x,y,z, add=T, col="grey", drawlabels=F, lwd=0.5)

map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3")

mtext("GOA-EBS 1950-1988", adj=0.5, cex=0.8)

# difference second era
z <- era2.diff  # replace elements NOT corresponding to land with loadings!
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image.plot(x,y,z, col=new.col, zlim=c(-1.7,1.7), ylim=ylim,
           xlab = "", ylab = "",yaxt="n", xaxt="n", legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0)))

contour(x,y,z, add=T, col="grey", drawlabels=F, lwd=0.5)

map('world2Lores', c('Canada', 'usa', 'USSR', 'Mexico', 'China', 'Mongolia', 'Greenland',
                     'South Korea', 'North Korea', 'Japan', 'Finland', 'Norway', 'Denmark', 'Sweden', 'Ukraine'), 
    fill=T,add=T, lwd=0.5, col="darkgoldenrod3")

mtext("GOA-EBS 1989-2013", adj=0.5, cex=0.8)

dev.off()

