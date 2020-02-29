library(ncdf4)
library(maps)
library(maptools)
library(mapdata)
library(fields)

# download various climate data sets!

################
# download NCEP/NCAR
################

# downloading global NCEP/NCAR slp from http://apdrc.soest.hawaii.edu/las/v6/dataset?catitem=16960
# (loading the whole globe b/c I'm having trouble limiting to the northern hemisphere!)

slp.raw <- nc_open("data/NCEP.NCAR.slp.nc")

# identify latest year and month needed
# for now I'll just query through the end of 2019....will be simpler to process whole years!
year <- 2019
month <- "12"
query <- c("e025_be03_a4bb.nc?vflx",
           "803d_41d9_b553.nc?uflx",
           "f19d_3925_d70b.nc?slp")

variable <- c("vflx", "uflx", "slp")

for(i in 1:length(query)){
  i <- 1
  URL <- paste("http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_", query[i], "[(1948-01-01):1:(", year, "-",
               month, "-01T00:00:00Z)][(19.99970054626):1:(69.52169799805)][(120):1:(249.375)]", sep="")
  
  download.file(URL, paste("data/North.Pacific.NCEP.NCAR.", variable[i], sep=""))
}

# and test
test <- nc_open("data/North.Pacific.NCEP.NCAR.vflx")
test

x <- ncvar_get(test, "longitude")
y <- ncvar_get(test, "latitude")
z <- ncvar_get(test, "uflx", verbose = F)
# Change data from a 3-D array to a matrix of monthly data by grid point:
# First, reverse order of dimensions ("transpose" array)
z <- aperm(z, 3:1)  

# Change to matrix with column for each grid point, rows for monthly means
z <- matrix(z, nrow=dim(z)[1], ncol=prod(dim(z)[2:3]))  

z <- colMeans(z, na.rm=T)
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
#image.plot(x,y,z, col=new.col, zlim=c(lim[1], -lim[1]), ylim=c(20,68),
#           xlab = "", ylab = "", yaxt="n", xaxt="n", legend.mar=l.mar, legend.line=l.l, axis.args=list(cex.axis=l.cex, tcl=tc.l, mgp=c(3,0.3,0)))
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", yaxt="n", xaxt="n")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)