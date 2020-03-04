library(ncdf4)
library(maps)
library(maptools)
library(mapdata)
library(fields)

# download various climate data sets!

################
# download NCEP/NCAR
################

# downloading global NCEP/NCAR slp

  URL <- 
    "https://upwell.pfeg.noaa.gov/erddap/griddap/noaa_esrl_118e_d5aa_117b.nc?slp[(1948-01-01):1:(2019-12-01)][(90.0):1:(20)][(0.0):1:(357.5)]"
  
  download.file(URL, "data/NCEP.NCAR.slp.nc")

# and test
test <- nc_open("data/NCEP.NCAR.slp.nc")
test

x <- ncvar_get(test, "longitude")
y <- ncvar_get(test, "latitude")
slp <- ncvar_get(test, "slp", verbose = F)
dim(slp) # 144 long, 29 lat, 864 months

# need to reverse latitude for plotting!
y <- rev(y)
slp <- slp[,29:1,]

# Change data into a matrix with months / cells for rows / columns
slp <- aperm(slp, 3:1)  
slp <- matrix(slp, nrow=dim(slp)[1], ncol=prod(dim(slp)[2:3]))  

z <- colMeans(slp, na.rm=T) # mean value for each cell
z <- t(matrix(z, length(y)))  # Convert vector to matrix and transpose for plotting
image(x,y,z, col=tim.colors(64), xlab = "", ylab = "", yaxt="n", xaxt="n")

contour(x,y,z, add=T, col="white",vfont=c("sans serif", "bold"))
map('world2Hires', add=T, lwd=1)
# looks good!