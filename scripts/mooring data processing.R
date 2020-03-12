library(ncdf4)
library(tidyverse)
library(chron)

# load, process, combine M2 files!!

# get a list of files

files <- list.files("data/M2 mooring data")

# drop the readme file!

drop <- grep("Readme", files)

files <- files[-drop]

# loop through each and see if we can combine! 
# :)

# object to combine all files!
all.files <- data.frame()

for(i in 1:length(files)){
  
  # i <- 1
  
  path <- paste("data/M2 mooring data/", files[i], sep="")
  
  # load file
  nc <- nc_open(path)
  
  # get variable names
  vars <- names(nc$var)
  
  # extract dates
  raw <- ncvar_get(nc, "time") # dates since 1-1-1900
  date <- dates(raw, origin = c(1,1,1900))
  
  # extract depth
  depth <- ncvar_get(nc, "depth")
  
  # just to be sure/check, extract lat/long
  lat <- ncvar_get(nc, "lat")
  long <- ncvar_get(nc, "lon")

  vars <- names(nc$var)
  
  # attributes(nc)$names
  
  # dates_df %>% 
  #   separate(date, c("month", "day", "year"), "/")
  
  # now loop through each variable
  
  # set up an object to capture summarized data from this file
  this.file <- data.frame()
  
  for(j in 1:length(vars)){ 
    
    # j <- 1
    # choose <- paste()
    # var <- ncvar_get(nc, nc$var[j], verbose = F)
    
    var <- ncvar_get(nc, vars[j], verbose = F)
    
    meta.data <- ncatt_get(nc, attributes(nc$var)$names[j])
    
    # so we'll put together a data frame with daily means and sample size 
    
    temp.dat <- data.frame(dat=var,
                           date=date) %>%
      group_by(date) %>%
      summarise(daily.mean=mean(dat), n=n()) %>%
      separate(date, c("month", "day", "year"), "/") 
    
    # add identifying meta-data
    
    temp.dat$name <- meta.data$name
    temp.dat$long.name <- meta.data$long_name
    temp.dat$generic.name <- meta.data$generic_name
    temp.dat$units <- meta.data$units
    temp.dat$depth <- depth
    temp.dat$lat <- lat
    temp.dat$long <- long

    this.file <- rbind(this.file, temp.dat)
  
}
  
  all.files <- rbind(all.files, this.file)
  
}