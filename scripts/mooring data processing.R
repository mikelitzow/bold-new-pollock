library(ncdf4)
library(tidyverse)

# load, process, combine M2 files!!

# get a list of files

files <- list.files("data/M2 mooring data")

# drop the readme file!

drop <- grep("Readme", files)

files <- files[-drop]

# loop through each and see if we can combine! 
# :)

for(i in 1:length(files)){
  
  i <- 1
  
  path <- paste("data/M2 mooring data/", files[i], sep="")
  nc <- nc_open(path)
  
  summary <- str(print(nc))
  
 str(nc)
  
}