# download and process PDO/NPGO indices!
library(tidyverse)
# load pdo
download.file("http://jisao.washington.edu/pdo/PDO.latest", "data/pdo") # uncomment to load
names <- read.table("data/pdo", skip=30, nrows=1, as.is = T)
pdo <- read.table("data/pdo", skip=31, nrows=119, fill=T, col.names = names)
pdo$YEAR <- 1900:(1899+nrow(pdo)) # drop asterisks!
pdo <- pdo %>%
  gather(month, value, -YEAR) %>% # not loading tidyr because I think it conflicts with maps!
  arrange(YEAR)

# load npgo
download.file("http://www.oces.us/npgo/npgo.php", "data/npgo") # uncomment to load
npgo <- read.table("data/npgo", skip=10, nrows=831, fill=T, col.names = c("Year", "month", "value"))

# but, for now we will use this version with recent values of the PDO estimated through PCA on ERSST data

pdo <- read.csv("data/estimated PDO through May 2019.csv")

# get annual winter means for each and save!
pdo$winter.year <- ifelse(pdo$month %in% c("Nov", "Dec"), pdo$year+1, pdo$year)

pdo <- pdo %>%
  filter(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar"))

pdo <- tapply(pdo$real.pdo, pdo$winter.year, mean)  

plot(names(pdo), pdo, type="l") # looks good!

# now npgo
npgo$winter.year <- ifelse(npgo$month %in% 11:12, npgo$Year+1, npgo$Year)

npgo <- npgo %>%
  filter(month %in% c(11,12,1:3))

npgo <- tapply(npgo$value, npgo$winter.year, mean)  

plot(names(npgo), npgo, type="l") # looks good!

# and save for other uses!
that <- data.frame(year=1951:2019,
                   pdo.ndjfm=pdo[names(pdo) %in% 1951:2019],
                   npgo.ndjfm=npgo[names(npgo) %in% 1951:2019])

write.csv(that, "data/winter pdo-npgo.csv", row.names = F)
