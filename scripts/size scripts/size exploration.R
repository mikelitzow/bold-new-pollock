library(tidyverse)
theme_set(theme_bw())

d1 <- read.csv("data/survey data/Poll_LW_BS_GOA.csv")

head(d1)
tail(d1)
unique(d1$SPECIES_CODE)

d2 <- read.csv("data/survey data/poll_specimen_haul.csv")

head(d2)
unique(d2$REGION)
unique(d2$SPECIES_CODE)
unique(d2$SURVEY)

d1 <- d1 %>%
  filter(REGION=="BS") #%>%
  #select(HAULJOIN, SPECIMENID, LENGTH, WEIGHT, AGE) #drop this for now, want to retain all cols

d1$df <- "d1"
d1$SEX <- NA

d2 <- d2 %>%
  filter(SURVEY=="EBS") #%>%
  #select(HAULJOIN, SPECIMENID, LENGTH, WEIGHT, AGE) #drop this for now, want to retain all cols

d2$df <- "d2"

d2 <- d2[,-1] #drop col 'survey'
names(d2)

d2 <- d2 %>% rename(BOTTOM_TEMP = BOTTOM_TEMPERATURE)

# confirm that there are no hauls in both data sets

intersect(d1$HAULJOIN, d2$HAULJOIN)
# looks good!

d1 <- d1[,-4] #drop column vessel
d2 <- d2[,-6] #drop column stationID
d2 <- d2[,-4] #drop column stratum


sizedata <- rbind(d1, d2) 

sizedata$CRUISE2 <- sizedata$CRUISE
sizedata <- sizedata %>% separate(CRUISE, c("YEAR", "Cruisenum"), sep=4)
sizedata$YEAR <- as.numeric(sizedata$YEAR)


ggplot(sizedata, aes(YEAR, LENGTH)) + 
        geom_point()+  geom_smooth() + facet_wrap(~as.factor(AGE), scales = "free")

ggplot(sizedata[which(sizedata$AGE==1),], aes(YEAR, LENGTH)) + 
  geom_point()+  geom_smooth() + facet_wrap(~as.factor(AGE), scales = "free")
#the age 1 data looks very oddly regular for some reason? Others don't

ggplot(sizedata[which(sizedata$AGE==3|
                        sizedata$AGE==6|
                        sizedata$AGE==9|
                        sizedata$AGE==12|
                        sizedata$AGE==15|
                        sizedata$AGE==18),], aes(YEAR, LENGTH)) + 
  geom_point()+  geom_smooth() + facet_wrap(~as.factor(AGE), scales = "free")


ggplot(sizedata[which(sizedata$AGE==2|
                        sizedata$AGE==4|
                        sizedata$AGE==6|
                        sizedata$AGE==8|
                        sizedata$AGE==10|
                        sizedata$AGE==12|
                        sizedata$AGE==14|
                        sizedata$AGE==16),], aes(YEAR, LENGTH)) + 
  geom_point()+  geom_smooth() + facet_wrap(~as.factor(AGE), scales = "free", nrow=2) + xlab("Year") + ylab("Body length (mm)")

tt <- read.csv("data/survey data/poll_cpue_by_sex_cm.csv")
head(tt)
tail(tt)

hist(sizedata$LENGTH)
hist(sizedata$AGE)
hist(sizedata$WEIGHT) #a lot of ones!
table(sizedata$LENGTH)
table(sizedata$AGE)
table(sizedata$WEIGHT)
nrow(sizedata)

ggplot(sizedata, aes(LENGTH)) + geom_histogram(binwidth = 10)

ggplot(sizedata, aes(LENGTH)) + geom_histogram(binwidth = 10) + facet_wrap(~CRUISE)


ggplot(sizedata, aes(WEIGHT)) + geom_histogram(binwidth = 10)

ggplot(sizedata, aes(WEIGHT)) + geom_histogram(binwidth = 10) + facet_wrap(~CRUISE)


ggplot(sizedata, aes(AGE)) + geom_histogram(binwidth = 1)

ggplot(sizedata, aes(AGE)) + geom_histogram(binwidth = 1) + facet_wrap(~CRUISE)

ggplot(sizedata, aes(LENGTH, WEIGHT, col=as.factor(AGE))) + geom_point() + facet_wrap(~CRUISE)

ggplot(sizedata[which(sizedata$CRUISE>198200),], aes(LENGTH, WEIGHT, col=as.factor(AGE))) + geom_point() 

ggplot(sizedata[which(sizedata$CRUISE>198200),], aes(LENGTH, WEIGHT, col=as.factor(AGE))) + 
  geom_point() + facet_wrap(~CRUISE)


table(sizedata$YEAR)


#hmm outlier that does not look possible (medium length, highest weight recorded)

ggplot(d1, aes(LENGTH, WEIGHT, col=as.factor(AGE))) + geom_point()
#oh even bigger outlier
ggplot(d1, aes(LENGTH, WEIGHT, col=as.factor(REGION))) + geom_point()
ggplot(d1, aes(LENGTH, WEIGHT, col=as.factor(CRUISE))) + geom_point()

table(d1$CRUISE)
table(d1$CRUISE, d1$AGE)

ggplot(d2, aes(LENGTH, WEIGHT, col=as.factor(AGE))) + geom_point()

ggplot(d2, aes(LENGTH, WEIGHT, col=as.factor(AGE))) + geom_point() + facet_wrap(~CRUISE)

# limit to 10-20 cm

data <- data %>%
  filter(LENGTH >= 100 & LENGTH <= 200)

sum.cpue <- tt %>%
  filter(LENGTH >= 100 & LENGTH <= 200) %>%
  group_by(HAULJOIN) %>%
  summarise(sum.cpue=sum(NUMCPUE_LENGTH)) #need NA RM here?

data <- left_join(data, sum.cpue)


ggplot(data, aes(LENGTH, WEIGHT)) +     
  geom_point() +
  geom_smooth(method="gam")

# outlier!
arrange(data, desc(WEIGHT))
# remove that one...
data <- data %>%
  filter(WEIGHT<100)

ggplot(data, aes(LENGTH, WEIGHT)) + 
  geom_point() +
  geom_smooth(method="gam")

# look at strata by year
ff <- function(x) sum(!is.na(x))
strata <- tapply(tt$HAUL, list(tt$YEAR, tt$STRATUM), ff)
strata  

# add station, stratum, year
xtra <- tt %>%
  select(YEAR, STRATUM, STATIONID, HAULJOIN)   

data <- left_join(data, xtra)  #this is causing massive duplication!

unique(data$YEAR)

# exclude data without years...
keep <- !is.na(data$YEAR)    #this would drop hundreds of rows!
data <- data[keep,]

# and fit a gam
mod <- mgcv::gam(WEIGHT ~ s(LENGTH, k=4), data=data)
summary(mod)
mgcv::plot.gam(mod, resid=T, pch=1, se=T, shade=T)

data$RESIDUAL.WEIGHT <- resid(mod)

# missing cpue seems to be causing a problem - drop for now
look <- is.na(data$sum.cpue)
View(data[look,])
data <- data[!look,]

# get average residual for each haul
haul.mean <- data %>%
  group_by(HAULJOIN) %>%
  summarise(mean.residual=mean(RESIDUAL.WEIGHT),
            year=mean(YEAR),
            sum.cpue=mean(sum.cpue))

annual.mean <- plyr::ddply(haul.mean, "year", function(x) {
  mu <- weighted.mean(x$mean.residual, log(x$sum.cpue))
  data.frame(mu=mu)
}
  )

# load climate data
clim.dat <- read.csv("data/climate data.csv")

ggplot(clim.dat, aes(south.sst.ndjfm, south.sst.amj)) + 
  geom_point()

annual.mean$sst.ndjfm <- clim.dat$south.sst.ndjfm[match(annual.mean$year, clim.dat$year)] 
annual.mean$era <- as.factor(ifelse(annual.mean$year < 2014, 1, 2))
annual.mean$sst.amj <- clim.dat$south.sst.amj[match(annual.mean$year, clim.dat$year)] 
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(annual.mean, aes(sst.ndjfm, mu, color=era)) +
  geom_point()

ggplot(annual.mean, aes(sst.amj, mu, color=era)) +
  geom_point() 

mod <- mgcv::gam(mu ~ s(sst.amj, k=4), data=annual.mean)
summary(mod)

plot(mod, se=T, shade=T, resid=T, pch=1)

# try fitting a model to lat/long as well