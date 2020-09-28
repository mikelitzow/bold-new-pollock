library(tidyverse)
library(zoo)

# for GOA, restrict to the time series known to show changing associations with PDO
dat <- read.csv("data/EBS.recruit.time.series.csv")

dat2 <- read.csv("data/winter pdo-npgo.csv")

dfa.dat <- left_join(dat, dat2)

dat3 <- read.csv("data/climate data.csv")
dat3 <- dat3 %>%
  select(year, AO.jfm)

dfa.dat <- left_join(dfa.dat, dat3)

# rolling 3-yr means for modes!
dfa.dat$pdo.ndjfm <- rollmean(dfa.dat$pdo.ndjfm, 3, fill = NA)
dfa.dat$npgo.ndjfm <- rollmean(dfa.dat$npgo.ndjfm, 3, fill = NA)
dfa.dat$AO.jfm <- rollmean(dfa.dat$AO.jfm, 3, fill = NA)


dfa.dat <- dfa.dat %>%
  pivot_longer(cols=c(-year, -pdo.ndjfm, -npgo.ndjfm, -AO.jfm))


# get rolling 25-yr correlations

cor <- data.frame()
vars <- unique(dfa.dat$name)

for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2000){
    # i <- 1990
    cor <- rbind(cor,
                     data.frame(year=i,
                                var=vars[j],
                                mode="pdo.ndjfm",
                                cor=cor(temp$pdo.ndjfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}

# and npgo
for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2000){
    # i <- 1990
    cor <- rbind(cor,
                     data.frame(year=i,
                                var=vars[j],
                                mode="npgo.ndjfm",
                                cor=cor(temp$npgo.ndjfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}

# and ao!
for(j in 1:length(vars)){
  # j <- 1
  temp <- dfa.dat %>%
    filter(name==vars[j])
  
  for(i in 1963:2000){
    # i <- 1990
    cor <- rbind(cor,
                     data.frame(year=i,
                                var=vars[j],
                                mode="AO.jfm",
                                cor=cor(temp$AO.jfm[temp$year %in% (i-12):(i+12)],
                                        temp$value[temp$year %in% (i-12):(i+12)])))
    
    
  }}


# now, restrict to correlation time series with 
# absolute values >= 0.5 for at least 1 25-yr window!

ff <- function(x) max(abs(x), na.rm=T)>=0.5

cor$var.mode <- paste(cor$var, cor$mode, sep=".")

cor <- plyr::ddply(cor, "var.mode", mutate, keep = ff(cor))


# rename with plot-friendly names!
cor$mode <- ifelse(cor$mode=="AO.jfm", "AO",
                       ifelse(cor$mode=='pdo.ndjfm', "PDO", "NPGO"))

cor$mode.order <- ifelse(cor$mode=="PDO", 1,
                             ifelse(cor$mode=="NPGO", 2, 3))

cor$mode <- reorder(cor$mode, cor$mode.order)

# goa.cor$var <- ifelse(goa.cor$var=="NDJ.grad", "SLP gradient",
#                       ifelse(goa.cor$var=="FMA.FW", "Freshwater discharge",
#                              ifelse(goa.cor$var=="FMA.WS", "Wind stress",
#                                     ifelse(goa.cor$var=="Papa", "Papa advection", "SSH"))))


# remove Opilio and cod, which are so short...
restricted.cor <- cor %>%
  filter(var!="Opilio.age0.recruits") %>%
  filter(var!="cod.age0.R")

ggplot(filter(restricted.cor, keep==TRUE), aes(year, cor, color=var)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~mode, scales="free_y") +
  xlab("Center of 25-year window") +
  ylab("Pearson's correlation") +
  geom_vline(xintercept = 1988.5, lty=2) +
  geom_hline(yintercept = 0, color="gray") +
  scale_color_manual(values=cb[c(2,3,4,6,7)]) +
  theme(legend.title = element_blank())


 ggsave("figs/rolling correlations - EBS recruit TS and modes.png", width=10, height=3, units='in')
