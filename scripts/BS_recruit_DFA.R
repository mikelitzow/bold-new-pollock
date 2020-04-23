#============================================================================================================================================
# DFA on EBS recruitment data

#Created by Krista, April 2020
#Based on "BS_climate_DFA.R"
#============================================================================================================================================
#Notes:
#============================================================================================================================================
library(ggplot2)
library(reshape2)
library(dplyr)
library(pracma)
library(MARSS)
library(tidyr)
# devtools::install_github("kassambara/ggpubr")
library(ggpubr)
library(cowplot)
library(gridExtra)
library(broom)
library(lemon)
library(MuMIn)
library(lmtest)
library(cowplot)

#Get data plot data =======

# load environmental data
rec_dat <- read.csv("data/EBS.recruit.time.series.csv")

# plot TS for SI
do.rec.dat <- as.data.frame(scale(rec_dat[,-1])) # scale to plot on 1 axis
do.rec.dat$year <- rec_dat$year
plot.rec.dat <- gather(do.rec.dat, key=key, value=value, -year)


ggplot(plot.rec.dat, aes(x=year, y=value)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~key) +
  ylab("Standard anomaly") + xlab("") + theme_bw() + geom_hline(yintercept = 0)

raw.rec.dat <- gather(rec_dat, key=species, value=recruitment, -year)

ggplot(raw.rec.dat, aes(x=year, y=recruitment)) + geom_point() + geom_line() +
  facet_wrap(~species, scales="free") +
  ylab("Recruitment") + xlab("") + theme_bw()

ggplot(raw.rec.dat, aes(x=year, y=recruitment)) + geom_point() + geom_smooth() +
  facet_wrap(~species, scales="free") +
  ylab("Recruitment") + xlab("") + theme_bw() 

#log transform====

log_rec <- as.data.frame(cbind(rec_dat$year, log(rec_dat$YLFN.R.age1.18.1a), log(rec_dat$TRBT.Age.0.R.16.1b),
                 log(rec_dat$FHSL.R.age0), log(rec_dat$Opilio.age0.recruits),
                 log(rec_dat$cod.age0.R), log(rec_dat$pollock.age0.R)))
#set col names
log_rec <- log_rec %>% 
  rename(
    year = V1,
    YLFN.R.age1.18.1a = V2,
    TRBT.Age.0.R.16.1b = V3,
    FHSL.R.age0 = V4,
    Opilio.age0.recruits = V5,
    cod.age0.R = V6,
    pollock.age0.R = V7
  )


log.rec.dat <- gather(log_rec, key=species, value=log_recruitment, -year)

ggplot(log.rec.dat, aes(x=year, y=log_recruitment)) + geom_point() + geom_line() +
  facet_wrap(~species, scales="free") +
  ylab("log(Recruitment)") + xlab("") + theme_bw()



#manupulate data for DFA========


# save the full data set for later use...
log.rec.mat <- t(as.matrix(log_rec))
colnames(log.rec.mat) <- log.rec.mat[1,]
log.rec.mat <- log.rec.mat[-1,]


#fit model========

# now fit DFA models with 1-3 trends and different error structures and compare

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1:4) {  # allowing up to 3 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(log.rec.mat, model=dfa.model, control=cntl.list,
                 form="dfa", z.score=TRUE)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)
  } # end m loop
} # end R loop

# calculate delta-AICc scores, sort in descending order, and compare
model.data$dAICc <- model.data$AICc-min(model.data$AICc)
model.data <- model.data %>%
  arrange(dAICc)
model.data










