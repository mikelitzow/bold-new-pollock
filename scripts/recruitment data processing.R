library(tidyverse)

# combine recruitment time series

d1 <- read.csv("data/turbot.recruitment.csv")
d2 <- read.csv("data/yellowfin.recruitment.csv")
d3 <- read.csv("data/flathead.recruitment.csv")
d4 <- read.csv("data/snow crab.csv")
d5 <- read.csv("data/Pcod.recruitment.csv")
d6 <- read.csv("data/2019.SSB.recruitment.csv")

names(d1)[1] <- names(d2)[1] <- names(d3)[1] <- names(d4)[1] <- names(d5)[1] <- names(d6)[1] <- "year"

# reduce each one to year and recruitment TS only...
head(d1)
d1 <- d1 %>%
  select(year, TRBT.Age.0.R.16.1b)

head(d2)
d2 <- d2 %>%
  select(year, YLFN.R.age1.18.1a)

# and lag year by one to match with cohort year
# i.e., 1954 age1 == 1953 age 0!

d2$year <- d2$year-1

head(d3)
d3 <- d3 %>%
  select(year, FHSL.R.age0)

head(d4)
d4 <- d4 %>%
  select(year, recruits)
names(d4)[2] <- "Opilio.age0.recruits"

# and lag 5 years per conversation with Cody Szuwalski
d4$year <- d4$year-5

head(d5)
d5 <- d5 %>%
  select(year, cod.age0.R)

head(d6)
d6 <- d6 %>%
  select(year, Recruitment)
names(d6)[2] <- "pollock.age0.R"
# and lag!
d6$year <- d6$year-1

range(d1$year, d2$year, d3$year, d4$year, d5$year, d6$year)

recruits <- left_join(d2, d1)
recruits <- left_join(recruits, d3)
recruits <- left_join(recruits, d4)
recruits <- left_join(recruits, d5)
recruits <- left_join(recruits, d6)

View(recruits)

# save
write.csv(recruits, "data/EBS.recruit.time.series.csv", row.names = F)
