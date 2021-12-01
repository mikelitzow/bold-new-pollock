library(tidyverse)

dat <- read.csv("./data/climate data.csv")

head(dat)

dat <- dat %>%
  select(year, south.sst.ndjfm, south.sst.amj, north.sst.ndjfm, north.sst.amj) %>%
  pivot_longer(cols = -year)

ggplot(dat, aes(year, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~name, ncol=2)

dat$season <- NA

dat$season[which(dat$name=="south.sst.ndjfm" | dat$name=="north.sst.ndjfm")] <- "winter"
dat$season[which(dat$name=="south.sst.amj" | dat$name=="north.sst.amj")] <- "spring"

dat$region <- NA

dat$region[which(dat$name=="south.sst.ndjfm" | dat$name=="south.sst.amj")] <- "south"
dat$region[which(dat$name=="north.sst.ndjfm" | dat$name=="north.sst.amj")] <- "north"


ggplot(dat, aes(year, value, colour=region)) +
  geom_line() +
  geom_point() +
  facet_wrap(~season) + ylab("SST") + xlab("Year") + theme_bw() +
  theme(legend.position = c(0.75, 0.9), legend.direction = "horizontal") +
  scale_colour_manual(values=c("black", "dark grey"))
