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
