library(tidyverse)

# summarize Aleutian Low - Bering Sea Anticyclone index for spring (MAM) and winter (DJF)

# ref is Cox et al. 2019 GRL: 
# The Aleutian Low‐Beaufort Sea Anticyclone: A ClimateIndex Correlated With the Timing of Springtime
# Melt in the Pacific Arctic Cryosphere

# data from https://psl.noaa.gov/data/timeseries/ALBSA/

albsa <- read.csv("data/albsa.ncepr1.csv")

albsa <- albsa %>%
  separate(Date, c("month", "day", "year"), "/")

albsa$month <- as.numeric(albsa$month)
albsa$year <- as.numeric(albsa$year)

albsa$winter.year <- ifelse(albsa$month %in% 11:12, albsa$year+1, albsa$year)

spring <- albsa %>%
  filter(month %in% 3:5) %>%
  group_by(year) %>%
  summarise(albsa.mam=mean(ALBSA))

winter <- albsa %>%
  filter(month %in% c(12,1,2)) %>%
  group_by(winter.year) %>%
  summarise(albsa.djf=mean(ALBSA))
names(winter)[1] <- "year"

export <- left_join(spring, winter)

export <- export %>%
  filter(year %in% 1949:2019)

ggplot(export, aes(albsa.djf, albsa.mam)) +
  theme_bw() +
  geom_point()

# quite independent!

write.csv(export, "data/monthly albsa.csv", row.names = F)
