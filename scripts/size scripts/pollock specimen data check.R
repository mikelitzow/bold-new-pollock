library(tidyverse)
theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

d1 <- read.csv("data/survey data/Poll_LW_BS_GOA.csv")

head(d1)
tail(d1)
unique(d1$SPECIES_CODE)

d2 <- read.csv("data/survey data/poll_specimen_haul.csv")

head(d2)
unique(d2$REGION)
unique(d2$SPECIES_CODE)
unique(d2$SURVEY)

# check region, year, and sample size for d1
check.d1 <- d1 %>%
  mutate(YEAR=floor(CRUISE/100)) %>%
  group_by(REGION, YEAR) %>%
  summarise(count=n())
View(check.d1)
# these are for EBS and GOA for 1975-2018

# check survey area, year, and sample size for d2
check.d2 <- d2 %>%
  mutate(YEAR=floor(CRUISE/100)) %>%
  group_by(SURVEY, YEAR) %>%
  summarise(count=n())
check.d2
# these are EBS/GOA for 2019, and NBS for all years to date (2010, 2017-2019)


d1 <- d1 %>%
  mutate(YEAR=floor(CRUISE/100)) %>%
  filter(REGION=="BS") %>%
  select(YEAR, HAULJOIN, SPECIMENID, LENGTH, WEIGHT, AGE) 

d2 <- d2 %>%
  mutate(YEAR=floor(CRUISE/100)) %>%
  filter(SURVEY=="EBS") %>%
  select(YEAR, HAULJOIN, SPECIMENID, LENGTH, WEIGHT, AGE)

# confirm that there are no hauls in both data sets

intersect(d1$HAULJOIN, d2$HAULJOIN)

# looks good!

data <- rbind(d1, d2)

# add length bins

summary <- data %>%
  mutate(bin_cm = 
           case_when(
             LENGTH %in% 100:200 ~ "10-20",
             LENGTH %in% 201:300 ~ "20-30",
             LENGTH %in% 301:400 ~ "30-40",
             LENGTH %in% 401:500 ~ "40-50",
             LENGTH %in% 501:600 ~ "50-60",
             LENGTH %in% 601:700 ~ "60-70",
             LENGTH %in% 701:800 ~ "70-80"), 
         L_W = if_else(!is.na(LENGTH)==T & !is.na(WEIGHT)==T, 1, 0)) %>%
  filter(L_W==1)


ff <- function(x) sum(!is.na(x))
year.table <- as.data.frame(tapply(summary$LENGTH, list(summary$YEAR, summary$bin_cm), ff))

# load confirmation data query to check these are complete
dat2 <- read.csv(("./data/survey data/pollock_survey_specimen_data_confirmation.csv"))

nrow(dat2); nrow(data) # many more for dat2!

dat2 <- dat2 %>%
  mutate(YEAR=floor(CRUISE/100))

summary2 <- dat2 %>%
  mutate(bin_cm = 
           case_when(
             LENGTH %in% 100:200 ~ "10-20",
             LENGTH %in% 201:300 ~ "20-30",
             LENGTH %in% 301:400 ~ "30-40",
             LENGTH %in% 401:500 ~ "40-50",
             LENGTH %in% 501:600 ~ "50-60",
             LENGTH %in% 601:700 ~ "60-70",
             LENGTH %in% 701:800 ~ "70-80"), 
         L_W = if_else(!is.na(LENGTH)==T & !is.na(WEIGHT)==T, 1, 0)) %>%
  filter(L_W==1)


ff <- function(x) sum(!is.na(x))
year.table2 <- as.data.frame(tapply(summary2$LENGTH, list(summary2$YEAR, summary2$bin_cm), ff))

## combine and plot to compare

year.table$data.set <- "original"
year.table2$data.set <- "data.check"

year.table$year <- as.numeric(row.names(year.table))
year.table2$year <- as.numeric(row.names(year.table2))

                             
compare <- rbind(year.table, year.table2) %>%
  pivot_longer(cols = c(-year, -data.set), names_to = "bin")


ggplot(compare, aes(year, value, color=data.set)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=cb[c(2,6)]) +
  facet_wrap(~bin, scales="free_y")

ggsave("./data/survey data/pollock survey size data check by year.png", width=8, height=6, units='in')

ggplot(compare, aes(year, value, color=data.set)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=cb[c(2,6)]) +
  facet_wrap(~bin, scales="free_y")

ggsave("./data/survey data/pollock survey size data check by year.png", width=8, height=6, units='in')

compare <- compare %>%
  pivot_wider(names_from = data.set, values_from = value)

ggplot(compare, aes(original, data.check)) +
  geom_text(aes(label=year), size=3) +
  facet_wrap(~bin, scales="free")

ggsave("./data/survey data/pollock survey size data check by year scatter.png", width=8, height=6, units='in')
