library(tidyverse)

# combine AO, PDO, NPGO (winter values) in one data frame
dat1 <- read.csv("data/climate data.csv")

head(dat1)

dat2 <- read.csv("data/winter pdo-npgo.csv")

dat1 <- dat1 %>%
  select(year, AO.jfm)

dat <- left_join(dat1, dat2)

# 11-yr rolling correlations

correlations <- as.data.frame(matrix(nrow=69-25, ncol=3))
colnames(correlations) <- c("PDO-NPGO", "PDO-AO", "NPGO-AO")


for(i in 1951:1995){
  # i <- 1951
  
  temp <- dat[dat$year %in% i:(i+24),]
  
  correlations[(i-1950),1] <- cor(temp[,2], temp[,3])
  correlations[(i-1950),2] <- cor(temp[,1], temp[,2])
  correlations[(i-1950),3] <- cor(temp[,1], temp[,3])
}

correlations$year <- 1963:2007
correlations <- correlations %>%
  pivot_longer(cols=-year)

# load colorblind palette 
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(correlations, aes(year, value)) +
  facet_wrap(~name, nrow=3) + geom_hline(yintercept = 0) + theme_bw() +
  geom_line(color=cb[2]) + 
  xlab("") + ylab("Correlation") + theme(axis.title.x = element_blank())

ggsave("figs/rolling 25-yr correlations pdo npgo ao.png", width=6, height=6, units='in')
# PDO-NPGO doesn't look right! need to check