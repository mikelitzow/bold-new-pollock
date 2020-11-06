#stats on data binned by length

#using binmeta from join_CPUEdat_w_sizeCPUEdat.R
library(ggplot2)
library(sjPlot)

binmeta_clean$log_sum_WGTCPUE_LEN <- log(binmeta_clean$sum_wgtCPUE_len + 1)


binmeta_clean$STATION <- as.factor(binmeta_clean$STATION)
binmeta_clean$VESSEL <- as.factor(binmeta_clean$VESSEL)
binmeta_clean$CRUISE <- as.factor(binmeta_clean$CRUISE)
binmeta_clean$HAUL <- as.factor(binmeta_clean$HAUL)
binmeta_clean$bin <- as.factor(binmeta_clean$bin)

##exclusion criteria===============

#how to exclude stations w few obs? Maybe n in a single bin? yeah do once split out


bin1dat <- binmeta_clean[which(binmeta_clean$bin=="0-200"),]
bin2dat <- binmeta_clean[which(binmeta_clean$bin=="200-300"),]
bin3dat <- binmeta_clean[which(binmeta_clean$bin=="300-400"),]
bin4dat <- binmeta_clean[which(binmeta_clean$bin=="400-500"),]
bin5dat <- binmeta_clean[which(binmeta_clean$bin=="500+"),]



station_bin <- binmeta_clean %>% group_by(STATION) %>%
  summarize(n_yrs=n()) #not going to work, repeated years b/c diff bins

joinstat <- left_join(binmeta_clean, station_bin)

bin_analysis_dat <- joinstat[which(joinstat$n_yrs>5),] #is this too liberal?
table(joinstat$STATION, joinstat$bin)





#model selection======

binmeta_clean$bin <- as.factor(binmeta_clean$bin)


