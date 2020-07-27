#stats on data binned by length

#using binmeta from join_CPUEdat_w_sizeCPUEdat.R

library(ggplot2)

ex1 <- ggplot(binmeta, aes( YEAR, bin_sum_WGTCPUE_LEN))
ex1 + geom_point() + facet_wrap(~bin, scales="free") #one super high value in 500+ bin
#log??

binmeta$log_sum_WGTCPUE_LEN <- log(binmeta$bin_sum_WGTCPUE_LEN)
ex1.5 <- ggplot(binmeta, aes( YEAR, log_sum_WGTCPUE_LEN))
ex1.5 + geom_point() + facet_wrap(~bin, scales="free")

ex2 <- ggplot(binmeta, aes( BOT_DEPTH, bin_sum_WGTCPUE_LEN))
ex2 + geom_point() + facet_wrap(~bin, scales="free")

ex2.5 <- ggplot(binmeta, aes( BOT_DEPTH, log_sum_WGTCPUE_LEN))
ex2.5 + geom_point() + facet_wrap(~bin, scales="free")

ex3 <- ggplot(binmeta, aes( BOT_TEMP, bin_sum_WGTCPUE_LEN))
ex3 + geom_point() + facet_wrap(~bin, scales="free")

ex3.5 <- ggplot(binmeta, aes( BOT_TEMP, log_sum_WGTCPUE_LEN))
ex3.5 + geom_point() + facet_wrap(~bin, scales="free")

ex4 <- ggplot(binmeta, aes( long_albers, bin_sum_WGTCPUE_LEN))
ex4 + geom_point() + facet_wrap(~bin, scales="free")

ex4.5 <- ggplot(binmeta, aes( long_albers, log_sum_WGTCPUE_LEN))
ex4.5 + geom_point() + facet_wrap(~bin, scales="free")

ex5 <- ggplot(binmeta, aes( lat_albers, bin_sum_WGTCPUE_LEN))
ex5 + geom_point() + facet_wrap(~bin, scales="free")

ex5.5 <- ggplot(binmeta, aes( lat_albers, log_sum_WGTCPUE_LEN))
ex5.5 + geom_point() + facet_wrap(~bin, scales="free")
















