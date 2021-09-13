
#models==
library(fRegression)

# age 1
#age 1 has to be different because no previous year
lag1 <- lag12[which(lag12$AGE==1),]
base.null1 <- gam(log_sc_weight ~ s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag1, method="REML")
gam.check(base.null1) #
summary(base.null1) #wow R2 SO MUCH BETTER
plot(base.null1) #v wiggly
draw(base.null1, select=1)
draw(base.null1, select=2)
draw(base.null1, select=3)

lmTest(base.null1, "dw")

lag1sub <- lag1[,1:28]

lag1complete <- lag1sub[complete.cases(lag1sub)==TRUE,]
base.null1_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag1complete) 
summary(base.null1_ran$gam)
summary(base.null1_ran$mer)
AIC(base.null1_ran$mer) #10396.8 so lower than without random
#R-sq.(adj) =  0.455 lower than without random


vb1 <- visreg(base.null1, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)


AICc(base.null1) #

AICc_1base <- AICc(base.null1) #
AICc_1base # 

AICc_1baseran <- AICc(base.null1_ran$mer) #
AICc_1baseran

R2.a1 <- 1-var(residuals(base.null1))/(var(model.response(model.frame(base.null1))))
R2.r1 <- 1-var(residuals(base.null1_ran$gam))/(var(model.response(model.frame(base.null1_ran$gam))))

#without random
base.null1.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag1)
R2.a1.nosst <- 1-var(residuals(base.null1.nosst))/(var(model.response(model.frame(base.null1.nosst))))
R2.a1 - R2.a1.nosst 
AICc_1base_nosst <- AICc(base.null1.nosst) #
AICc_1base_nosst - AICc_1base

base.null1.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag1)
R2.a1.nolatlong <- 1-var(residuals(base.null1.nolatlong))/(var(model.response(model.frame(base.null1.nolatlong))))
R2.a1 - R2.a1.nolatlong 
AICc_1base_nolatlong <- AICc(base.null1.nolatlong) #
AICc_1base_nolatlong - AICc_1base

base.null1.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag1)
R2.a1.nojul <- 1-var(residuals(base.null1.nojul))/(var(model.response(model.frame(base.null1.nojul))))
R2.a1 - R2.a1.nojul 
AICc_1base_nojul <- AICc(base.null1.nojul) #
AICc_1base_nojul - AICc_1base


#with random
nosst.null1_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag1complete) 
R2.r1.nosst <- 1-var(residuals(nosst.null1_ran$gam))/(var(model.response(model.frame(nosst.null1_ran$gam))))
R2.r1 - R2.r1.nosst 
AICc_1baseran_nosst <- AICc(nosst.null1_ran$mer) #
AICc_1baseran_nosst - AICc_1baseran

nolatlong.null1_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag1complete) 
R2.r1.nolatlong <- 1-var(residuals(nolatlong.null1_ran$gam))/(var(model.response(model.frame(nolatlong.null1_ran$gam))))
R2.r1 - R2.r1.nolatlong 
AICc_1baseran_nolatlong <- AICc(nolatlong.null1_ran$mer) #
AICc_1baseran_nolatlong - AICc_1baseran

nojul.null1_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                             random=~(1|YEAR/HAUL), data=lag1complete) 
R2.r1.nojul <- 1-var(residuals(nojul.null1_ran$gam))/(var(model.response(model.frame(nojul.null1_ran$gam))))
R2.r1 - R2.r1.nojul 
AICc_1baseran_nojul <- AICc(nojul.null1_ran$mer) #
AICc_1baseran_nojul - AICc_1baseran





# age 2
lag2 <- lag12[which(lag12$AGE==2),]
base.null2 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag2)
gam.check(base.null2) #
summary(base.null2)
plot(base.null2) #v wiggly
draw(base.null2, select=1)
draw(base.null2, select=2)
draw(base.null2, select=3)

lmTest(base.null2, "dw")

lag2complete <- lag2[complete.cases(lag2)==TRUE,]
base.null2_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag2complete)
summary(base.null2_ran$gam)
summary(base.null2_ran$mer)
AIC(base.null2_ran$mer) #4053.007 lower than without random
#R-sq.(adj) =  0.248 lower than without random


vb2 <- visreg(base.null2, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null2) #

AICc_2base <- AICc(base.null2) #
AICc_2base #

R2.a2 <- 1-var(residuals(base.null2))/(var(model.response(model.frame(base.null2))))

base.null2.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag2)
R2.a2.nosst <- 1-var(residuals(base.null2.nosst))/(var(model.response(model.frame(base.null2.nosst))))
R2.a2 - R2.a2.nosst 
AICc_2base_nosst <- AICc(base.null2.nosst) #
AICc_2base_nosst - AICc_2base

base.null2.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag2)
R2.a2.nolatlong <- 1-var(residuals(base.null2.nolatlong))/(var(model.response(model.frame(base.null2.nolatlong))))
R2.a2 - R2.a2.nolatlong 
AICc_2base_nolatlong <- AICc(base.null2.nolatlong) #
AICc_2base_nolatlong - AICc_2base

base.null2.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag2)
R2.a2.nojul <- 1-var(residuals(base.null2.nojul))/(var(model.response(model.frame(base.null2.nojul))))
R2.a2 - R2.a2.nojul 
AICc_2base_nojul <- AICc(base.null2.nojul) #
AICc_2base_nojul - AICc_2base


#with random
AICc_2baseran <- AICc(base.null2_ran$mer) #
AICc_2baseran

R2.r2 <- 1-var(residuals(base.null2_ran$gam))/(var(model.response(model.frame(base.null2_ran$gam))))

nosst.null2_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag2complete) 
R2.r2.nosst <- 1-var(residuals(nosst.null2_ran$gam))/(var(model.response(model.frame(nosst.null2_ran$gam))))
R2.r2 - R2.r2.nosst 
AICc_2baseran_nosst <- AICc(nosst.null2_ran$mer) #
AICc_2baseran_nosst - AICc_2baseran

nolatlong.null2_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag2complete) 
R2.r2.nolatlong <- 1-var(residuals(nolatlong.null2_ran$gam))/(var(model.response(model.frame(nolatlong.null2_ran$gam))))
R2.r2 - R2.r2.nolatlong 
AICc_2baseran_nolatlong <- AICc(nolatlong.null2_ran$mer) #
AICc_2baseran_nolatlong - AICc_2baseran

nojul.null2_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag2complete) 
R2.r2.nojul <- 1-var(residuals(nojul.null2_ran$gam))/(var(model.response(model.frame(nojul.null2_ran$gam))))
R2.r2 - R2.r2.nojul 
AICc_2baseran_nojul <- AICc(nojul.null2_ran$mer) #
AICc_2baseran_nojul - AICc_2baseran


# age 3

base.null3 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag3only)
gam.check(base.null3) #
summary(base.null3)
plot(base.null3) #
draw(base.null3, select=1)
draw(base.null3, select=2)
draw(base.null3, select=3)

lmTest(base.null3, "dw")

lag3complete <- lag3only[complete.cases(lag3only)==TRUE,]
base.null3_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag3complete)
summary(base.null3_ran$gam)
summary(base.null3_ran$mer)
AIC(base.null3_ran$mer) #4278.209 lower than without random
#R-sq.(adj) =  0.237 lower than without random


vb3 <- visreg(base.null3, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)


AICc_3base <- AICc(base.null3) #
AICc_3base # 

R2.a3 <- 1-var(residuals(base.null3))/(var(model.response(model.frame(base.null3))))

base.null3.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag3only)
R2.a3.nosst <- 1-var(residuals(base.null3.nosst))/(var(model.response(model.frame(base.null3.nosst))))
R2.a3 - R2.a3.nosst 
AICc_3base_nosst <- AICc(base.null3.nosst) #
AICc_3base_nosst - AICc_3base

base.null3.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag3only)
R2.a3.nolatlong <- 1-var(residuals(base.null3.nolatlong))/(var(model.response(model.frame(base.null3.nolatlong))))
R2.a3 - R2.a3.nolatlong 
AICc_3base_nolatlong <- AICc(base.null3.nolatlong) #
AICc_3base_nolatlong - AICc_3base

base.null3.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag3only)
R2.a3.nojul <- 1-var(residuals(base.null3.nojul))/(var(model.response(model.frame(base.null3.nojul))))
R2.a3 - R2.a3.nojul 
AICc_3base_nojul <- AICc(base.null3.nojul) #
AICc_3base_nojul - AICc_3base


#with random
AICc_3baseran <- AICc(base.null3_ran$mer) #
AICc_3baseran

R2.r3 <- 1-var(residuals(base.null3_ran$gam))/(var(model.response(model.frame(base.null3_ran$gam))))

nosst.null3_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag3complete) 
R2.r3.nosst <- 1-var(residuals(nosst.null3_ran$gam))/(var(model.response(model.frame(nosst.null3_ran$gam))))
R2.r3 - R2.r3.nosst 
AICc_3baseran_nosst <- AICc(nosst.null3_ran$mer) #
AICc_3baseran_nosst - AICc_3baseran

nolatlong.null3_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag3complete) 
R2.r3.nolatlong <- 1-var(residuals(nolatlong.null3_ran$gam))/(var(model.response(model.frame(nolatlong.null3_ran$gam))))
R2.r3 - R2.r3.nolatlong 
AICc_3baseran_nolatlong <- AICc(nolatlong.null3_ran$mer) #
AICc_3baseran_nolatlong - AICc_3baseran

nojul.null3_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag3complete) 
R2.r3.nojul <- 1-var(residuals(nojul.null3_ran$gam))/(var(model.response(model.frame(nojul.null3_ran$gam))))
R2.r3 - R2.r3.nojul 
AICc_3baseran_nojul <- AICc(nojul.null3_ran$mer) #
AICc_3baseran_nojul - AICc_3baseran

# age 4

base.null4 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag4only)
gam.check(base.null4) # not positive definite!!!
summary(base.null4)
plot(base.null4) #
draw(base.null4, select=1)
draw(base.null4, select=2)
draw(base.null4, select=3)

lmTest(base.null4, "dw")

lag4complete <- lag4only[complete.cases(lag4only)==TRUE,]
base.null4_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                       random=~(1|YEAR/HAUL), data=lag4complete)
summary(base.null4_ran$gam)
summary(base.null4_ran$mer)
AIC(base.null4_ran$mer) #5537.197 lower than without random
#R-sq.(adj) =  0.248 lower than without random

vb4 <- visreg(base.null4, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null4) #

AICc_4base <- AICc(base.null4) #
AICc_4base

R2.a4 <- 1-var(residuals(base.null4))/(var(model.response(model.frame(base.null4))))

base.null4.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag4only)
R2.a4.nosst <- 1-var(residuals(base.null4.nosst))/(var(model.response(model.frame(base.null4.nosst))))
R2.a4 - R2.a4.nosst 
AICc_4base_nosst <- AICc(base.null4.nosst) #
AICc_4base_nosst - AICc_4base

base.null4.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag4only)
R2.a4.nolatlong <- 1-var(residuals(base.null4.nolatlong))/(var(model.response(model.frame(base.null4.nolatlong))))
R2.a4 - R2.a4.nolatlong 
AICc_4base_nolatlong <- AICc(base.null4.nolatlong) #
AICc_4base_nolatlong - AICc_4base

base.null4.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag4only)
R2.a4.nojul <- 1-var(residuals(base.null4.nojul))/(var(model.response(model.frame(base.null4.nojul))))
R2.a4 - R2.a4.nojul 
AICc_4base_nojul <- AICc(base.null4.nojul) #
AICc_4base_nojul - AICc_4base


#with random
AICc_4baseran <- AICc(base.null4_ran$mer) #
AICc_4baseran

R2.r4 <- 1-var(residuals(base.null4_ran$gam))/(var(model.response(model.frame(base.null4_ran$gam))))

nosst.null4_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag4complete) 
R2.r4.nosst <- 1-var(residuals(nosst.null4_ran$gam))/(var(model.response(model.frame(nosst.null4_ran$gam))))
R2.r4 - R2.r4.nosst 
AICc_4baseran_nosst <- AICc(nosst.null4_ran$mer) #
AICc_4baseran_nosst - AICc_4baseran

nolatlong.null4_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag4complete) 
R2.r4.nolatlong <- 1-var(residuals(nolatlong.null4_ran$gam))/(var(model.response(model.frame(nolatlong.null4_ran$gam))))
R2.r4 - R2.r4.nolatlong 
AICc_4baseran_nolatlong <- AICc(nolatlong.null4_ran$mer) #
AICc_4baseran_nolatlong - AICc_4baseran

nojul.null4_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag4complete) 
R2.r4.nojul <- 1-var(residuals(nojul.null4_ran$gam))/(var(model.response(model.frame(nojul.null4_ran$gam))))
R2.r4 - R2.r4.nojul 
AICc_4baseran_nojul <- AICc(nojul.null4_ran$mer) #
AICc_4baseran_nojul - AICc_4baseran


# age 5

base.null5 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag5only)
gam.check(base.null5) #
summary(base.null5)
plot(base.null5) #
draw(base.null5, select=1)
draw(base.null5, select=2)
draw(base.null5, select=3)

lmTest(base.null5, "dw")

lag5complete <- lag5only[complete.cases(lag5only)==TRUE,]
base.null5_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag5complete)
summary(base.null5_ran$gam)
summary(base.null5_ran$mer)
AIC(base.null5_ran$mer) #8091.068 lower than without random
#R-sq.(adj) =  0.157 lower than without random


vb5 <- visreg(base.null5, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null5) #

AICc_5base <- AICc(base.null5) #
AICc_5base

R2.a5 <- 1-var(residuals(base.null5))/(var(model.response(model.frame(base.null5))))

base.null5.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag5only)
R2.a5.nosst <- 1-var(residuals(base.null5.nosst))/(var(model.response(model.frame(base.null5.nosst))))
R2.a5 - R2.a5.nosst 
AICc_5base_nosst <- AICc(base.null5.nosst) #
AICc_5base_nosst - AICc_5base

base.null5.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag5only)
R2.a5.nolatlong <- 1-var(residuals(base.null5.nolatlong))/(var(model.response(model.frame(base.null5.nolatlong))))
R2.a5 - R2.a5.nolatlong 
AICc_5base_nolatlong <- AICc(base.null5.nolatlong) #
AICc_5base_nolatlong - AICc_5base

base.null5.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag5only)
R2.a5.nojul <- 1-var(residuals(base.null5.nojul))/(var(model.response(model.frame(base.null5.nojul))))
R2.a5 - R2.a5.nojul 
AICc_5base_nojul <- AICc(base.null5.nojul) #
AICc_5base_nojul - AICc_5base


#with random
AICc_5baseran <- AICc(base.null5_ran$mer) #
AICc_5baseran

R2.r5 <- 1-var(residuals(base.null5_ran$gam))/(var(model.response(model.frame(base.null5_ran$gam))))

nosst.null5_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag5complete) 
R2.r5.nosst <- 1-var(residuals(nosst.null5_ran$gam))/(var(model.response(model.frame(nosst.null5_ran$gam))))
R2.r5 - R2.r5.nosst 
AICc_5baseran_nosst <- AICc(nosst.null5_ran$mer) #
AICc_5baseran_nosst - AICc_5baseran

nolatlong.null5_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag5complete) 
R2.r5.nolatlong <- 1-var(residuals(nolatlong.null5_ran$gam))/(var(model.response(model.frame(nolatlong.null5_ran$gam))))
R2.r5 - R2.r5.nolatlong 
AICc_5baseran_nolatlong <- AICc(nolatlong.null5_ran$mer) #
AICc_5baseran_nolatlong - AICc_5baseran

nojul.null5_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag5complete) 
R2.r5.nojul <- 1-var(residuals(nojul.null5_ran$gam))/(var(model.response(model.frame(nojul.null5_ran$gam))))
R2.r5 - R2.r5.nojul 
AICc_5baseran_nojul <- AICc(nojul.null5_ran$mer) #
AICc_5baseran_nojul - AICc_5baseran


# age 6

base.null6 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag6only)
gam.check(base.null6) #
summary(base.null6)
plot(base.null6) #
draw(base.null6, select=1)
draw(base.null6, select=2)
draw(base.null6, select=3)

lmTest(base.null6, "dw")

lag6complete <- lag6only[complete.cases(lag6only)==TRUE,]
base.null6_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag6complete)
summary(base.null6_ran$gam)
summary(base.null6_ran$mer)
AIC(base.null6_ran$mer) #9820.327 lower than without random
#R-sq.(adj) =  0.176 lower than without random


vb6 <- visreg(base.null6, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null6) #

AICc_6base <- AICc(base.null6) #
AICc_6base 

R2.a6 <- 1-var(residuals(base.null6))/(var(model.response(model.frame(base.null6))))

base.null6.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag6only)
R2.a6.nosst <- 1-var(residuals(base.null6.nosst))/(var(model.response(model.frame(base.null6.nosst))))
R2.a6 - R2.a6.nosst 
AICc_6base_nosst <- AICc(base.null6.nosst) #
AICc_6base_nosst - AICc_6base

base.null6.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag6only)
R2.a6.nolatlong <- 1-var(residuals(base.null6.nolatlong))/(var(model.response(model.frame(base.null6.nolatlong))))
R2.a6 - R2.a6.nolatlong 
AICc_6base_nolatlong <- AICc(base.null6.nolatlong) #
AICc_6base_nolatlong - AICc_6base

base.null6.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag6only)
R2.a6.nojul <- 1-var(residuals(base.null6.nojul))/(var(model.response(model.frame(base.null6.nojul))))
R2.a6 - R2.a6.nojul 
AICc_6base_nojul <- AICc(base.null6.nojul) #
AICc_6base_nojul - AICc_6base


#with random
AICc_6baseran <- AICc(base.null6_ran$mer) #
AICc_6baseran

R2.r6 <- 1-var(residuals(base.null6_ran$gam))/(var(model.response(model.frame(base.null6_ran$gam))))

nosst.null6_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag6complete) 
R2.r6.nosst <- 1-var(residuals(nosst.null6_ran$gam))/(var(model.response(model.frame(nosst.null6_ran$gam))))
R2.r6 - R2.r6.nosst 
AICc_6baseran_nosst <- AICc(nosst.null6_ran$mer) #
AICc_6baseran_nosst - AICc_6baseran

nolatlong.null6_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag6complete) 
R2.r6.nolatlong <- 1-var(residuals(nolatlong.null6_ran$gam))/(var(model.response(model.frame(nolatlong.null6_ran$gam))))
R2.r6 - R2.r6.nolatlong 
AICc_6baseran_nolatlong <- AICc(nolatlong.null6_ran$mer) #
AICc_6baseran_nolatlong - AICc_6baseran

nojul.null6_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag6complete) 
R2.r6.nojul <- 1-var(residuals(nojul.null6_ran$gam))/(var(model.response(model.frame(nojul.null6_ran$gam))))
R2.r6 - R2.r6.nojul 
AICc_6baseran_nojul <- AICc(nojul.null6_ran$mer) #
AICc_6baseran_nojul - AICc_6baseran


# age 7

base.null7 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag7only)
gam.check(base.null7) #
summary(base.null7)
plot(base.null7) #
draw(base.null7, select=1)
draw(base.null7, select=2)
draw(base.null7, select=3)

lmTest(base.null7, "dw")

lag7complete <- lag7only[complete.cases(lag7only)==TRUE,]
base.null7_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag7complete)
summary(base.null7_ran$gam)
summary(base.null7_ran$mer)
AIC(base.null7_ran$mer) #8404.48 lower than without random
#R-sq.(adj) =  0.221 lower than without random


vb7 <- visreg(base.null7, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null7) #

AICc_7base <- AICc(base.null7) #
AICc_7base  

R2.a7 <- 1-var(residuals(base.null7))/(var(model.response(model.frame(base.null7))))

base.null7.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag7only)
R2.a7.nosst <- 1-var(residuals(base.null7.nosst))/(var(model.response(model.frame(base.null7.nosst))))
R2.a7 - R2.a7.nosst 
AICc_7base_nosst <- AICc(base.null7.nosst) #
AICc_7base_nosst - AICc_7base


base.null7.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag7only)
R2.a7.nolatlong <- 1-var(residuals(base.null7.nolatlong))/(var(model.response(model.frame(base.null7.nolatlong))))
R2.a7 - R2.a7.nolatlong 
AICc_7base_nolatlong <- AICc(base.null7.nolatlong) #
AICc_7base_nolatlong - AICc_7base

base.null7.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag7only)
R2.a7.nojul <- 1-var(residuals(base.null7.nojul))/(var(model.response(model.frame(base.null7.nojul))))
R2.a7 - R2.a7.nojul 
AICc_7base_nojul <- AICc(base.null7.nojul) #
AICc_7base_nojul - AICc_7base


#with random
AICc_7baseran <- AICc(base.null7_ran$mer) #
AICc_7baseran

R2.r7 <- 1-var(residuals(base.null7_ran$gam))/(var(model.response(model.frame(base.null7_ran$gam))))

nosst.null7_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag7complete) 
R2.r7.nosst <- 1-var(residuals(nosst.null7_ran$gam))/(var(model.response(model.frame(nosst.null7_ran$gam))))
R2.r7 - R2.r7.nosst 
AICc_7baseran_nosst <- AICc(nosst.null7_ran$mer) #
AICc_7baseran_nosst - AICc_7baseran

nolatlong.null7_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag7complete) 
R2.r7.nolatlong <- 1-var(residuals(nolatlong.null7_ran$gam))/(var(model.response(model.frame(nolatlong.null7_ran$gam))))
R2.r7 - R2.r7.nolatlong 
AICc_7baseran_nolatlong <- AICc(nolatlong.null7_ran$mer) #
AICc_7baseran_nolatlong - AICc_7baseran

nojul.null7_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag7complete) 
R2.r7.nojul <- 1-var(residuals(nojul.null7_ran$gam))/(var(model.response(model.frame(nojul.null7_ran$gam))))
R2.r7 - R2.r7.nojul 
AICc_7baseran_nojul <- AICc(nojul.null7_ran$mer) #
AICc_7baseran_nojul - AICc_7baseran



# age 8

base.null8 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag8only)
gam.check(base.null8) #
summary(base.null8)
plot(base.null8) #
draw(base.null8, select=1)
draw(base.null8, select=2)
draw(base.null8, select=3)

lmTest(base.null8, "dw")

lag8complete <- lag8only[complete.cases(lag8only)==TRUE,]
base.null8_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag8complete)
summary(base.null8_ran$gam)
summary(base.null8_ran$mer)
AIC(base.null8_ran$mer) #6114.361 lower than without random
#R-sq.(adj) =  0.193 lower than without random


vb8 <- visreg(base.null8, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null8) #

AICc_8base <- AICc(base.null8) #
AICc_8base 

R2.a8 <- 1-var(residuals(base.null8))/(var(model.response(model.frame(base.null8))))

base.null8.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag8only)
R2.a8.nosst <- 1-var(residuals(base.null8.nosst))/(var(model.response(model.frame(base.null8.nosst))))
R2.a8 - R2.a8.nosst 
AICc_8base_nosst <- AICc(base.null8.nosst) #
AICc_8base_nosst - AICc_8base


base.null8.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag8only)
R2.a8.nolatlong <- 1-var(residuals(base.null8.nolatlong))/(var(model.response(model.frame(base.null8.nolatlong))))
R2.a8 - R2.a8.nolatlong 
AICc_8base_nolatlong <- AICc(base.null8.nolatlong) #
AICc_8base_nolatlong - AICc_8base

base.null8.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag8only)
R2.a8.nojul <- 1-var(residuals(base.null8.nojul))/(var(model.response(model.frame(base.null8.nojul))))
R2.a8 - R2.a8.nojul 
AICc_8base_nojul <- AICc(base.null8.nojul) #
AICc_8base_nojul - AICc_8base


#with random
AICc_8baseran <- AICc(base.null8_ran$mer) #
AICc_8baseran

R2.r8 <- 1-var(residuals(base.null8_ran$gam))/(var(model.response(model.frame(base.null8_ran$gam))))

nosst.null8_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag8complete) 
R2.r8.nosst <- 1-var(residuals(nosst.null8_ran$gam))/(var(model.response(model.frame(nosst.null8_ran$gam))))
R2.r8 - R2.r8.nosst 
AICc_8baseran_nosst <- AICc(nosst.null8_ran$mer) #
AICc_8baseran_nosst - AICc_8baseran

nolatlong.null8_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag8complete) 
R2.r8.nolatlong <- 1-var(residuals(nolatlong.null8_ran$gam))/(var(model.response(model.frame(nolatlong.null8_ran$gam))))
R2.r8 - R2.r8.nolatlong 
AICc_8baseran_nolatlong <- AICc(nolatlong.null8_ran$mer) #
AICc_8baseran_nolatlong - AICc_8baseran

nojul.null8_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag8complete) 
R2.r8.nojul <- 1-var(residuals(nojul.null8_ran$gam))/(var(model.response(model.frame(nojul.null8_ran$gam))))
R2.r8 - R2.r8.nojul 
AICc_8baseran_nojul <- AICc(nojul.null8_ran$mer) #
AICc_8baseran_nojul - AICc_8baseran



# age 9

base.null9 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag9only)
gam.check(base.null9) #not positive definite!
summary(base.null9)
plot(base.null9) #
draw(base.null9, select=1)
draw(base.null9, select=2)
draw(base.null9, select=3)

lmTest(base.null9, "dw")

lag9complete <- lag9only[complete.cases(lag9only)==TRUE,]
base.null9_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag9complete)
summary(base.null9_ran$gam)
summary(base.null9_ran$mer)
AIC(base.null9_ran$mer) #4921.163 lower than without random
#R-sq.(adj) =  0.169 lower than without random


vb9 <- visreg(base.null9, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null9) #

AICc_9base <- AICc(base.null9) #
AICc_9base 

R2.a9 <- 1-var(residuals(base.null9))/(var(model.response(model.frame(base.null9))))

base.null9.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag9only)
R2.a9.nosst <- 1-var(residuals(base.null9.nosst))/(var(model.response(model.frame(base.null9.nosst))))
R2.a9 - R2.a9.nosst 
AICc_9base_nosst <- AICc(base.null9.nosst) #
AICc_9base_nosst - AICc_9base

base.null9.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag9only)
R2.a9.nolatlong <- 1-var(residuals(base.null9.nolatlong))/(var(model.response(model.frame(base.null9.nolatlong))))
R2.a9 - R2.a9.nolatlong 
AICc_9base_nolatlong <- AICc(base.null9.nolatlong) #
AICc_9base_nolatlong - AICc_9base

base.null9.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag9only)
R2.a9.nojul <- 1-var(residuals(base.null9.nojul))/(var(model.response(model.frame(base.null9.nojul))))
R2.a9 - R2.a9.nojul 
AICc_9base_nojul <- AICc(base.null9.nojul) #
AICc_9base_nojul - AICc_9base


#with random
AICc_9baseran <- AICc(base.null9_ran$mer) #
AICc_9baseran

R2.r9 <- 1-var(residuals(base.null9_ran$gam))/(var(model.response(model.frame(base.null9_ran$gam))))

nosst.null9_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag9complete) 
R2.r9.nosst <- 1-var(residuals(nosst.null9_ran$gam))/(var(model.response(model.frame(nosst.null9_ran$gam))))
R2.r9 - R2.r9.nosst 
AICc_9baseran_nosst <- AICc(nosst.null9_ran$mer) #
AICc_9baseran_nosst - AICc_9baseran

nolatlong.null9_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag9complete) 
R2.r9.nolatlong <- 1-var(residuals(nolatlong.null9_ran$gam))/(var(model.response(model.frame(nolatlong.null9_ran$gam))))
R2.r9 - R2.r9.nolatlong 
AICc_9baseran_nolatlong <- AICc(nolatlong.null9_ran$mer) #
AICc_9baseran_nolatlong - AICc_9baseran

nojul.null9_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag9complete) 
R2.r9.nojul <- 1-var(residuals(nojul.null9_ran$gam))/(var(model.response(model.frame(nojul.null9_ran$gam))))
R2.r9 - R2.r9.nojul 
AICc_9baseran_nojul <- AICc(nojul.null9_ran$mer) #
AICc_9baseran_nojul - AICc_9baseran



# age 10

base.null10 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag10only)
gam.check(base.null10) #
summary(base.null10)
plot(base.null10) #
draw(base.null10, select=1)
draw(base.null10, select=2)
draw(base.null10, select=3)

lmTest(base.null10, "dw")

lag10complete <- lag10only[complete.cases(lag10only)==TRUE,]
base.null10_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag10complete)
summary(base.null10_ran$gam)
summary(base.null10_ran$mer)
AIC(base.null10_ran$mer) #4160.911 lower than without random
#R-sq.(adj) =  0.17 lower than without random


vb10 <- visreg(base.null10, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null10) #

AICc_10base <- AICc(base.null10) #
AICc_10base 

R2.a10 <- 1-var(residuals(base.null10))/(var(model.response(model.frame(base.null10))))

base.null10.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag10only)
R2.a10.nosst <- 1-var(residuals(base.null10.nosst))/(var(model.response(model.frame(base.null10.nosst))))
R2.a10 - R2.a10.nosst 
AICc_10base_nosst <- AICc(base.null10.nosst) #
AICc_10base_nosst - AICc_10base

base.null10.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag10only)
R2.a10.nolatlong <- 1-var(residuals(base.null10.nolatlong))/(var(model.response(model.frame(base.null10.nolatlong))))
R2.a10 - R2.a10.nolatlong 
AICc_10base_nolatlong <- AICc(base.null10.nolatlong) #
AICc_10base_nolatlong - AICc_10base

base.null10.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag10only)
R2.a10.nojul <- 1-var(residuals(base.null10.nojul))/(var(model.response(model.frame(base.null10.nojul))))
R2.a10 - R2.a10.nojul 
AICc_10base_nojul <- AICc(base.null10.nojul) #
AICc_10base_nojul - AICc_10base


#with random
AICc_10baseran <- AICc(base.null10_ran$mer) #
AICc_10baseran

R2.r10 <- 1-var(residuals(base.null10_ran$gam))/(var(model.response(model.frame(base.null10_ran$gam))))

nosst.null10_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag10complete) 
R2.r10.nosst <- 1-var(residuals(nosst.null10_ran$gam))/(var(model.response(model.frame(nosst.null10_ran$gam))))
R2.r10 - R2.r10.nosst 
AICc_10baseran_nosst <- AICc(nosst.null10_ran$mer) #
AICc_10baseran_nosst - AICc_10baseran

nolatlong.null10_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag10complete) 
R2.r10.nolatlong <- 1-var(residuals(nolatlong.null10_ran$gam))/(var(model.response(model.frame(nolatlong.null10_ran$gam))))
R2.r10 - R2.r10.nolatlong 
AICc_10baseran_nolatlong <- AICc(nolatlong.null10_ran$mer) #
AICc_10baseran_nolatlong - AICc_10baseran

nojul.null10_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag10complete) 
R2.r10.nojul <- 1-var(residuals(nojul.null10_ran$gam))/(var(model.response(model.frame(nojul.null10_ran$gam))))
R2.r10 - R2.r10.nojul 
AICc_10baseran_nojul <- AICc(nojul.null10_ran$mer) #
AICc_10baseran_nojul - AICc_10baseran


# age 11

base.null11 <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag11only)
gam.check(base.null11) #
summary(base.null11)
plot(base.null11) #
draw(base.null11, select=1)
draw(base.null11, select=2)
draw(base.null11, select=3)

lmTest(base.null11, "dw")

lag11complete <- lag11only[complete.cases(lag11only)==TRUE,]
base.null11_ran <- gamm4(log_sc_weight ~  s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                        random=~(1|YEAR/HAUL), data=lag11complete)
summary(base.null11_ran$gam)
summary(base.null11_ran$mer)
AIC(base.null11_ran$mer) #3426.587 lower than without random
#R-sq.(adj) =  0.188 lower than without random


vb11 <- visreg(base.null11, "sst.amj", scale="response",ylab="Scaled log(weight-at-age)", xlab="April-June SST", rug=1)

AICc(base.null11) #

AICc_11base <- AICc(base.null11) #
AICc_11base  

R2.a11 <- 1-var(residuals(base.null11))/(var(model.response(model.frame(base.null11))))

base.null11.nosst <- gam(log_sc_weight ~   te(LONGITUDE, LATITUDE) + s(julian, k = 4), data=lag11only)
R2.a11.nosst <- 1-var(residuals(base.null11.nosst))/(var(model.response(model.frame(base.null11.nosst))))
R2.a11 - R2.a11.nosst 
AICc_11base_nosst  <- AICc(base.null11.nosst ) #
AICc_11base_nosst  - AICc_11base

base.null11.nolatlong <- gam(log_sc_weight ~  s(sst.amj, k=4) +  s(julian, k = 4), data=lag11only)
R2.a11.nolatlong <- 1-var(residuals(base.null11.nolatlong))/(var(model.response(model.frame(base.null11.nolatlong))))
R2.a11 - R2.a11.nolatlong 
AICc_11base_nolatlong  <- AICc(base.null11.nolatlong ) #
AICc_11base_nolatlong  - AICc_11base

base.null11.nojul <- gam(log_sc_weight ~  s(sst.amj, k=4) + te(LONGITUDE, LATITUDE), data=lag11only)
R2.a11.nojul <- 1-var(residuals(base.null11.nojul))/(var(model.response(model.frame(base.null11.nojul))))
R2.a11 - R2.a11.nojul 
AICc_11base_nojul <- AICc(base.null11.nojul) #
AICc_11base_nojul - AICc_11base


#with random
AICc_11baseran <- AICc(base.null11_ran$mer) #
AICc_11baseran

R2.r11 <- 1-var(residuals(base.null11_ran$gam))/(var(model.response(model.frame(base.null11_ran$gam))))

nosst.null11_ran <- gamm4(log_sc_weight ~   t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                         random=~(1|YEAR/HAUL), data=lag11complete) 
R2.r11.nosst <- 1-var(residuals(nosst.null11_ran$gam))/(var(model.response(model.frame(nosst.null11_ran$gam))))
R2.r11 - R2.r11.nosst 
AICc_11baseran_nosst <- AICc(nosst.null11_ran$mer) #
AICc_11baseran_nosst - AICc_11baseran

nolatlong.null11_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + s(julian, k = 4),
                             random=~(1|YEAR/HAUL), data=lag11complete) 
R2.r11.nolatlong <- 1-var(residuals(nolatlong.null11_ran$gam))/(var(model.response(model.frame(nolatlong.null11_ran$gam))))
R2.r11 - R2.r11.nolatlong 
AICc_11baseran_nolatlong <- AICc(nolatlong.null11_ran$mer) #
AICc_11baseran_nolatlong - AICc_11baseran

nojul.null11_ran <- gamm4(log_sc_weight ~   s(sst.amj, k=4) + t2(LONGITUDE, LATITUDE),
                         random=~(1|YEAR/HAUL), data=lag11complete) 
R2.r11.nojul <- 1-var(residuals(nojul.null11_ran$gam))/(var(model.response(model.frame(nojul.null11_ran$gam))))
R2.r11 - R2.r11.nojul 
AICc_11baseran_nojul <- AICc(nojul.null11_ran$mer) #
AICc_11baseran_nojul - AICc_11baseran




#plot each age models======


vvb2 <- visregList(vb8, vb9, vb10, 
                  vb5, vb6, vb7, 
                  vb2, vb3, vb4,
                  collapse=TRUE,
                  labels=c("Age 8","Age 9","Age 10", 
                           "Age 5", "Age 6", "Age 7",
                           "Age 2", "Age 3", "Age 4"))

#par(mfrow=c(4,3), mar = c(4,4,4,4))
plot(vvb2,
     ylab="Scaled log (weight-at-age)",
     xlab="April-June SST")

vvb3 <- visregList(vb8, vb9, vb10, vb11, 
                   vb4, vb5, vb6,  vb7, 
                   vb1, vb2, vb3, 
                   collapse=TRUE,
                   # labels=c("Age 8", "Age 9",  "Age 10", "Age 11", 
                   #          "Age 4", "Age 5", "Age 6", "Age 7",  
                   #          "Age 1", "Age 2", "Age 3"))
                    labels=c("Age 8", "Age 9",  "Age 10", "Age 11", 
                   "Age 4", "Age 5", "Age 6", "Age 7",  
                    "Age 1", "Age 2", "Age 3"))
vvbg <- visregList( vb1, vb2, vb3, 
                    vb4, vb5, vb6,  vb7, 
                    vb8, vb9, vb10, vb11, 
                    collapse=TRUE)

plot(vvb3,
     ylab="Scaled log (weight-at-age)",
     xlab="April-June SST", rug=1)


#add age 11
vvb2 <- visregList(vb10, vb11, 
                  vb7, vb8, vb9,   
                  vb4, vb5,vb6, 
                  vb1, vb2, vb3, 
                  collapse=TRUE,
                  labels=c("Age 10", "Age 11",
                           "Age 7", "Age 8","Age 9",
                           "Age 4", "Age 5", "Age 6", 
                           "Age 1", "Age 2", "Age 3"))

#par(mfrow=c(4,3), mar = c(4,4,4,4))
plot(vvb2,
     ylab="Scaled log (weight-at-age)",
     xlab="April-June SST", rug=1)


vv3 <- visregList(v1, 
                  v2, v3, v4, v5, v6, v7, v8, v9, v10,
                  collapse=FALSE
))
par(mfrow=c(4,3), mar = c(4,4,4,4))
plot(vv3)


#multi-panel latxlong
library(cowplot)
library(gratia)
library(visreg)
library(mgcViz)

cb1 <- draw(base.null1, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb2 <- draw(base.null2, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb3 <- draw(base.null3, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb4 <- draw(base.null4, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb5 <- draw(base.null5, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb6 <- draw(base.null6, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb7 <- draw(base.null7, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb8 <- draw(base.null8, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb9 <- draw(base.null9, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))
cb10 <- draw(base.null10, select=2,  continuous_fill = ggplot2::scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)))

plot_grid(cb1, cb2, cb3,
          cb4, cb5, cb6, cb7,
          cb8, cb9, cb10, 
          labels = c('Age 1', 'Age 2', 'Age 3', 'Age 4', 'Age 5', 'Age 6',
                     'Age 7', 'Age 8', 'Age 9', 'Age 10'), label_size = 12)

cb1 <- getViz(base.null1)
pcb1 <- plot(sm(c1, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())


cb2 <- getViz(base.null2)
pcb2 <- plot(sm(c2, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())


cb3 <- getViz(base.null3)
pcb3 <- plot(sm(c3, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())


cb4 <- getViz(base.null4)
pcb4 <- plot(sm(c4, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())


cb5 <- getViz(base.null5)
pcb5 <- plot(sm(c5, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())



cb6 <- getViz(base.null6)
pcb6 <- plot(sm(c6, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())


cb7 <- getViz(base.null7)
pcb7 <- plot(sm(c7, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())



cb8 <- getViz(base.null8)
pcb8 <- plot(sm(c8, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())



cb9 <- getViz(base.null9)
pcb9 <- plot(sm(c9, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())



cb10 <- getViz(base.null10)
pcb10 <- plot(sm(c10, 2)) + l_fitRaster() + l_fitContour() + 
  labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3)) +
  theme(legend.position = "none")+ theme(plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank())
# l_fitRaster(pTrans = function(.p) 0.5) + 
# l_fitContour() + l_points() 

cb11 <- getViz(base.null11)
pcb11 <- plot(sm(c11, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3), name="Effect") + 
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0.1), "cm"), plot.title = element_blank(),
                                                axis.title.x = element_blank(),
                                                axis.title.y = element_blank())

pcb11_w_legend <- plot(sm(c11, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + #l_points() +
  geom_polygon(data = map_data ("world"), 
               aes(x=long, y = lat,group=group),fill="white",color="black",
               inherit.aes = F)+coord_sf(xlim = c(-177, -158.5), ylim = c(54.5, 62), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", type = "div", limits = c(-3,3), name="Effect") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())



gridPrint(pcb1, pcb2, pcb3, pcb4, pcb5,
          pcb6, pcb7, pcb8, pcb9, pcb10, pcb11, ncol = 3, 
          bottom="Longitude", left="Latitude")

#map for fig 1=====

#all_analysis_dat required, see script 'NEBS_depth_temp_plot.R'
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 63), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE), size=0.1, data=all_analysis_dat[which(all_analysis_dat$region=="SEBS"),]) + theme_bw() + 
  theme( legend.position = c(0.87, 0.85), legend.key = element_blank(),
         legend.background=element_blank()) #+ #geom_path(aes(x_adj,y), data=contour.lines.adj$z20, col="dark blue") +
# geom_path(aes(x_adj,y), data=contour.lines.adj$z50, col="#9ecae1") + geom_path(aes(x_adj,y), data=contour.lines.adj$z100, col="#3182bd") +
#  geom_path(aes(x_adj,y), data=contour.lines.adj$z200, col="navy blue") #+ geom_path(aes(x_adj,y), data=contour.lines.adj$z1000, col="dark green") 

