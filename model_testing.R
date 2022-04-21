### model testing ###
#-------------------------------------------------------------------------------

#disable scientific notation
options(scipen=999)

### NORTH ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousN <- CallSites_N(coniferous_woodland)
coniferousN_int <- make_interval_pol(coniferousN, 100, 10000)
PinusPiceaN <- rowSums(coniferousN_int[12:15]) #only Pinus and Picea
conN <- rowSums(coniferousN_int[3:18], na.rm = TRUE) #all coniferous taxa
mcp_area <- data.frame(age=coniferousN_int$lower_ends, LCC=PinusPiceaN) #LCC = PinusPiceaN or conN
plot(mcp_area)
# I4
model = list(LCC~1, ~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1384") #11
hypothesis(fit_mcp, "cp_2=5094") #3
hypothesis(fit_mcp, "cp_3=7674") #31
hypothesis(fit_mcp, "cp_4=8763") #16
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1471") #7
hypothesis(fit_mcp, "cp_2=8349") #40
# JS4
model = list(LCC~1, ~0+age, ~0+age, ~0+age, ~0+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=286") #4
hypothesis(fit_mcp, "cp_2=2416") #2
hypothesis(fit_mcp, "cp_3=4767") #1
hypothesis(fit_mcp, "cp_4=7061") #9
# JS2
model = list(LCC~1, ~0+age, ~0+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=312") #7
hypothesis(fit_mcp, "cp_2=6626") #7
# DS4
model = list(LCC~1, ~1+age, ~1+age, ~1+age, ~1+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1301") #9
hypothesis(fit_mcp, "cp_2=5230") #3
hypothesis(fit_mcp, "cp_3=7758") #7
hypothesis(fit_mcp, "cp_4=8729") #9
# DS2
model = list(LCC~1, ~1+age, ~1+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1364") #0.2
hypothesis(fit_mcp, "cp_2=7148") #4

# count sites
count <- countSitesPerInterval(coniferousN, 100)
count <- count[c(1:102),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1486") #11
hypothesis(fit_mcp, "cp_2=8370") #41


###deciduous_woodland
deciduousN <- CallSites_N(deciduous_woodland)
deciduousN_int <- make_interval_pol(deciduousN, 100, 10000)
BetulaN <- rowSums(deciduousN_int[7:13])
decN <- rowSums(deciduousN_int[3:ncol(deciduousN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousN_int$lower_ends, LCC=decN)
plot(mcp_area)


###wet_woodland
wetwoodlandN <- CallSites_N(wet_woodland)
wetwoodlandN_int <- make_interval_pol(wetwoodlandN, 100, 10000)
AlnusSalixN <- rowSums(wetwoodlandN_int[c(3,4,5,6,23)])
wetwN <- rowSums(wetwoodlandN_int[3:ncol(wetwoodlandN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandN_int$lower_ends, LCC=AlnusSalixN)
plot(mcp_area)


###wet_meadow
wetmeadowN <- CallSites_N(wet_meadow)
wetmeadowN_int <- make_interval_pol(wetmeadowN, 100, 10000)
wetmN <- rowSums(wetmeadowN_int[3:ncol(wetmeadowN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowN_int$lower_ends, LCC=wetmN)
plot(mcp_area)

###pasture
pastureN <- CallSites_N(pasture)
pastureN_int <- make_interval_pol(pastureN, 100, 10000)
#PoaceaeN
pasN <- rowSums(pastureN_int[3:ncol(pastureN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureN_int$lower_ends, LCC=pasN)
plot(mcp_area)
# I4
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1384") #11
hypothesis(fit_mcp, "cp_2=5094") #3
hypothesis(fit_mcp, "cp_3=7674") #31
hypothesis(fit_mcp, "cp_4=8763") #16

###arable
arableN <- CallSites_N(arable)
arableN_int <- make_interval_pol(arableN, 100, 10000)
araN <- rowSums(arableN_int[3:ncol(arableN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableN_int$lower_ends, LCC=araN)
mcp_area <- mcp_area[c(1:82),] #until 8000BP
plot(mcp_area)
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=2009") #18
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1258") #0.5
hypothesis(fit_mcp, "cp_2=2614") #5

# count sites
count <- countSitesPerInterval(arableN, 100)
count <- count[c(1:82),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1996") #25


###heath
heathN <- CallSites_N(heath)
heathN_int <- make_interval_pol(heathN, 100, 10000)
EricaN <- rowSums(heathN_int[c(12:15)])
heaN <- rowSums(heathN_int[3:ncol(heathN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathN_int$lower_ends, LCC=EricaN)

#ecp
ecp_area <- data.frame(age=coniferousN_int$lower_ends, conN, decN, wetwN, wetmN, pasN, araN, heaN)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpN <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpN, "North_ecp")

#countTaxa
CountTaxaPerInterval(coniferousN, 100)


### SOUTHEAST ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousSE <- CallSites_SE(coniferous_woodland)
coniferousSE_int <- make_interval_pol(coniferousSE, 100, 10000)
PinusPiceaSE <- rowSums(coniferousSE_int[12:15])
conSE <- rowSums(coniferousSE_int[3:ncol(coniferousSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousSE_int$lower_ends, LCC=conSE)
plot(mcp_area)
# I4
model = list(LCC~1, ~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=142") #35
hypothesis(fit_mcp, "cp_2=1389") #11
hypothesis(fit_mcp, "cp_3=2675") #0.4
hypothesis(fit_mcp, "cp_4=4618") #1
# I3
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=151") #52
hypothesis(fit_mcp, "cp_2=1537") #33
hypothesis(fit_mcp, "cp_3=3453") #125
# J2
model = list(LCC~1, ~0+age, 0+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1929") #2
hypothesis(fit_mcp, "cp_2=5131") #10

# count sites
count <- countSitesPerInterval(coniferousSE, 100)
count <- count[c(1:102),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I3
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=155") #57
hypothesis(fit_mcp, "cp_2=1536") #36
hypothesis(fit_mcp, "cp_3=3453") #125


###deciduous_woodland
deciduousSE <- CallSites_SE(deciduous_woodland)
deciduousSE_int <- make_interval_pol(deciduousSE, 100, 10000)
#BetulaSE <- rowSums(deciduousSE_int[7:13])
decSE <- rowSums(deciduousSE_int[3:ncol(deciduousSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousSE_int$lower_ends, LCC=decSE)

###wet_woodland
wetwoodlandSE <- CallSites_SE(wet_woodland)
wetwoodlandSE_int <- make_interval_pol(wetwoodlandSE, 100, 10000)
AlnusSalixSE <- rowSums(wetwoodlandSE_int[c(3,4,5,6,23)])
wetwSE <- rowSums(wetwoodlandSE_int[3:ncol(wetwoodlandSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandSE_int$lower_ends, LCC=AlnusSalixSE)

###wet_meadow
wetmeadowSE <- CallSites_SE(wet_meadow)
wetmeadowSE_int <- make_interval_pol(wetmeadowSE, 100, 10000)
wetmSE <- rowSums(wetmeadowSE_int[3:ncol(wetmeadowSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowSE_int$lower_ends, LCC=wetmSE)

###pasture
pastureSE <- CallSites_SE(pasture)
pastureSE_int <- make_interval_pol(pastureSE, 100, 10000)
#PoaceaeSE
pasSE <- rowSums(pastureSE_int[3:ncol(pastureSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureSE_int$lower_ends, LCC=pasSE)

###arable
arableSE <- CallSites_SE(arable)
arableSE_int <- make_interval_pol(arableSE, 100, 10000)
araSE <- rowSums(arableSE_int[3:ncol(arableSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableSE_int$lower_ends, LCC=araSE)
mcp_area <- mcp_area[c(1:82),]
plot(mcp_area)
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=917") #64
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=350") #48
hypothesis(fit_mcp, "cp_2=650") #248

# count sites
count <- countSitesPerInterval(arableSE, 100)
count <- count[c(1:82),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=351") #47
hypothesis(fit_mcp, "cp_2=652") #289


###heath
heathSE <- CallSites_SE(heath)
heathSE_int <- make_interval_pol(heathN, 100, 10000)
EricaSE <- rowSums(heathSE_int[c(12:15)])
heaSE <- rowSums(heathSE_int[3:ncol(heathSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathSE_int$lower_ends, LCC=EricaSE)

#ecp
ecp_area <- data.frame(age=coniferousSE_int$lower_ends, conSE, decSE, wetwSE, wetmSE, pasSE, araSE, heaSE)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpSE <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpSE, "SouthEast_ecp")


### MIDWEST ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousMW <- CallSites_MW(coniferous_woodland)
coniferousMW_int <- make_interval_pol(coniferousMW, 100, 10000)
PinusPiceaMW <- rowSums(coniferousMW_int[12:15])
conMW <- rowSums(coniferousMW_int[3:ncol(coniferousMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousMW_int$lower_ends, LCC=conMW)
plot(mcp_area)
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=4312") #0.4

# count sites
count <- countSitesPerInterval(coniferousMW, 100)
count <- count[c(1:102),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=4952") #0.8
# JS1
model = list(LCC~1, ~0+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=6083") #1


###deciduous_woodland
deciduousMW <- CallSites_MW(deciduous_woodland)
deciduousMW_int <- make_interval_pol(deciduousMW, 100, 10000)
BetulaMW <- rowSums(deciduousMW_int[7:13])
#decMW <- rowSums(deciduousMW_int[3:ncol(deciduousMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousMW_int$lower_ends, LCC=BetulaMW)

###wet_woodland
wetwoodlandMW <- CallSites_MW(wet_woodland)
wetwoodlandMW_int <- make_interval_pol(wetwoodlandMW, 100, 10000)
AlnusSalixMW <- rowSums(wetwoodlandMW_int[c(3,4,5,6,23)])
#wetwMW <- rowSums(wetwoodlandMW_int[3:ncol(wetwoodlandMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandMW_int$lower_ends, LCC=wetwMW)

###wet_meadow
wetmeadowMW <- CallSites_MW(wet_meadow)
wetmeadowMW_int <- make_interval_pol(wetmeadowMW, 100, 10000)
wetmMW <- rowSums(wetmeadowMW_int[3:ncol(wetmeadowMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowMW_int$lower_ends, LCC=wetmMW)

###pasture
pastureMW <- CallSites_MW(pasture)
pastureMW_int <- make_interval_pol(pastureMW, 100, 10000)
#PoaceaeMW
pasMW <- rowSums(pastureMW_int[3:ncol(pastureMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureMW_int$lower_ends, LCC=pasMW)

###arable
arableMW <- CallSites_MW(arable)
arableMW_int <- make_interval_pol(arableMW, 100, 10000)
araMW <- rowSums(arableMW_int[3:ncol(arableMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableMW_int$lower_ends, LCC=araMW)
mcp_area <- mcp_area[c(1:82),]
plot(mcp_area)
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1145") #0.5
hypothesis(fit_mcp, "cp_2=3077") #1
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=2450") #27

# count sites
count <- countSitesPerInterval(arableMW, 100)
count <- count[c(1:82),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=2446") #46


###heath
heathMW <- CallSites_MW(heath)
heathMW_int <- make_interval_pol(heathMW, 100, 10000)
EricaMW <- rowSums(heathMW_int[c(12:15)])
#heaMW <- rowSums(heathMW_int[3:ncol(heathMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathMW_int$lower_ends, LCC=EricaMW)

#ecp
ecp_area <- data.frame(age=coniferousMW_int$lower_ends, conMW, decMW, wetwMW, wetmMW, pasMW, araMW, heaMW)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpMW <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpSE, "MidWest_ecp")


### MIDMID ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousMM <- CallSites_MM(coniferous_woodland)
coniferousMM_int <- make_interval_pol(coniferousMM, 100, 10000)
PinusPiceaMM <- rowSums(coniferousMM_int[12:15])
conMM <- rowSums(coniferousMM_int[3:ncol(coniferousMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousMM_int$lower_ends, LCC=conMM)
plot(mcp_area)
# I2
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=4211") #34

# count sites
count <- countSitesPerInterval(coniferousMM, 100)
count <- count[c(1:102),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I1
model = list(LCC~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=4209") #34

###deciduous_woodland
deciduousMM <- CallSites_MM(deciduous_woodland)
deciduousMM_int <- make_interval_pol(deciduousMM, 100, 10000)
#BetulaMM <- rowSums(deciduousMM_int[7:13])
decMM <- rowSums(deciduousMM_int[3:ncol(deciduousMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousMM_int$lower_ends, LCC=decMM)

###wet_woodland
wetwoodlandMM <- CallSites_MM(wet_woodland)
wetwoodlandMM_int <- make_interval_pol(wetwoodlandMM, 100, 10000)
AlnusSalixMM <- rowSums(wetwoodlandMM_int[c(3,4,5,6,23)])
wetwMM <- rowSums(wetwoodlandMM_int[3:ncol(wetwoodlandMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandMM_int$lower_ends, LCC=wetwMM)

###wet_meadow
wetmeadowMM <- CallSites_MM(wet_meadow)
wetmeadowMM_int <- make_interval_pol(wetmeadowSE, 100, 10000)
wetmMM <- rowSums(wetmeadowMM_int[3:ncol(wetmeadowMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowMM_int$lower_ends, LCC=wetmMM)

###pasture
pastureMM <- CallSites_MM(pasture)
pastureMM_int <- make_interval_pol(pastureMM, 100, 10000)
#PoaceaeMM
pasMM <- rowSums(pastureMM_int[3:ncol(pastureMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureMM_int$lower_ends, LCC=pastureMM_int$Poaceae)

###arable
arableMM <- CallSites_MM(arable)
arableMM_int <- make_interval_pol(arableMM, 100, 10000)
araMM <- rowSums(arableMM_int[3:ncol(arableMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableMM_int$lower_ends, LCC=araMM)
mcp_area <- mcp_area[c(1:82),]
plot(mcp_area)
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=505") #21
hypothesis(fit_mcp, "cp_2=1583") #28
# I3
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=105") #38
hypothesis(fit_mcp, "cp_2=450") #213
hypothesis(fit_mcp, "cp_3=1503") #116

# count sites
count <- countSitesPerInterval(arableMM, 100)
count <- count[c(1:82),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I3
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=105") #38
hypothesis(fit_mcp, "cp_2=450") #213
hypothesis(fit_mcp, "cp_3=1503") #116


###heath
heathMM <- CallSites_MM(heath)
heathMM_int <- make_interval_pol(heathMM, 100, 10000)
#EricaMM <- rowSums(heathMM_int[c(12:15)])
heaMM <- rowSums(heathMM_int[3:ncol(heathMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathMM_int$lower_ends, LCC=heaMM)

#ecp
ecp_area <- data.frame(age=coniferousMM_int$lower_ends, conMM, decMM, wetwMM, wetmMM, pasMM, araMM, heaMM)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpMM <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpMM, "MidMid_ecp")


### SOUTHWEST ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousSW <- CallSites_SW(coniferous_woodland)
coniferousSW_int <- make_interval_pol(coniferousSW, 100, 10000)
PinusPiceaSW <- rowSums(coniferousSW_int[12:15])
conSW <- rowSums(coniferousSW_int[3:ncol(coniferousSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousSW_int$lower_ends, LCC=conSW)
plot(mcp_area)
# I
model = list(LCC~1, ~0+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)

# count sites
count <- countSitesPerInterval(coniferousSW, 100)
count <- count[c(1:102),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=5)
plot(mcp_area$age, mcp_area$LCC)
# I
model = list(LCC~1, ~1+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=2653") #0.7


###deciduous_woodland
deciduousSW <- CallSites_SW(deciduous_woodland)
deciduousSW_int <- make_interval_pol(deciduousSW, 100, 10000)
BetulaSW <- rowSums(deciduousSW_int[7:13])
#decSW <- rowSums(deciduousSW_int[3:ncol(deciduousSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousSW_int$lower_ends, LCC=BetulaSW)

###wet_woodland
wetwoodlandSW <- CallSites_SW(wet_woodland)
wetwoodlandSW_int <- make_interval_pol(wetwoodlandSW, 100, 10000)
AlnusSalixSW <- rowSums(wetwoodlandSW_int[c(3,4,5,6,23)])
wetwSW <- rowSums(wetwoodlandSW_int[3:ncol(wetwoodlandSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandSW_int$lower_ends, LCC=AlnusSalixSW)

###wet_meadow
wetmeadowSW <- CallSites_SW(wet_meadow)
wetmeadowSW_int <- make_interval_pol(wetmeadowSW, 100, 10000)
wetmSW <- rowSums(wetmeadowSW_int[3:ncol(wetmeadowSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowSW_int$lower_ends, LCC=wetmSW)

###pasture
pastureSW <- CallSites_SW(pasture)
pastureSW_int <- make_interval_pol(pastureSW, 100, 10000)
#PoaceaeSW
pasSW <- rowSums(pastureSW_int[3:ncol(pastureSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureSW_int$lower_ends, LCC=pasSW)

###arable
arableSW <- CallSites_SW(arable)
arableSW_int <- make_interval_pol(arableSW, 100, 10000)
araSW <- rowSums(arableSW_int[3:ncol(arableSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableSW_int$lower_ends, LCC=araSW)
mcp_area <- mcp_area[c(1:82),]
plot(mcp_area)
# I3
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=-49") #58
hypothesis(fit_mcp, "cp_2=1150") #101
hypothesis(fit_mcp, "cp_3=3139") #4

# count sites
count <- countSitesPerInterval(arableSW, 100)
count <- count[c(1:82),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1150") #48
hypothesis(fit_mcp, "cp_2=3193") #2.8


###heath
heathSW <- CallSites_SW(heath)
heathSW_int <- make_interval_pol(heathSW, 100, 10000)
EricaSW <- rowSums(heathSW_int[c(12:15)])
heaSW <- rowSums(heathSW_int[3:ncol(heathSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathSW_int$lower_ends, LCC=heaSW)

#ecp
ecp_area <- data.frame(age=coniferousSW_int$lower_ends, conSW, decSW, wetwSW, wetmSW, pasSW, araSW, heaSW)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpSW <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpSW, "SouthWest_ecp")


### SOUTHMID ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousSM <- CallSites_SM(coniferous_woodland)
coniferousSM_int <- make_interval_pol(coniferousSM, 100, 10000)
PinusPiceaSM <- rowSums(coniferousSM_int[12:15])
conSM <- rowSums(coniferousSM_int[3:ncol(coniferousSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousSM_int$lower_ends, LCC=conSM)
plot(mcp_area)
# I3
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=983") #44
hypothesis(fit_mcp, "cp_2=1420") #0.6
hypothesis(fit_mcp, "cp_3=8276") #18
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1532") #8
hypothesis(fit_mcp, "cp_2=8195") #11

# count sites
count <- countSitesPerInterval(coniferousSM, 100)
count <- count[c(1:102),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1116") #25
hypothesis(fit_mcp, "cp_2=8325") #32


###deciduous_woodland
deciduousSM <- CallSites_SM(deciduous_woodland)
deciduousSM_int <- make_interval_pol(deciduousSM, 100, 10000)
BetulaSM <- rowSums(deciduousSM_int[7:13])
decSM <- rowSums(deciduousSM_int[3:ncol(deciduousSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousSM_int$lower_ends, LCC=BetulaSM)

###wet_woodland
wetwoodlandSM <- CallSites_SM(wet_woodland)
wetwoodlandSM_int <- make_interval_pol(wetwoodlandSM, 100, 10000)
AlnusSalixSM <- rowSums(wetwoodlandSM_int[c(3,4,5,6,23)])
wetwSM <- rowSums(wetwoodlandSM_int[3:ncol(wetwoodlandSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandSM_int$lower_ends, LCC=AlnusSalixSM)

###wet_meadow
wetmeadowSM <- CallSites_SM(wet_meadow)
wetmeadowSM_int <- make_interval_pol(wetmeadowSM, 100, 10000)
wetmSM <- rowSums(wetmeadowSM_int[3:ncol(wetmeadowSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowSM_int$lower_ends, LCC=wetmSM)

###pasture
pastureSM <- CallSites_SM(pasture)
pastureSM_int <- make_interval_pol(pastureSM, 100, 10000)
#PoaceaeSM
pasSM <- rowSums(pastureSM_int[3:ncol(pastureSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureSM_int$lower_ends, LCC=pasSM)

###arable
arableSM <- CallSites_SM(arable)
arableSM_int <- make_interval_pol(arableSM, 100, 10000)
araSM <- rowSums(arableSM_int[3:ncol(arableSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableSM_int$lower_ends, LCC=araSM)
mcp_area <- mcp_area[c(1:82),]
plot(mcp_area)
# I3
model = list(LCC~1, ~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=550") #36
hypothesis(fit_mcp, "cp_2=1402") #24
hypothesis(fit_mcp, "cp_3=2141") #2
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=550") #46
hypothesis(fit_mcp, "cp_2=1581") #90
# JS2
model = list(LCC~1, ~0+age, ~0+age)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1168") #7
hypothesis(fit_mcp, "cp_2=4499") #0.66

# count sites
count <- countSitesPerInterval(arableSM, 100)
count <- count[c(1:82),]
mcp_area$NRsites <- count$NRsites
mcp_area <- subset(mcp_area, NRsites >=3)
plot(mcp_area$age, mcp_area$LCC)
# I2
model = list(LCC~1, ~1, ~1)
fit_mcp = mcp(model, data = mcp_area, par_x = "age", adapt = 5000, sample = "both", iter = 10000)
summary(fit_mcp)
plot(fit_mcp)
hypothesis(fit_mcp, "cp_1=1300") #21
hypothesis(fit_mcp, "cp_2=2433") #16


###heath
heathSM <- CallSites_SM(heath)
heathSM_int <- make_interval_pol(heathSM, 100, 10000)
EricaSM <- rowSums(heathSM_int[c(12:15)])
heaSM <- rowSums(heathSM_int[3:ncol(heathSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathSM_int$lower_ends, LCC=heaSM)

#ecp
ecp_area <- data.frame(age=coniferousSM_int$lower_ends, conSM, decSM, wetwSM, wetmSM, pasSM, araSM, heaSM)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpSM <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpSW, "SouthMid_ecp")
