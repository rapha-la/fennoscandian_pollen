### single LCC mcp ###

### NORTH ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousN <- CallSites_N(coniferous_woodland)
coniferousN_int <- make_interval_pol(coniferousN, 100, 10000)
#coniferousN_int <- coniferousN_int[-c(73:nrow(coniferousN_int)),] #only until 7000 BP
#PinusPiceaN <- rowSums(coniferousN_int[12:15]) #only Pinus and Picea
conN <- rowSums(coniferousN_int[3:18], na.rm = TRUE) #all coniferous taxa
mcp_area <- data.frame(age=coniferousN_int$lower_ends, LCC=conN) #LCC = PinusPiceaN or conN
mcp2_LCC(mcp_area)
# 1400(1.0) + 7700(1.1) BP

###deciduous_woodland
deciduousN <- CallSites_N(deciduous_woodland)
deciduousN_int <- make_interval_pol(deciduousN, 100, 10000)
BetulaN <- rowSums(deciduousN_int[7:13])
decN <- rowSums(deciduousN_int[3:ncol(deciduousN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousN_int$lower_ends, LCC=decN)
mcp2_LCC(mcp_area)
#no cp

###wet_woodland
wetwoodlandN <- CallSites_N(wet_woodland)
wetwoodlandN_int <- make_interval_pol(wetwoodlandN, 100, 10000)
AlnusSalixN <- rowSums(wetwoodlandN_int[c(3,4,5,6,23)])
wetwN <- rowSums(wetwoodlandN_int[3:ncol(wetwoodlandN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandN_int$lower_ends, LCC=wetwN)
mcp2_LCC(mcp_area)
#8100(1.0) +5400(1.3)

###wet_meadow
wetmeadowN <- CallSites_N(wet_meadow)
wetmeadowN_int <- make_interval_pol(wetmeadowN, 100, 10000)
wetmN <- rowSums(wetmeadowN_int[3:ncol(wetmeadowN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowN_int$lower_ends, LCC=wetmN)
mcp2_LCC(mcp_area)

###pasture
pastureN <- CallSites_N(pasture)
pastureN_int <- make_interval_pol(pastureN, 100, 10000)
#PoaceaeN
pasN <- rowSums(pastureN_int[3:ncol(pastureN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureN_int$lower_ends, LCC=pastureN_int$Poaceae)
mcp2_LCC(mcp_area)

###arable
arableN <- CallSites_N(arable)
arableN_int <- make_interval_pol(arableN, 100, 10000)
araN <- rowSums(arableN_int[3:ncol(arableN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableN_int$lower_ends, LCC=araN)
mcp2_LCC(mcp_area)

###heath
heathN <- CallSites_N(heath)
heathN_int <- make_interval_pol(heathN, 100, 10000)
EricaN <- rowSums(heathN_int[c(12:15)])
heaN <- rowSums(heathN_int[3:ncol(heathN_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathN_int$lower_ends, LCC=EricaN)
mcp2_LCC(mcp_area)

#ecp
ecp_area <- data.frame(age=coniferousN_int$lower_ends, conN, decN, wetwN, wetmN, pasN, araN, heaN)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpN <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpN, "North_ecp")

#countTaxa
CountTaxaPerInterval(coniferousN)

### SOUTHEAST ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousSE <- CallSites_SE(coniferous_woodland)
coniferousSE_int <- make_interval_pol(coniferousSE, 100, 10000)
#PinusPiceaSE <- rowSums(coniferousSE_int[12:15])
conSE <- rowSums(coniferousSE_int[3:ncol(coniferousSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousSE_int$lower_ends, LCC=PinusPiceaSE)
mcp2_LCC(mcp_area)
#one cp at 3300 BP

###deciduous_woodland
deciduousSE <- CallSites_SE(deciduous_woodland)
deciduousSE_int <- make_interval_pol(deciduousSE, 100, 10000)
#BetulaSE <- rowSums(deciduousSE_int[7:13])
decSE <- rowSums(deciduousSE_int[3:ncol(deciduousSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousSE_int$lower_ends, LCC=decSE)
mcp2_LCC(mcp_area)

###wet_woodland
wetwoodlandSE <- CallSites_SE(wet_woodland)
wetwoodlandSE_int <- make_interval_pol(wetwoodlandSE, 100, 10000)
AlnusSalixSE <- rowSums(wetwoodlandSE_int[c(3,4,5,6,23)])
wetwSE <- rowSums(wetwoodlandSE_int[3:ncol(wetwoodlandSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandSE_int$lower_ends, LCC=AlnusSalixSE)
mcp2_LCC(mcp_area)

###wet_meadow
wetmeadowSE <- CallSites_SE(wet_meadow)
wetmeadowSE_int <- make_interval_pol(wetmeadowSE, 100, 10000)
wetmSE <- rowSums(wetmeadowSE_int[3:ncol(wetmeadowSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowSE_int$lower_ends, LCC=wetmSE)
mcp2_LCC(mcp_area)

###pasture
pastureSE <- CallSites_SE(pasture)
pastureSE_int <- make_interval_pol(pastureSE, 100, 10000)
#PoaceaeSE
pasSE <- rowSums(pastureSE_int[3:ncol(pastureSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureSE_int$lower_ends, LCC=pasSE)
mcp2_LCC(mcp_area)

###arable
arableSE <- CallSites_SE(arable)
arableSE_int <- make_interval_pol(arableSE, 100, 10000)
araSE <- rowSums(arableSE_int[3:ncol(arableSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableSE_int$lower_ends, LCC=araSE)
mcp2_LCC(mcp_area)

###heath
heathSE <- CallSites_SE(heath)
heathSE_int <- make_interval_pol(heathN, 100, 10000)
EricaSE <- rowSums(heathSE_int[c(12:15)])
heaSE <- rowSums(heathSE_int[3:ncol(heathSE_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathSE_int$lower_ends, LCC=EricaSE)
mcp2_LCC(mcp_area)

#ecp
ecp_area <- data.frame(age=coniferousSE_int$lower_ends, conSE, decSE, wetwSE, wetmSE, pasSE, araSE, heaSE)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpSE <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpSE, "SouthEast_ecp")


### MIDWEST ###
#-------------------------------------------------------------------------------
###coniferous_woodland #bad convergence
coniferousMW <- CallSites_MW(coniferous_woodland)
coniferousMW_int <- make_interval_pol(coniferousMW, 100, 10000)
PinusPiceaMW <- rowSums(coniferousMW_int[12:15])
conMW <- rowSums(coniferousMW_int[3:ncol(coniferousMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousMW_int$lower_ends, LCC=PinusPiceaMW)
mcp2_LCC(mcp_area)

###deciduous_woodland
deciduousMW <- CallSites_MW(deciduous_woodland)
deciduousMW_int <- make_interval_pol(deciduousMW, 100, 10000)
BetulaMW <- rowSums(deciduousMW_int[7:13])
#decMW <- rowSums(deciduousMW_int[3:ncol(deciduousMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousMW_int$lower_ends, LCC=BetulaMW)
mcp2_LCC(mcp_area)

###wet_woodland
wetwoodlandMW <- CallSites_MW(wet_woodland)
wetwoodlandMW_int <- make_interval_pol(wetwoodlandMW, 100, 10000)
AlnusSalixMW <- rowSums(wetwoodlandMW_int[c(3,4,5,6,23)])
#wetwMW <- rowSums(wetwoodlandMW_int[3:ncol(wetwoodlandMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandMW_int$lower_ends, LCC=wetwMW)
mcp2_LCC(mcp_area)

###wet_meadow
wetmeadowMW <- CallSites_MW(wet_meadow)
wetmeadowMW_int <- make_interval_pol(wetmeadowMW, 100, 10000)
wetmMW <- rowSums(wetmeadowMW_int[3:ncol(wetmeadowMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowMW_int$lower_ends, LCC=wetmMW)
mcp2_LCC(mcp_area)

###pasture
pastureMW <- CallSites_MW(pasture)
pastureMW_int <- make_interval_pol(pastureMW, 100, 10000)
#PoaceaeMW
pasMW <- rowSums(pastureMW_int[3:ncol(pastureMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureMW_int$lower_ends, LCC=pasMW)
mcp2_LCC(mcp_area)

###arable
arableMW <- CallSites_MW(arable)
arableMW_int <- make_interval_pol(arableMW, 100, 10000)
araMW <- rowSums(arableMW_int[3:ncol(arableMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableMW_int$lower_ends, LCC=araMW)
mcp2_LCC(mcp_area)

###heath
heathMW <- CallSites_MW(heath)
heathMW_int <- make_interval_pol(heathMW, 100, 10000)
EricaMW <- rowSums(heathMW_int[c(12:15)])
#heaMW <- rowSums(heathMW_int[3:ncol(heathMW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathMW_int$lower_ends, LCC=EricaMW)
mcp2_LCC(mcp_area)

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
mcp_area <- data.frame(age=coniferousMM_int$lower_ends, LCC=PinusPiceaMM)
mcp2_LCC(mcp_area)

###deciduous_woodland
deciduousMM <- CallSites_MM(deciduous_woodland)
deciduousMM_int <- make_interval_pol(deciduousMM, 100, 10000)
#BetulaMM <- rowSums(deciduousMM_int[7:13])
decMM <- rowSums(deciduousMM_int[3:ncol(deciduousMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousMM_int$lower_ends, LCC=decMM)
mcp2_LCC(mcp_area)

###wet_woodland
wetwoodlandMM <- CallSites_MM(wet_woodland)
wetwoodlandMM_int <- make_interval_pol(wetwoodlandMM, 100, 10000)
AlnusSalixMM <- rowSums(wetwoodlandMM_int[c(3,4,5,6,23)])
wetwMM <- rowSums(wetwoodlandMM_int[3:ncol(wetwoodlandMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandMM_int$lower_ends, LCC=wetwMM)
mcp2_LCC(mcp_area)

###wet_meadow
wetmeadowMM <- CallSites_MM(wet_meadow)
wetmeadowMM_int <- make_interval_pol(wetmeadowSE, 100, 10000)
wetmMM <- rowSums(wetmeadowMM_int[3:ncol(wetmeadowMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowMM_int$lower_ends, LCC=wetmMM)
mcp2_LCC(mcp_area)

###pasture
pastureMM <- CallSites_MM(pasture)
pastureMM_int <- make_interval_pol(pastureMM, 100, 10000)
#PoaceaeMM
pasMM <- rowSums(pastureMM_int[3:ncol(pastureMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureMM_int$lower_ends, LCC=pastureMM_int$Poaceae)
mcp2_LCC(mcp_area)

###arable
arableMM <- CallSites_MM(arable)
arableMM_int <- make_interval_pol(arableMM, 100, 10000)
araMM <- rowSums(arableMM_int[3:ncol(arableMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableMM_int$lower_ends, LCC=araMM)
mcp2_LCC(mcp_area)

###heath
heathMM <- CallSites_MM(heath)
heathMM_int <- make_interval_pol(heathMM, 100, 10000)
#EricaMM <- rowSums(heathMM_int[c(12:15)])
heaMM <- rowSums(heathMM_int[3:ncol(heathMM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathMM_int$lower_ends, LCC=heaMM)
mcp2_LCC(mcp_area)

#ecp
ecp_area <- data.frame(age=coniferousMM_int$lower_ends, conMM, decMM, wetwMM, wetmMM, pasMM, araMM, heaMM)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpMM <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpMM, "MidMid_ecp")


### MID (MW+MM) ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousM <- CallSites_M(coniferous_woodland)
coniferousM_int <- make_interval_pol(coniferousM, 100, 10000)
PinusPiceaM <- rowSums(coniferousM_int[12:15])
conM <- rowSums(coniferousM_int[3:ncol(coniferousM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousM_int$lower_ends, LCC=PinusPiceaM)
mcp2_LCC(mcp_area)

###deciduous_woodland
deciduousM <- CallSites_M(deciduous_woodland)
deciduousM_int <- make_interval_pol(deciduousM, 100, 10000)
#BetulaMM <- rowSums(deciduousMM_int[7:13])
decM <- rowSums(deciduousM_int[3:ncol(deciduousM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousM_int$lower_ends, LCC=decM)
mcp2_LCC(mcp_area)

###wet_woodland
wetwoodlandM <- CallSites_M(wet_woodland)
wetwoodlandM_int <- make_interval_pol(wetwoodlandM, 100, 10000)
AlnusSalixM <- rowSums(wetwoodlandM_int[c(3,4,5,6,23)])
wetwM <- rowSums(wetwoodlandM_int[3:ncol(wetwoodlandM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandM_int$lower_ends, LCC=wetwM)
mcp2_LCC(mcp_area)

###wet_meadow
wetmeadowM <- CallSites_M(wet_meadow)
wetmeadowM_int <- make_interval_pol(wetmeadowM, 100, 10000)
wetmM <- rowSums(wetmeadowMM_int[3:ncol(wetmeadowM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowM_int$lower_ends, LCC=wetmM)
mcp2_LCC(mcp_area)

###pasture
pastureM <- CallSites_M(pasture)
pastureM_int <- make_interval_pol(pastureM, 100, 10000)
#PoaceaeMM
pasM <- rowSums(pastureM_int[3:ncol(pastureM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureM_int$lower_ends, LCC=pastureM_int$Poaceae)
mcp2_LCC(mcp_area)

###arable
arableM <- CallSites_M(arable)
arableM_int <- make_interval_pol(arableM, 100, 10000)
araM <- rowSums(arableM_int[3:ncol(arableM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableM_int$lower_ends, LCC=araM)
mcp2_LCC(mcp_area)

###heath
heathM <- CallSites_M(heath)
heathM_int <- make_interval_pol(heathM, 100, 10000)
#EricaM <- rowSums(heathM_int[c(12:15)])
heaM <- rowSums(heathM_int[3:ncol(heathM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathM_int$lower_ends, LCC=heaM)
mcp2_LCC(mcp_area)

#ecp
ecp_area <- data.frame(age=coniferousM_int$lower_ends, conM, decM, wetwM, wetmM, pasM, araM, heaM)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpM <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpM, "Mid_ecp")


### SOUTHWEST ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousSW <- CallSites_SW(coniferous_woodland)
coniferousSW_int <- make_interval_pol(coniferousSW, 100, 10000)
PinusPiceaSW <- rowSums(coniferousSW_int[12:15])
conSW <- rowSums(coniferousSW_int[3:ncol(coniferousSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousSW_int$lower_ends, LCC=conSW)
mcp2_LCC(mcp_area)

###deciduous_woodland
deciduousSW <- CallSites_SW(deciduous_woodland)
deciduousSW_int <- make_interval_pol(deciduousSW, 100, 10000)
BetulaSW <- rowSums(deciduousSW_int[7:13])
#decSW <- rowSums(deciduousSW_int[3:ncol(deciduousSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousSW_int$lower_ends, LCC=BetulaSW)
mcp2_LCC(mcp_area)

###wet_woodland
wetwoodlandSW <- CallSites_SW(wet_woodland)
wetwoodlandSW_int <- make_interval_pol(wetwoodlandSW, 100, 10000)
AlnusSalixSW <- rowSums(wetwoodlandSW_int[c(3,4,5,6,23)])
wetwSW <- rowSums(wetwoodlandSW_int[3:ncol(wetwoodlandSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandSW_int$lower_ends, LCC=AlnusSalixSW)
mcp2_LCC(mcp_area)

###wet_meadow
wetmeadowSW <- CallSites_SW(wet_meadow)
wetmeadowSW_int <- make_interval_pol(wetmeadowSW, 100, 10000)
wetmSW <- rowSums(wetmeadowSW_int[3:ncol(wetmeadowSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowSW_int$lower_ends, LCC=wetmSW)
mcp2_LCC(mcp_area)

###pasture
pastureSW <- CallSites_SW(pasture)
pastureSW_int <- make_interval_pol(pastureSW, 100, 10000)
#PoaceaeSW
pasSW <- rowSums(pastureSW_int[3:ncol(pastureSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureSW_int$lower_ends, LCC=pasSW)
mcp2_LCC(mcp_area)

###arable
arableSW <- CallSites_SW(arable)
arableSW_int <- make_interval_pol(arableSW, 100, 10000)
araSW <- rowSums(arableSW_int[3:ncol(arableSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableSW_int$lower_ends, LCC=araSW)
mcp2_LCC(mcp_area)

###heath
heathSW <- CallSites_SW(heath)
heathSW_int <- make_interval_pol(heathSW, 100, 10000)
EricaSW <- rowSums(heathSW_int[c(12:15)])
heaSW <- rowSums(heathSW_int[3:ncol(heathSW_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathSW_int$lower_ends, LCC=heaSW)
mcp2_LCC(mcp_area)

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
mcp_area <- data.frame(age=coniferousSM_int$lower_ends, LCC=PinusPiceaSM)
mcp2_LCC(mcp_area)

###deciduous_woodland
deciduousSM <- CallSites_SM(deciduous_woodland)
deciduousSM_int <- make_interval_pol(deciduousSM, 100, 10000)
BetulaSM <- rowSums(deciduousSM_int[7:13])
decSM <- rowSums(deciduousSM_int[3:ncol(deciduousSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousSM_int$lower_ends, LCC=BetulaSM)
mcp2_LCC(mcp_area)

###wet_woodland
wetwoodlandSM <- CallSites_SM(wet_woodland)
wetwoodlandSM_int <- make_interval_pol(wetwoodlandSM, 100, 10000)
AlnusSalixSM <- rowSums(wetwoodlandSM_int[c(3,4,5,6,23)])
wetwSM <- rowSums(wetwoodlandSM_int[3:ncol(wetwoodlandSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandSM_int$lower_ends, LCC=AlnusSalixSM)
mcp2_LCC(mcp_area)

###wet_meadow
wetmeadowSM <- CallSites_SM(wet_meadow)
wetmeadowSM_int <- make_interval_pol(wetmeadowSM, 100, 10000)
wetmSM <- rowSums(wetmeadowSM_int[3:ncol(wetmeadowSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowSM_int$lower_ends, LCC=wetmSM)
mcp2_LCC(mcp_area)

###pasture
pastureSM <- CallSites_SM(pasture)
pastureSM_int <- make_interval_pol(pastureSM, 100, 10000)
#PoaceaeSM
pasSM <- rowSums(pastureSM_int[3:ncol(pastureSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureSM_int$lower_ends, LCC=pasSM)
mcp2_LCC(mcp_area)

###arable
arableSM <- CallSites_SM(arable)
arableSM_int <- make_interval_pol(arableSM, 100, 10000)
araSM <- rowSums(arableSM_int[3:ncol(arableSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableSM_int$lower_ends, LCC=araSM)
mcp2_LCC(mcp_area)

###heath
heathSM <- CallSites_SM(heath)
heathSM_int <- make_interval_pol(heathSM, 100, 10000)
EricaSM <- rowSums(heathSM_int[c(12:15)])
heaSM <- rowSums(heathSM_int[3:ncol(heathSM_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathSM_int$lower_ends, LCC=heaSM)
mcp2_LCC(mcp_area)

#ecp
ecp_area <- data.frame(age=coniferousSM_int$lower_ends, conSM, decSM, wetwSM, wetmSM, pasSM, araSM, heaSM)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpSM <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpSW, "SouthMid_ecp")


### SOUTH (SW+SM) ###
#-------------------------------------------------------------------------------
###coniferous_woodland
coniferousS <- CallSites_S(coniferous_woodland)
coniferousS_int <- make_interval_pol(coniferousS, 100, 10000)
PinusPiceaS <- rowSums(coniferousS_int[12:15])
conS <- rowSums(coniferousS_int[3:ncol(coniferousS_int)], na.rm = TRUE)
mcp_area <- data.frame(age=coniferousS_int$lower_ends, LCC=conS)
mcp2_LCC(mcp_area)

###deciduous_woodland
deciduousS <- CallSites_S(deciduous_woodland)
deciduousS_int <- make_interval_pol(deciduousS, 100, 10000)
BetulaS <- rowSums(deciduousS_int[7:13])
decS <- rowSums(deciduousS_int[3:ncol(deciduousS_int)], na.rm = TRUE)
mcp_area <- data.frame(age=deciduousS_int$lower_ends, LCC=BetulaS)
mcp2_LCC(mcp_area)

###wet_woodland
wetwoodlandS <- CallSites_S(wet_woodland)
wetwoodlandS_int <- make_interval_pol(wetwoodlandS, 100, 10000)
AlnusSalixS <- rowSums(wetwoodlandS_int[c(3,4,5,6,23)])
wetwS <- rowSums(wetwoodlandS_int[3:ncol(wetwoodlandS_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetwoodlandS_int$lower_ends, LCC=wetwS)
mcp2_LCC(mcp_area)

###wet_meadow
wetmeadowS <- CallSites_S(wet_meadow)
wetmeadowS_int <- make_interval_pol(wetmeadowS, 100, 10000)
wetmS <- rowSums(wetmeadowS_int[3:ncol(wetmeadowS_int)], na.rm = TRUE)
mcp_area <- data.frame(age=wetmeadowS_int$lower_ends, LCC=wetmS)
mcp2_LCC(mcp_area)

###pasture
pastureS <- CallSites_S(pasture)
pastureS_int <- make_interval_pol(pastureS, 100, 10000)
#PoaceaeS
pasS <- rowSums(pastureS_int[3:ncol(pastureS_int)], na.rm = TRUE)
mcp_area <- data.frame(age=pastureS_int$lower_ends, LCC=pasS)
mcp2_LCC(mcp_area)

###arable
arableS <- CallSites_S(arable)
arableS_int <- make_interval_pol(arableS, 100, 10000)
araS <- rowSums(arableS_int[3:ncol(arableS_int)], na.rm = TRUE)
mcp_area <- data.frame(age=arableS_int$lower_ends, LCC=araS)
mcp2_LCC(mcp_area)

###heath
heathS <- CallSites_S(heath)
heathS_int <- make_interval_pol(heathS, 100, 10000)
EricaS <- rowSums(heathS_int[c(12:15)])
heaS <- rowSums(heathS_int[3:ncol(heathS_int)], na.rm = TRUE)
mcp_area <- data.frame(age=heathS_int$lower_ends, LCC=heaS)
mcp2_LCC(mcp_area)

#ecp
ecp_area <- data.frame(age=coniferousS_int$lower_ends, conS, decS, wetwS, wetmS, pasS, araS, heaS)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt)
ecpS <- ecp_sums(ecp_area, "ecp_area")
plot_LCCecp(ecp_area, ecpS, "South_ecp")
