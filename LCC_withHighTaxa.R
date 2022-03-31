
###N-coniferous_woodland
coniferousN <- CallSites_N(coniferous_woodland)
coniferousN_int <- make_interval_pol(coniferousN, 100, 10000)
#mcp
PinusPiceaN <- rowSums(coniferousN_int[,c(12:15)]) #only Pinus and Picea
conN <- rowSums(coniferousN_int[3:18], na.rm = TRUE) #all coniferous taxa
mcp_area <- data.frame(age=coniferousN_int$lower_ends, LCC=PinusPiceaN)
mcp_LCC(mcp_area)
#ecp
ecp_area <- data.frame(age=coniferousN_int$lower_ends, LCC=conN)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt) 
ecp_area <- data.frame(age=coniferousN_int$lower_ends[2:102], LCC=ecp_inc)
conN_ecp <- ecp_sums(ecp_area, "ecp_area")

###N-deciduous
deciduousN <- CallSites_N(deciduous_woodland)
deciduousN_int <- make_interval_pol(deciduousN, 100, 10000)
BetulaN <- rowSums(deciduousN_int[,c(7:13)])
mcp_area <- data.frame(age=deciduousN_int$lower_ends, LCC=BetulaN)
mcp_LCC(mcp_area)

pastureN <- CallSites_N(pasture)
pastureN_int <- make_interval_pol(pastureN, 100, 10000)
#BetulaN <- rowSums(pastureN_int[,c(7:13)])
pasN <- rowSums(pastureN_int[3:91])
mcp_area <- data.frame(age=pastureN_int$lower_ends, LCC=pasN)
mcp_LCC(mcp_area)

arableN <- CallSites_N(arable)
arableN_int <- make_interval_pol(arableN, 100, 10000)
#BetulaN <- rowSums(arableN_int[,c(7:13)])
araN <- rowSums(arableN_int[3:ncol(arableN_int)])
mcp_area <- data.frame(age=arableN_int$lower_ends, LCC=araN)
mcp_LCC(mcp_area)


###S
coniferousSW <- CallSites_SW(coniferous_woodland)
coniferousSW_int <- make_interval_pol(coniferousSW, 100, 10000)
PinusPiceaSW <- rowSums(coniferousSW_int[,c(12:15)])
mcp_area <- data.frame(age=coniferousSW_int$lower_ends, LCC=PinusPiceaSW)
mcp_LCC(mcp_area)

coniferousS <- CallSites_S(coniferous_woodland)
coniferousS_int <- make_interval_pol(coniferousS, 100, 10000)
PinusPiceaS <- rowSums(coniferousS_int[,c(12:15)])
mcp_area <- data.frame(age=coniferousS_int$lower_ends, LCC=PinusPiceaS)
mcp_LCC(mcp_area)

coniferousSM <- CallSites_SM(coniferous_woodland)
coniferousSM_int <- make_interval_pol(coniferousSM, 100, 10000)
PinusPiceaSM <- rowSums(coniferousSM_int[,c(12:15)])
mcp_area <- data.frame(age=coniferousSM_int$lower_ends, LCC=PinusPiceaSM)
mcp_LCC(mcp_area)

deciduousSM <- CallSites_SM(deciduous_woodland)
deciduousSM_int <- make_interval_pol(deciduousSM, 100, 10000)
BetulaSM <- rowSums(deciduousSM_int[,c(7:13)])
mcp_area <- data.frame(age=deciduousSM_int$lower_ends, LCC=BetulaSM)
mcp_LCC(mcp_area)

pastureSM <- CallSites_SM(pasture)
pastureSM_int <- make_interval_pol(pastureSM, 100, 10000)
#BetulaN <- rowSums(pastureN_int[,c(7:13)])
pasSM <- rowSums(pastureSM_int[3:91])
mcp_area <- data.frame(age=pastureSM_int$lower_ends, LCC=pasSM)
mcp_LCC(mcp_area)

