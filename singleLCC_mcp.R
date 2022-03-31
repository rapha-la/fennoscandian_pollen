### mcp on single LCCs ###

#countSitesPerInterval()
#CountTaxaPerInterval()

### North_15 ###
#-------------------------------------------------------------------------------
### POL ###
pol15_all <- CallSites_N(LCC)
pol15_int <- make_interval_pol(pol15_all, 100, 10000)
#coniferous_woodland /1.1
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$coniferous_woodland_sum)
mcp_area$NRsites <- siteNR$NRsites
mcp9 <- subset(mcp_area, NRsites >=9)
mcp_LCC(mcp_area)
#decidious_woodland /1.0
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$deciduous_woodland_sum)
mcp_area$NRsites <- siteNR$NRsites
mcp9 <- subset(mcp_area, NRsites >=9)
mcp_LCC(mcp_area)
#wet_woodland /9.0
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$wet_woodland_sum)
mcp_area$NRsites <- siteNR$NRsites
mcp9 <- subset(mcp_area, NRsites >=9)
mcp_LCC(mcp_area)
#wet_meadow /1.2
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$wet_meadow_sum)
mcp_LCC(mcp_area)
#pasture/meadow /1.5
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$pasture_sum)
mcp_LCC(mcp_area)
#arable /1.1
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$arable_sum)
mcp_area$NRsites <- siteNR$NRsites
mcp9 <- subset(mcp_area, NRsites >=9)
mcp_LCC(mcp_area)
#heath /9.9
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$heath_sum)
mcp_LCC(mcp_area)

### ecp ###
coniferousN <- CallSites_N(coniferous_woodland)
coniferousN_int <- make_interval_pol(coniferousN, 100, 10000)
coniferousN_ecp <- ecp_inc(coniferousN_int, "coniferousN_int")

CountTaxaPerInterval(coniferousN, 100)
siteNR = countSitesPerInterval(coniferousN, 100)
siteNR <- siteNR[-c(103:nrow(siteNR)),]
lessthan4 <- subset(siteNR, NRsites < 4) #Less than 4: 1, 3, 90, 93, 102
morethan9 <- subset(siteNR, NRsites >=9)
morethan9 <- c(morethan9$lower_ends)

### single sites ###
#site 20 #bad convergence without intervals (1.4) #with intervals: 1.1
mcp_site <- LCC[LCC$dataset_ID==20,]
  dataset_interval <- mcp_site
  dataset_interval$age = floor(dataset_interval$meantimes/interval)*interval
  dataset_interval <- dataset_interval %>%
    group_by(age) %>%
    summarise(
      LCC = sum(coniferous_woodland_sum))
mcp_site <- dataset_interval[-c(103:nrow(dataset_interval)),]
mcp_LCC(mcp_site)


### SouthWest_1 ###
#-------------------------------------------------------------------------------
### POL ###
pol1_all <- CallSites_SW(LCC)
pol1_int = make_interval_pol(pol1_all, 100, 10000)
#coniferous_woodland /1
mcp_area <- data.frame(age=pol1_int$lower_ends, LCC=pol1_int$coniferous_woodland_sum)
mcp_LCC(mcp_area)
#decidious_woodland /1
mcp_area <- data.frame(age=pol1_int$lower_ends, LCC=pol1_int$deciduous_woodland_sum)
mcp_LCC(mcp_area)
#wet_woodland /1.6
mcp_area <- data.frame(age=pol1_int$lower_ends, LCC=pol1_int$wet_woodland_sum)
mcp_LCC(mcp_area)
#wet_meadow /1.8
mcp_area <- data.frame(age=pol1_int$lower_ends, LCC=pol1_int$wet_meadow_sum)
mcp_LCC(mcp_area)
#pasture/meadow /1.4
mcp_area <- data.frame(age=pol1_int$lower_ends, LCC=pol1_int$pasture_sum)
mcp_LCC(mcp_area)
#arable /1
mcp_area <- data.frame(age=pol1_int$lower_ends, LCC=pol1_int$arable_sum)
mcp_LCC(mcp_area)
#heath /1.1
mcp_area <- data.frame(age=pol1_int$lower_ends, LCC=pol1_int$heath_sum)
mcp_LCC(mcp_area)

ecp_pol1 <- ecp_inc(pol1_int, "pol1_int")
plot_LCC(pol1_int, ecp_pol1, "LCC_SouthWest")


### MidWest_2 ###
#-------------------------------------------------------------------------------
### POL ###
pol2_all <- CallSites_MW(LCC)
pol2_int = make_interval_pol(pol2_all, 100, 10000)
#coniferous_woodland /1.0
mcp_area <- data.frame(age=pol2_int$lower_ends, LCC=pol2_int$coniferous_woodland_sum)
mcp_LCC(mcp_area)
#decidious_woodland /1.1
mcp_area <- data.frame(age=pol2_int$lower_ends, LCC=pol2_int$deciduous_woodland_sum)
mcp_LCC(mcp_area)
#wet_woodland /1.0
mcp_area <- data.frame(age=pol2_int$lower_ends, LCC=pol2_int$wet_woodland_sum)
mcp_LCC(mcp_area)
#wet_meadow /1.4
mcp_area <- data.frame(age=pol2_int$lower_ends, LCC=pol2_int$wet_meadow_sum)
mcp_LCC(mcp_area)
#pasture/meadow
mcp_area <- data.frame(age=pol2_int$lower_ends, LCC=pol2_int$pasture_sum)
mcp_LCC(mcp_area)
#arable
mcp_area <- data.frame(age=pol2_int$lower_ends, LCC=pol2_int$arable_sum)
mcp_LCC(mcp_area)
#heath
mcp_area <- data.frame(age=pol2_int$lower_ends, LCC=pol2_int$heath_sum)
mcp_LCC(mcp_area)

ecp_pol2 <- ecp_inc(pol2_int, "pol2_int")
plot_LCC(pol2_int, ecp_pol2, "LCC_MidWest")


### SouthMid_36 ###
#-------------------------------------------------------------------------------
### POL ###
pol36_all <- CallSites_SM(LCC)
pol36_int = make_interval_pol(pol36_all, 100, 10000)
#coniferous_woodland
mcp_area <- data.frame(age=pol36_int$lower_ends, LCC=pol36_int$coniferous_woodland_sum)
mcp_LCC(mcp_area)
#decidious_woodland
mcp_area <- data.frame(age=pol36_int$lower_ends, LCC=pol36_int$deciduous_woodland_sum)
mcp_LCC(mcp_area)
#wet_woodland
mcp_area <- data.frame(age=pol36_int$lower_ends, LCC=pol36_int$wet_woodland_sum)
mcp_LCC(mcp_area)
#wet_meadow
mcp_area <- data.frame(age=pol36_int$lower_ends, LCC=pol36_int$wet_meadow_sum)
mcp_LCC(mcp_area)
#pasture/meadow
mcp_area <- data.frame(age=pol36_int$lower_ends, LCC=pol36_int$pasture_sum)
mcp_LCC(mcp_area)
#arable
mcp_area <- data.frame(age=pol36_int$lower_ends, LCC=pol36_int$arable_sum)
mcp_LCC(mcp_area)
#heath
mcp_area <- data.frame(age=pol36_int$lower_ends, LCC=pol36_int$heath_sum)
mcp_LCC(mcp_area)

ecp_pol36 <- ecp_inc(pol36_int, "pol36_int")
plot_LCC(pol36_int, ecp_pol36, "LCC_SouthMid")


### MidMid_47 ###
#-------------------------------------------------------------------------------
### POL ###
pol47_all <- CallSites_MM(LCC)
pol47_int = make_interval_pol(pol47_all, 100, 10000)
#coniferous_woodland
mcp_area <- data.frame(age=pol47_int$lower_ends, LCC=pol47_int$coniferous_woodland_sum)
mcp_LCC(mcp_area)
#decidious_woodland
mcp_area <- data.frame(age=pol47_int$lower_ends, LCC=pol47_int$deciduous_woodland_sum)
mcp_LCC(mcp_area)
#wet_woodland
mcp_area <- data.frame(age=pol47_int$lower_ends, LCC=pol47_int$wet_woodland_sum)
mcp_LCC(mcp_area)
#wet_meadow
mcp_area <- data.frame(age=pol47_int$lower_ends, LCC=pol47_int$wet_meadow_sum)
mcp_LCC(mcp_area)
#pasture/meadow
mcp_area <- data.frame(age=pol47_int$lower_ends, LCC=pol47_int$pasture_sum)
mcp_LCC(mcp_area)
#arable
mcp_area <- data.frame(age=pol47_int$lower_ends, LCC=pol47_int$arable_sum)
mcp_LCC(mcp_area)
#heath
mcp_area <- data.frame(age=pol47_int$lower_ends, LCC=pol47_int$heath_sum)
mcp_LCC(mcp_area)

ecp_pol47 <- ecp_inc(pol47_int, "pol47_int")
plot_LCC(pol47_int, ecp_pol47, "LCC_MidMid")


### SouthEast_911 ###
#-------------------------------------------------------------------------------
### POL ###
pol911_all <- CallSites_SE(LCC)
pol911_int = make_interval_pol(pol911_all, 100, 10000)
#coniferous_woodland
mcp_area <- data.frame(age=pol911_int$lower_ends, LCC=pol911_int$coniferous_woodland_sum)
mcp_LCC(mcp_area)
#decidious_woodland
mcp_area <- data.frame(age=pol911_int$lower_ends, LCC=pol911_int$deciduous_woodland_sum)
mcp_LCC(mcp_area)
#wet_woodland
mcp_area <- data.frame(age=pol911_int$lower_ends, LCC=pol911_int$wet_woodland_sum)
mcp_LCC(mcp_area)
#wet_meadow
mcp_area <- data.frame(age=pol911_int$lower_ends, LCC=pol911_int$wet_meadow_sum)
mcp_LCC(mcp_area)
#pasture/meadow
mcp_area <- data.frame(age=pol911_int$lower_ends, LCC=pol911_int$pasture_sum)
mcp_LCC(mcp_area)
#arable
mcp_area <- data.frame(age=pol911_int$lower_ends, LCC=pol911_int$arable_sum)
mcp_LCC(mcp_area)
#heath
mcp_area <- data.frame(age=pol911_int$lower_ends, LCC=pol911_int$heath_sum)
mcp_LCC(mcp_area)

ecp_pol911 <- ecp_inc(pol911_int, "pol911_int")
plot_LCC(pol911_int, ecp_pol911, "LCC_SouthEast")
