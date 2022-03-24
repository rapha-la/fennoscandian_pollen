### make dataframes of LCC's according to long/lat-grids ###


### North_15 ###
#-------------------------------------------------------------------------------
### ARC ###
arc15_int = make_interval_arc(arc15, 100, 10000)
arc15_int <- arc15_int[-c(105),]
# plot
plot_arc(arc15_int)


### POL ###
pol15_all <- CallSites_N(LCC)

#pol15_int
pol15_all_int <- make_interval_pol(pol15_all, 100, 10000)
pol15_int <- pol15_all_int[-c(129),]

#relative_abundance
#pol15_int_rel = relative_pol_int(pol15_int)

#sqrt
sqrt_pol15_int <- sqrt(pol15_int[3:9])
sqrt_pol15_int$meantimes <- pol15_int$meantimes
sqrt_pol15_int <- sqrt_pol15_int %>% select(meantimes, everything())

#increments
pol15_inc = makeIncrements(sqrt_pol15_int)
pol15_inc$meantimes = sqrt_pol15_int$meantimes[2:nrow(sqrt_pol15_int)]

#ecp
ecp_pol15 <- ecp_inc(pol15_inc, "pol15_inc")

#plot
plot_LCC(pol15_int, ecp_pol15, "LCC_North")


### SouthWest_1 ###
#-------------------------------------------------------------------------------
### ARC ###
arc1_int = make_interval_arc(arc1, 100, 10000)
# plot
plot_arc(arc1_int)


### POL ###
pol1_all <- CallSites_SW(LCC)

#pol1_int
pol1_all_interval = make_interval_pol(pol1_all, 100, 10000)
pol1_int = pol1_all_interval[-c(129),]

#relative_abundance
#pol1_int_rel = relative_pol_int(pol1_int)

#sqrt
sqrt_pol1_int <- sqrt(pol1_int[3:9])
sqrt_pol1_int$meantimes <- pol1_int$meantimes
sqrt_pol1_int <- sqrt_pol1_int %>% select(meantimes, everything())

#increments
pol1_inc = makeIncrements(sqrt_pol1_int)
pol1_inc$meantimes = sqrt_pol1_int$meantimes[2:nrow(sqrt_pol1_int)]

#ecp
ecp_pol1 <- ecp_inc(pol1_inc, "pol1_inc")

#plot
plot_LCC(pol1_int, ecp_pol1, "LCC_SouthWest")


### MidWest_2 ###
#-------------------------------------------------------------------------------
### ARC ###
arc2_int = make_interval_arc(arc2, 100, 10000)
# plot
plot_arc(arc2_int)


### POL ###
pol2_all <- CallSites_MW(LCC)

#pol2_int
pol2_all_interval = make_interval_pol(pol2_all, 100, 10000)
pol2_int = pol2_all_interval[-c(129),]

#relative_abundance
#pol2_int_rel = relative_pol_int(pol2_int)

#sqrt
sqrt_pol2_int <- sqrt(pol2_int[3:9])
sqrt_pol2_int$meantimes <- pol2_int$meantimes
sqrt_pol2_int <- sqrt_pol2_int %>% select(meantimes, everything())

#increments
pol2_inc = makeIncrements(sqrt_pol2_int)
pol2_inc$meantimes = sqrt_pol2_int$meantimes[2:nrow(sqrt_pol2_int)]

#ecp
ecp_pol2 <- ecp_inc(pol2_inc, "pol2_inc")

#plot
plot_LCC(pol2_int, ecp_pol2, "LCC_MidWest")


### SouthMid_36 ###
#-------------------------------------------------------------------------------
### ARC ###
arc36_int = make_interval_arc(arc36, 100, 10000)
# plot
plot_arc(arc36_int)


### POL ###
pol36_all <- CallSites_SM(LCC)

#pol36_int
pol36_all_interval = make_interval_pol(pol36_all, 100, 10000)
pol36_int = pol36_all_interval[-c(112),]

#relative_abundance
#pol36_int_rel = relative_pol_int(pol36_int)

#sqrt
sqrt_pol36_int <- sqrt(pol36_int[3:9])
sqrt_pol36_int$meantimes <- pol36_int$meantimes
sqrt_pol36_int <- sqrt_pol36_int %>% select(meantimes, everything())

#increments
pol36_inc = makeIncrements(sqrt_pol36_int)
pol36_inc$meantimes = sqrt_pol36_int$meantimes[2:nrow(sqrt_pol36_int)]

#ecp
ecp_pol36 <- ecp_inc(pol36_inc, "pol36_inc")

#plot
plot_LCC(pol36_int, ecp_pol36, "LCC_SouthMid")


### MidMid_47 ###
#-------------------------------------------------------------------------------
### ARC ###
arc47_int = make_interval_arc(arc47, 100, 10000)
# plot
plot_arc(arc47_int)


### POL ###
pol47_all <- CallSites_MM(LCC)

#pol47_int
pol47_all_interval = make_interval_pol(pol47_all, 100, 10000)
pol47_int = pol47_all_interval[-c(129),]

#relative_abundance
#pol47_int_rel = relative_pol_int(pol47_int)

#sqrt
sqrt_pol47_int <- sqrt(pol47_int[3:9])
sqrt_pol47_int$meantimes <- pol47_int$meantimes
sqrt_pol47_int <- sqrt_pol47_int %>% select(meantimes, everything())

#increments
pol47_inc = makeIncrements(sqrt_pol47_int)
pol47_inc$meantimes = sqrt_pol47_int$meantimes[2:nrow(sqrt_pol47_int)]

#ecp
ecp_pol47 <- ecp_inc(pol47_inc, "pol47_inc")

#plot
plot_LCC(pol47_int, ecp_pol47, "LCC_MidMid")


### SouthEast_911 ###
#-------------------------------------------------------------------------------
### ARC ###
arc911_int = make_interval_arc(arc911, 100, 10000)
# plot
plot_arc(arc911_int)


### POL ###
poll911_all <- CallSites_SE(LCC)

#pol911_int
pol911_all_interval = make_interval_pol(pol911_all, 100, 10000)
pol911_int = pol911_all_interval

#relative_abundance
#pol911_int_rel = relative_pol_int(pol911_int)

#sqrt
sqrt_pol911_int <- sqrt(pol911_int[3:9])
sqrt_pol911_int$meantimes <- pol911_int$meantimes
sqrt_pol911_int <- sqrt_pol911_int %>% select(meantimes, everything())

#increments
pol911_inc = makeIncrements(sqrt_pol911_int)
pol911_inc$meantimes = sqrt_pol911_int$meantimes[2:nrow(sqrt_pol911_int)]

#ecp
ecp_pol911 <- ecp_inc(pol911_inc, "pol911_inc")

#plot
plot_LCC(pol911_int, ecp_pol911, "LCC_SouthEast")
