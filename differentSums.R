### why different sums? ###

#1.METHOD
#-------------------------------------------------------------------------------
#That's how I first calculated the sum for the taxa that belong to the LCC "coniferous_woodland".
#sum #I had to change it into a data.matrix before I could use the function rowSums.
coniferous_woodland_sum = subset(coniferous_woodland, select = -c(dataset_ID, meantimes))
coniferous_woodland_sum = data.matrix(coniferous_woodland_sum)
coniferous_woodland_sum = rowSums(coniferous_woodland_sum, na.rm = TRUE)
coniferous_woodland_sum = as.data.frame(coniferous_woodland_sum)
coniferous_woodland_sum$meantimes = coniferous_woodland$meantimes
coniferous_woodland_sum$dataset_ID = coniferous_woodland$dataset_ID

#mcp
pol15_all <- CallSites_N(coniferous_woodland_sum)
pol15_int <- make_interval_pol(pol15_all, 100, 10000)
mcp_area <- data.frame(age=pol15_int$lower_ends, LCC=pol15_int$coniferous_woodland_sum)
mcp_LCC(mcp_area) #bad convergence
#ecp
pol15_sqrt <- sqrt_pol(pol15_int)
pol15_inc <- makeIncrements(pol15_sqrt)
ecp_pol15 <- ecp_inc(pol15_inc, "pol15_inc") #no change-points


#2.METHOD
#-------------------------------------------------------------------------------
#That's how I calculated the sums today, and I got different values.
###N-coniferous_woodland
coniferousN <- CallSites_N(coniferous_woodland)
coniferousN_int <- make_interval_pol(coniferousN, 100, 10000)
#mcp
PinusPiceaN <- rowSums(coniferousN_int[,c(12:15)]) #only Pinus and Picea
conN <- rowSums(coniferousN_int[3:18], na.rm = TRUE) #all coniferous taxa
mcp_area <- data.frame(age=coniferousN_int$lower_ends, LCC=conN) #LCC = PinusPiceaN or conN
mcp_LCC(mcp_area) #better convergence
#ecp
ecp_area <- data.frame(age=coniferousN_int$lower_ends, LCC=conN)
ecp_sqrt <- sqrt_sums(ecp_area)
ecp_inc <- makeIncrements_singleLCC(ecp_sqrt) 
ecp_area <- data.frame(age=coniferousN_int$lower_ends[2:102], LCC=ecp_inc)
conN_ecp <- ecp_sums(ecp_area, "ecp_area") #three change-points