### make climate/archaeological/pollen-dataframes of the different areas with intervals ###

### North_15 ###
#-------------------------------------------------------------------------------
### ARC ###
arc15_int = make_interval_arc(arc15, 100, 10000)
plot_arc(arc15_int)

### POL ###
pol15_all <- CallSites_N(LCC)
pol15_int <- make_interval_pol(pol15_all, 100, 10000)
pol15_sqrt <- sqrt_pol(pol15_int)
pol15_inc <- makeIncrements(pol15_sqrt) #is the first year lost or the last?
ecp_pol15 <- ecp_inc(pol15_inc, "pol15_inc")
plot_LCC(pol15_int, ecp_pol15, "LCC_North")

### CLIM ###
ecp_clim15_int = ecp_clim(clim15_int, "clim15_int")
#plot
ggplot(clim15_int, aes(x = higher_ends)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = t1,colour="col1")) + 
  geom_line(aes(y = t2,colour="col1")) + 
  geom_line(aes(y = t4,colour="col1")) + 
  geom_line(aes(y = t5,colour="col1")) + 
  geom_line(aes(y = t7,colour="col1")) + 
  geom_line(aes(y = t8,colour="col1")) + 
  geom_line(aes(y = t9,colour="col1")) + 
  geom_line(aes(y = t10,colour="col1")) + 
  geom_line(aes(y = t11,colour="col1")) + 
  geom_line(aes(y = t12,colour="col1")) + 
  geom_line(aes(y = t13,colour="col1")) + 
  geom_line(aes(y = t14,colour="col1")) + 
  geom_line(aes(y = t15,colour="col1")) + 
  geom_line(aes(y = t17,colour="col1")) + 
  geom_line(aes(y = t18,colour="col1")) + 
  geom_line(aes(y = t19,colour="col1")) + 
  geom_line(aes(y = t20,colour="col1")) +
  geom_line(aes(y = t21,colour="col1")) +
  ylim(0,15) +
  geom_vline(aes(xintercept = ecp_clim15_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim15_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim15_int$point3, colour = "changepoint")) +
  ggtitle("temperature / North") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### SouthWest_1 ###
#-------------------------------------------------------------------------------
### ARC ###
arc1_int = make_interval_arc(arc1, 100, 10000)
plot_arc(arc1_int)

### POL ###
pol1_all <- CallSites_SW(LCC)
pol1_int = make_interval_pol(pol1_all, 100, 10000)
pol1_sqrt <- sqrt_pol(pol1_int)
pol1_inc = makeIncrements(pol1_sqrt)
ecp_pol1 <- ecp_inc(pol1_inc, "pol1_inc")
plot_LCC(pol1_int, ecp_pol1, "LCC_SouthWest")

### CLIM ###
ecp_clim1_int = ecp_clim(clim1_int, "clim1_int")
#plot
ggplot(clim1_int, aes(x = higher_ends)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = t1,colour="col1")) + 
  geom_line(aes(y = t2,colour="col1")) + 
  geom_line(aes(y = t3,colour="col1")) + 
  geom_line(aes(y = t4,colour="col1")) + 
  geom_line(aes(y = t5,colour="col1")) + 
  geom_line(aes(y = t6,colour="col1")) + 
  geom_line(aes(y = t7,colour="col1")) + 
  geom_line(aes(y = t8,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim1_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim1_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim1_int$point3, colour = "changepoint")) +
  ggtitle("temperature / SouthWest") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### MidWest_2 ###
#-------------------------------------------------------------------------------
### ARC ###
arc2_int = make_interval_arc(arc2, 100, 10000)
plot_arc(arc2_int)

### POL ###
pol2_all <- CallSites_MW(LCC)
pol2_int = make_interval_pol(pol2_all, 100, 10000)
pol2_sqrt <- sqrt_pol(pol2_int)
pol2_inc = makeIncrements(pol2_sqrt)
ecp_pol2 <- ecp_inc(pol2_inc, "pol2_inc")
plot_LCC(pol2_int, ecp_pol2, "LCC_MidWest")

### CLIM ###
ecp_clim2_int = ecp_clim(clim2_int, "clim2_int")
#plot
ggplot(clim2_int, aes(x = higher_ends)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = t1,colour="col1")) + 
  geom_line(aes(y = t2,colour="col1")) + 
  geom_line(aes(y = t4,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim2_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point3, colour = "changepoint")) +
  ggtitle("temperature / MidWest") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### SouthMid_36 ###
#-------------------------------------------------------------------------------
### ARC ###
arc36_int = make_interval_arc(arc36, 100, 10000)
plot_arc(arc36_int)

### POL ###
pol36_all <- CallSites_SM(LCC)
pol36_int = make_interval_pol(pol36_all, 100, 10000)
pol36_sqrt <- sqrt_pol(pol36_int)
pol36_inc = makeIncrements(pol36_sqrt)
ecp_pol36 <- ecp_inc(pol36_inc, "pol36_inc")
plot_LCC(pol36_int, ecp_pol36, "LCC_SouthMid")

### CLIM ###
ecp_clim36_int = ecp_clim(clim36_int, "clim36_int")
#plot
ggplot(clim36_int, aes(x = higher_ends)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = t1,colour="col1")) + 
  geom_line(aes(y = t2,colour="col1")) + 
  geom_line(aes(y = t3,colour="col1")) + 
  geom_line(aes(y = t4,colour="col1")) + 
  geom_line(aes(y = t5,colour="col1")) + 
  geom_line(aes(y = t6,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim2_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point3, colour = "changepoint")) +
  ggtitle("temperature / MidWest") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### MidMid_47 ###
#-------------------------------------------------------------------------------
### ARC ###
arc47_int = make_interval_arc(arc47, 100, 10000)
plot_arc(arc47_int)

### POL ###
pol47_all <- CallSites_MM(LCC)
pol47_int = make_interval_pol(pol47_all, 100, 10000)
pol47_sqrt <- sqrt_pol(pol47_int)
pol47_inc = makeIncrements(pol47_sqrt)
ecp_pol47 <- ecp_inc(pol47_inc, "pol47_inc")
plot_LCC(pol47_int, ecp_pol47, "LCC_MidMid")

### CLIM ###
ecp_clim47_int = ecp_clim(clim47_int, "clim47_int")
#plot
ggplot(clim47_int, aes(x = higher_ends)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = t1,colour="col1")) + 
  geom_line(aes(y = t2,colour="col1")) + 
  geom_line(aes(y = t3,colour="col1")) + 
  geom_line(aes(y = t4,colour="col1")) + 
  geom_line(aes(y = t5,colour="col1")) + 
  geom_line(aes(y = t6,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim2_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point3, colour = "changepoint")) +
  ggtitle("temperature / MidWest") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### SouthEast_911 ###
#-------------------------------------------------------------------------------
### ARC ###
arc911_int = make_interval_arc(arc911, 100, 10000)
plot_arc(arc911_int)

### POL ###
pol911_all <- CallSites_SE(LCC)
pol911_int = make_interval_pol(pol911_all, 100, 10000)
pol911_sqrt <- sqrt_pol(pol911_int)
pol911_inc = makeIncrements(pol911_sqrt)
ecp_pol911 <- ecp_inc(pol911_inc, "pol911_inc")
plot_LCC(pol911_int, ecp_pol911, "LCC_SouthEast")

### CLIM ###
ecp_clim911_int = ecp_clim(clim911_int, "clim911_int")
#plot
ggplot(clim911_int, aes(x = higher_ends)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = t1,colour="col1")) + 
  geom_line(aes(y = t2,colour="col1")) + 
  geom_line(aes(y = t3,colour="col1")) + 
  geom_line(aes(y = t4,colour="col1")) + 
  geom_line(aes(y = t5,colour="col1")) + 
  geom_line(aes(y = t6,colour="col1")) + 
  geom_line(aes(y = t7,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim2_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point3, colour = "changepoint")) +
  ggtitle("temperature / MidWest") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")
