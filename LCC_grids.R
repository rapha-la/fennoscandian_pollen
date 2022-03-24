### make dataframes of LCC's according to long/lat-grids ###

#install.packages("plyr")
#install.packages("dplyr")
library(plyr)
library(dplyr)
library(tidyverse)
library(ade4)
library(funrar)
library(ecp)
library(ggplot2)

#LOAD
LCC <- read.csv("LCC.csv")
#grid_files
clim15 <- read.csv("clim15.csv")
pol15 <- read.csv("pol15.csv")
arc15 <- read.csv("arc15.csv")
clim1 <- read.csv("clim1.csv")
pol1 <- read.csv("pol1.csv")
arc1 <- read.csv("arc1.csv")
clim2 <- read.csv("clim2.csv")
pol2 <- read.csv("pol2.csv")
arc2 <- read.csv("arc2.csv")
clim36 <- read.csv("clim36.csv")
pol36 <- read.csv("pol36.csv")
arc36 <- read.csv("arc36.csv")
clim47 <- read.csv("clim47.csv")
pol47 <- read.csv("pol47.csv")
arc47 <- read.csv("arc47.csv")
clim911 <- read.csv("clim911.csv")
pol911 <- read.csv("pol911.csv")
arc911 <- read.csv("arc911.csv")


### FUNCTIONS ###
#-------------------------------------------------------------------------------
#plot_arc
plot_arc = function(df){
  ggplot(df, aes(x = mean_age)) + 
    scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
    geom_line(aes(y = nr_finds,colour="col1")) + 
    labs(x = "Year BP",y = "Nr of finds",colour = "Legend")
}

#plot_LCC
plot_LCC = function(LCC_df, ecp_df, titleinquotes){
  ggplot(LCC_df, aes(x = meantimes)) + 
    scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                        labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
    geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
    geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
    geom_line(aes(y = wet_woodland_sum,colour="col3")) +
    geom_line(aes(y = pasture_sum,colour="col4")) + 
    geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
    geom_line(aes(y = arable_sum,colour="col6")) +
    geom_line(aes(y = heath_sum,colour="col7")) + 
    geom_vline(aes(xintercept = ecp_df$point1, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecp_df$point2, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecp_df$point3, colour = "changepoint")) +
    ggtitle(titleinquotes) +
    labs(x = "Years BP",y = "counts",colour = "Legend")
}

#make_interval_arc
make_interval_arc = function(dataset, interval, earliest_date){
  # set up cut-off values (every100)
  breaks <- seq(from = -100, to = earliest_date, by = interval)
  dataset$find <- c(1)
  # bucketing values into bins
  group_tags <- cut(dataset$weighted_mean, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE)
  dataset_interval <- dataset
  dataset_interval$time.intervals <- group_tags
  # group by
  dataset_interval <- dataset_interval %>%
    group_by(time.intervals) %>%
    summarise(
      mean_age = mean(weighted_mean),
      nr_finds = sum(find))
}

#make_interval_pol
make_interval_pol = function(dataset, interval, earliest_date){
  # set up cut-off values (every100)
  breaks <- seq(from = -100, to = earliest_date, by = interval)
  # bucketing values into bins
  group_tags <- cut(dataset$meantimes, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE)
  dataset_interval <- dataset
  dataset_interval$time.intervals <- group_tags
  #Sort
  dataset_interval <- dataset_interval %>% select(dataset_ID, meantimes, time.intervals, everything())
  dataset_interval <- subset(dataset_interval, select = -c(dataset_ID))
  # group by
  dataset_interval <- dataset_interval %>%
    group_by(time.intervals) %>%
    summarise(
      meantimes = mean(meantimes),
      across(2:8,na.rm=TRUE,sum))
}

#relative_pol_int
relative_pol_int = function(dataframe){
  mat = subset(dataframe, select = -c(time.intervals, meantimes))
  mat = data.matrix(mat)
  mat_rel = make_relative(mat)
  df_rel_intervals = data.frame(mat_rel)
  df_rel_intervals$meantimes = dataframe$meantimes
  df_rel_intervals = df_rel_intervals %>% select(meantimes, everything())
}

#make increments
makeIncrements = function(df){
  df[2:(nrow(df)),3:ncol(df)]-df[1:(nrow(df)-1),3:ncol(df)]
}

#ecp_inc
ecp_inc = function(groupdf, df){
  big=groupdf
  big_sorted = big[order(big$meantimes),]
  big_restricted = big_sorted
  big_restricted_nonas = big_restricted[,colSums(is.na(big_restricted)) < nrow(big_restricted)]
  big_noids = subset(big_restricted_nonas,select=-c(meantimes))
  big_scaled = scale(big_noids,scale = FALSE)
  ecp_divisive_for_site = e.divisive(big_scaled, k = NULL)
  point1 <- big$meantimes[ecp_divisive_for_site$estimates[2]]
  point2 <- big$meantimes[ecp_divisive_for_site$estimates[3]]
  point3 <- big$meantimes[ecp_divisive_for_site$estimates[4]]
  data.frame(df, point1, point2, point3)
}


### North_15 ###
#-------------------------------------------------------------------------------
### ARC ###
arc15_int = make_interval_arc(arc15, 100, 10000)
arc15_int <- arc15_int[-c(105),]
# plot
plot_arc(arc15_int)


### POL ###
print(pol15$site)

pol15_1 <-  subset(LCC, dataset_ID == "20")
pol15_2 <-  subset(LCC, dataset_ID == "317")
pol15_3 <-  subset(LCC,dataset_ID == "720")
pol15_4 <-  subset(LCC,dataset_ID == "4257")
pol15_5 <-  subset(LCC,dataset_ID == "4286")
pol15_6 <-  subset(LCC,dataset_ID == "4372")
pol15_7 <-  subset(LCC,dataset_ID == "4468")
pol15_8 <-  subset(LCC,dataset_ID == "20034")
pol15_9 <-  subset(LCC,dataset_ID == "20279")
pol15_10 <-  subset(LCC,dataset_ID == "20285")
pol15_11 <-  subset(LCC,dataset_ID == "20293")
pol15_12 <-  subset(LCC,dataset_ID == "44941")
pol15_13 <-  subset(LCC,dataset_ID == "45311")
pol15_14 <-  subset(LCC,dataset_ID == "45636")
pol15_15 <-  subset(LCC,dataset_ID == "45639")
pol15_16 <-  subset(LCC,dataset_ID == "45642")
pol15_17 <-  subset(LCC,dataset_ID == "24757")
pol15_18 <-  subset(LCC,dataset_ID == "4169")

pol15_all <- rbind(pol15_1,
                   pol15_2,
                   pol15_3,
                   pol15_4,
                   pol15_5,
                   pol15_6,
                   pol15_7,
                   pol15_8,
                   pol15_9,
                   pol15_10,
                   pol15_11,
                   pol15_12,
                   pol15_13,
                   pol15_14,
                   pol15_15,
                   pol15_16,
                   pol15_17,
                   pol15_18)

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
print(pol1$site)

pol1_1 <-  subset(LCC, dataset_ID == "20050")
pol1_2 <-  subset(LCC, dataset_ID == "45704")
pol1_3 <-  subset(LCC,dataset_ID == "45707")
pol1_4 <-  subset(LCC,dataset_ID == "45710")
pol1_5 <-  subset(LCC,dataset_ID == "45713")
pol1_6 <-  subset(LCC,dataset_ID == "45716")
pol1_7 <-  subset(LCC,dataset_ID == "45719")
pol1_8 <-  subset(LCC,dataset_ID == "45722")
pol1_9 <-  subset(LCC,dataset_ID == "45725")
pol1_10 <-  subset(LCC,dataset_ID == "45728")
pol1_11 <-  subset(LCC,dataset_ID == "45731")

pol1_all <- rbind(pol1_1,
                  pol1_2,
                  pol1_3,
                  pol1_4,
                  pol1_5,
                  pol1_6,
                  pol1_7,
                  pol1_8,
                  pol1_9,
                  pol1_10,
                  pol1_11)

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
print(pol2$site)

pol2_1 <-  subset(LCC, dataset_ID == "977")
pol2_2 <-  subset(LCC, dataset_ID == "20042")
pol2_3 <-  subset(LCC,dataset_ID == "20046")
pol2_4 <-  subset(LCC,dataset_ID == "45331")
pol2_5 <-  subset(LCC,dataset_ID == "45345")
pol2_6 <-  subset(LCC,dataset_ID == "45347")
pol2_7 <-  subset(LCC,dataset_ID == "45349")

pol2_all <- rbind(pol2_1,
                  pol2_2,
                  pol2_3,
                  pol2_4,
                  pol2_5,
                  pol2_6,
                  pol2_7)

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
print(pol36$site)

pol36_1 <-  subset(LCC, dataset_ID == "12")
pol36_2 <-  subset(LCC, dataset_ID == "1438")
pol36_3 <-  subset(LCC,dataset_ID == "4403")
pol36_4 <-  subset(LCC,dataset_ID == "45329")

pol36_all <- rbind(pol36_1,
                   pol36_2,
                   pol36_3,
                   pol36_4)

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
print(pol47$site)

pol47_1 <-  subset(LCC, dataset_ID == "19906")
pol47_2 <-  subset(LCC, dataset_ID == "19909")
pol47_3 <-  subset(LCC,dataset_ID == "19913")
pol47_4 <-  subset(LCC,dataset_ID == "20018")
pol47_5 <-  subset(LCC, dataset_ID == "21790")
pol47_6 <-  subset(LCC, dataset_ID == "45351")
pol47_7 <-  subset(LCC,dataset_ID == "45698")
pol47_8 <-  subset(LCC,dataset_ID == "45701")

pol47_all <- rbind(pol47_1,
                   pol47_2,
                   pol47_3,
                   pol47_4,
                   pol47_5,
                   pol47_6,
                   pol47_7,
                   pol47_8)

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
print(pol911$site)

pol911_1 <-  subset(LCC, dataset_ID == "4092")
pol911_2 <-  subset(LCC, dataset_ID == "4133")
pol911_3 <-  subset(LCC,dataset_ID == "4156")
pol911_4 <-  subset(LCC,dataset_ID == "4168")
pol911_5 <-  subset(LCC, dataset_ID == "4259")
pol911_6 <-  subset(LCC, dataset_ID == "4393")
pol911_7 <-  subset(LCC,dataset_ID == "4420")
pol911_8 <-  subset(LCC,dataset_ID == "4472")
pol911_9 <-  subset(LCC, dataset_ID == "4539")
pol911_10 <-  subset(LCC, dataset_ID == "4543")
pol911_11 <-  subset(LCC,dataset_ID == "4017")
pol911_12 <-  subset(LCC,dataset_ID == "3928")

pol911_all <- rbind(pol911_1,
                    pol911_2,
                    pol911_3,
                    pol911_4,
                    pol911_5,
                    pol911_6,
                    pol911_7,
                    pol911_8,
                    pol911_9,
                    pol911_10,
                    pol911_11,
                    pol911_12)

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
