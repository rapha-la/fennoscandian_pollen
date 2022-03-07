### ecp multivariate change point test ###

library(ecp)
library(ggplot2)
library(dplyr)

# ecp-functions
#for site
ecp_divisive_site = function(dataset_ID){
  big=bigdf_genera[bigdf_genera$dataset_ID==dataset_ID,]
  big_sorted = big[order(big$meantimes),]
  big_restricted = big_sorted
  big_restricted_nonas = big_restricted[,colSums(is.na(big_restricted)) < nrow(big_restricted)]
  big_noids = subset(big_restricted_nonas,select=-c(meantimes,dataset_ID))
  big_scaled = scale(big_noids,scale = FALSE)
  ecp_divisive_for_site = e.divisive(big_scaled, k = NULL)
  #point1 <- big$meantimes[ecp_divisive_for_site$estimates[2]]
  #point2 <- big$meantimes[ecp_divisive_for_site$estimates[3]]
  #point3 <- big$meantimes[ecp_divisive_for_site$estimates[4]]
  #list(point1, point2, point3)
}

#for groups without intervals
ecp_divisive_group = function(groupdf){
  big=groupdf
  big_sorted = big[order(big$meantimes),]
  big_restricted = big_sorted
  big_restricted_nonas = big_restricted[,colSums(is.na(big_restricted)) < nrow(big_restricted)]
  big_noids = subset(big_restricted_nonas,select=-c(meantimes,dataset_ID))
  big_scaled = scale(big_noids,scale = FALSE)
  ecp_divisive_for_site = e.divisive(big_scaled, k = NULL)
  #point1 <- big$meantimes[ecp_divisive_for_site$estimates[2]]
  #point2 <- big$meantimes[ecp_divisive_for_site$estimates[3]]
  #point3 <- big$meantimes[ecp_divisive_for_site$estimates[4]]
  #list(point1, point2, point3)
}

#for groups with intervals
ecp_divisive_intervals = function(groupdf, df){
  big=groupdf
  big_sorted = big[order(big$meantimes),]
  big_restricted = big_sorted
  big_restricted_nonas = big_restricted[,colSums(is.na(big_restricted)) < nrow(big_restricted)]
  big_noids = subset(big_restricted_nonas,select=-c(meantimes,time.intervals))
  big_scaled = scale(big_noids,scale = FALSE)
  ecp_divisive_for_site = e.divisive(big_scaled, k = NULL)
  point1 <- big$meantimes[ecp_divisive_for_site$estimates[2]]
  point2 <- big$meantimes[ecp_divisive_for_site$estimates[3]]
  point3 <- big$meantimes[ecp_divisive_for_site$estimates[4]]
  data.frame(df, point1, point2, point3)
}

#for rel
ecp_divisive_rel = function(groupdf, df){
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

#SINGLE SITES
ecp_12 <- ecp_divisive_site(12)


# 100yr intervals / relative abundance #

Fennoscandia100_rel <- read.csv("Fennoscandia100_rel.csv")
ecp_Fennoscandia100_rel = ecp_divisive_rel(Fennoscandia100_rel, "Fennoscandia100_rel")

North100_rel <- read.csv("North100_rel.csv")
ecp_North100_rel <- ecp_divisive_rel(North100_rel, "North100_rel")

South100_rel <- read.csv("South100_rel.csv")
ecp_South100_rel <- ecp_divisive_rel(South100_rel, "South100_rel")

eco_groups100_rel <- read.csv("eco_groups100_rel.csv")
ecp_eco_groups100_rel = ecp_divisive_rel(eco_groups100_rel, "eco_groups100_rel")

noTRSH100_rel <- subset(eco_groups100_rel, select = -c(TRSH))
ecp_noTRSH100_rel = ecp_divisive_rel(noTRSH100_rel, "noTRSH100_rel")

North.groups100_rel <- read.csv("North.groups100_rel.csv")
ecp_North.groups100_rel = ecp_divisive_rel(North.groups100_rel, "North.groups100_rel")

South.groups100_rel <- read.csv("South.groups100_rel.csv")
ecp_South.groups100_rel = ecp_divisive_rel(South.groups100_rel, "South.groups100_rel")

AQVP100_rel <- read.csv("AQVP100_rel.csv")
ecp_AQVP100_rel = ecp_divisive_rel(AQVP100_rel, "AQVP100_rel")

TRSH100_rel <- read.csv("TRSH100_rel.csv")
ecp_TRSH100_rel = ecp_divisive_rel(TRSH100_rel, "TRSH100_rel")

UPHE100_rel <- read.csv("UPHE100_rel.csv")
ecp_UPHE100_rel = ecp_divisive_rel(UPHE100_rel, "UPHE100_rel")

VACR100_rel <- read.csv("VACR100_rel.csv")
ecp_VACR100_rel = ecp_divisive_rel(VACR100_rel, "VACR100_rel")

CLTV100_rel <- read.csv("CLTV100_rel.csv")
ecp_CLTV100_rel = ecp_divisive_rel(CLTV100_rel, "CLTV100_rel")


#results
ecp_results_100_rel <- bind_rows(
  ecp_Fennoscandia100_rel,
  ecp_North100_rel,
  ecp_South100_rel,
  ecp_eco_groups100_rel,
  ecp_noTRSH100_rel,
  ecp_North.groups100_rel,
  ecp_South.groups100_rel,
  ecp_AQVP100_rel,
  ecp_TRSH100_rel,
  ecp_UPHE100_rel,
  ecp_VACR100_rel,
  ecp_CLTV100_rel)

write.csv(ecp_results_100_rel, file = "ecp_results_100_rel.csv", row.names = FALSE)
ecp_results_100_rel <- read.csv("ecp_results_100_rel.csv")


# 100yr intervals / Count data #

FENNOSCANDIA_interval <- read.csv("FENNOSCANDIA_genera_every100.csv")
ecp_FENNOSCANDIA_interval <- ecp_divisive_intervals(FENNOSCANDIA_interval, "FENNOSCANDIA_interval")

NORTH_intervals100 <- read.csv("NORTH_intervals100.csv")
ecp_NORTH_intervals100 <- ecp_divisive_intervals(NORTH_intervals100, "NORTH_intervals100")

SOUTH_intervals100 <- read.csv("SOUTH_intervals100.csv")
ecp_SOUTH_intervals100 <- ecp_divisive_intervals(SOUTH_intervals100, "SOUTH_intervals100")

eco_groups_intervals100 <- read.csv("eco_groups_intervals100.csv")
ecp_eco_groups_intervals100 <- ecp_divisive_intervals(eco_groups_intervals100, "eco_groups_intervals100")

noTRSH100 <- subset(eco_groups_intervals100, select = -c(TRSH))
ecp_noTRSH100 = ecp_divisive_intervals(noTRSH100, "noTRSH100")

NORTHgroups_interval100 <- read.csv("NORTHgroups_interval100.csv")
ecp_NORTHgroups_interval100 <- ecp_divisive_intervals(NORTHgroups_interval100, "NORTHgroups_interval100")

SOUTHgroups_interval100 <- read.csv("SOUTHgroups_interval100.csv")
ecp_SOUTHgroups_interval100 <- ecp_divisive_intervals(SOUTHgroups_interval100, "SOUTHgroups_interval100")

AQVP_intervals <- read.csv("AQVP_intervals.csv")
ecp_AQVP_intervals <- ecp_divisive_intervals(AQVP_intervals, "AQVP_intervals")

TRSH_intervals <- read.csv("TRSH_intervals.csv")
ecp_TRSH_intervals <- ecp_divisive_intervals(TRSH_intervals, "TRSH_intervals")

UPHE_intervals <- read.csv("UPHE_intervals.csv")
ecp_UPHE_intervals <- ecp_divisive_intervals(UPHE_intervals, "UPHE_intervals")

VACR_intervals <- read.csv("VACR_intervals.csv")
ecp_VACR_intervals <- ecp_divisive_intervals(VACR_intervals, "VACR_intervals")

CLTV_intervals <- read.csv("CLTV_intervals.csv")
ecp_CLTV_intervals <- ecp_divisive_intervals(CLTV_intervals, "CLTV_intervals")

#results
ecp_results_100_count <- bind_rows(
  ecp_FENNOSCANDIA_interval,
  ecp_NORTH_intervals100,
  ecp_SOUTH_intervals100,
  ecp_eco_groups_intervals100,
  ecp_noTRSH100,
  ecp_NORTHgroups_interval100,
  ecp_SOUTHgroups_interval100,
  ecp_AQVP_intervals,
  ecp_TRSH_intervals,
  ecp_UPHE_intervals,
  ecp_VACR_intervals,
  ecp_CLTV_intervals)

write.csv(ecp_results_100_count, file = "ecp_results_100_count.csv", row.names = FALSE)
ecp_results_100_count <- read.csv("ecp_results_100_count.csv")

# results together
ecp_results_100_all <- bind_rows(
  ecp_FENNOSCANDIA_interval,
  ecp_Fennoscandia100_rel,
  ecp_NORTH_intervals100,
  ecp_North100_rel,
  ecp_SOUTH_intervals100,
  ecp_South100_rel,
  ecp_eco_groups_intervals100,
  ecp_eco_groups100_rel,
  ecp_noTRSH100,
  ecp_noTRSH100_rel,
  ecp_NORTHgroups_interval100,
  ecp_North.groups100_rel,
  ecp_SOUTHgroups_interval100,
  ecp_South.groups100_rel,
  ecp_AQVP_intervals,
  ecp_AQVP100_rel,
  ecp_TRSH_intervals,
  ecp_TRSH100_rel,
  ecp_UPHE_intervals,
  ecp_UPHE100_rel,
  ecp_VACR_intervals,
  ecp_VACR100_rel,
  ecp_CLTV_intervals,
  ecp_CLTV100_rel)

write.csv(ecp_results_100_all, file = "ecp_results_100_all.csv", row.names = FALSE)
ecp_results_100_all <- read.csv("ecp_results_100_all.csv")










