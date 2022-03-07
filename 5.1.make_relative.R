### relative abundance ###

#install.packages("ade4")
#install.packages("funrar")
library(ade4)
library(funrar)


#function for df without time.intervals
#1. load and name the dataframe
relative_abundance_normal = function(dataframe){
  mat = subset(dataframe, select = -c(dataset_ID, meantimes))
  mat = data.matrix(mat)
  mat_rel = make_relative(mat)
  df_rel_normal = as.data.frame(mat_rel)
  df_rel_normal$meantimes = dataframe$meantimes
  df_rel_normal$dataset_ID = dataframe$dataset_ID
  df_rel_normal = df_rel_normal %>% select(dataset_ID, meantimes, everything())
}

#function for df with time.intervals
#1. load and name the dataframe
relative_abundance_intervals = function(dataframe){
  mat = subset(dataframe, select = -c(time.intervals, meantimes))
  mat = data.matrix(mat)
  mat_rel = make_relative(mat)
  df_rel_intervals = data.frame(mat_rel)
  df_rel_intervals$meantimes = dataframe$meantimes
  df_rel_intervals = df_rel_intervals %>% select(meantimes, everything())
}

#FENNOSCANDIA
#no intervals
load("bigdf_familynames.Rda")
bigdf_familynames <- subset(bigdf_familynames, select = -c(333))
Fennoscandia_rel = relative_abundance_normal(bigdf_familynames)
write.csv(Fennoscandia_rel, file = "Fennoscandia_rel.csv", row.names = FALSE)
Fennoscandia_rel <- read.csv("Fennoscandia_rel.csv")

#100yr steps intervals
FENNOSCANDIA_interval <- read.csv("FENNOSCANDIA_genera_every100.csv")
Fennoscandia100_rel = relative_abundance_intervals(FENNOSCANDIA_interval)
write.csv(Fennoscandia100_rel, file = "Fennoscandia100_rel.csv", row.names = FALSE)
Fennoscandia100_rel <- read.csv("Fennoscandia100_rel.csv")

#500yr steps intervals
FENNOSCANDIA_intervals500 <- read.csv("FENNOSCANDIA_intervals500.csv")
Fennoscandia500_rel = relative_abundance_intervals(FENNOSCANDIA_intervals500)
write.csv(Fennoscandia500_rel, file = "Fennoscandia500_rel.csv", row.names = FALSE)
Fennoscandia500_rel <- read.csv("Fennoscandia500_rel.csv")


#ECO.GROUPS
#no intervals
eco_groups <- read.csv("eco_groups_header.csv")
eco_groups_rel = relative_abundance_normal(eco_groups)
write.csv(eco_groups_rel, file = "eco_groups_rel.csv", row.names = FALSE)
eco_groups_rel <- read.csv("eco_groups_rel.csv")

#100intervals
eco_groups_intervals100 <- read.csv("eco_groups_intervals100.csv")
eco_groups100_rel = relative_abundance_intervals(eco_groups_intervals100)
write.csv(eco_groups100_rel, file = "eco_groups100_rel.csv", row.names = FALSE)
eco_groups100_rel <- read.csv("eco_groups100_rel.csv")

#500intervals
eco_groups_interval500 <- read.csv("eco_groups_interval500.csv")
eco_groups500_rel = relative_abundance_intervals(eco_groups_interval500)
write.csv(eco_groups500_rel, file = "eco_groups500_rel.csv", row.names = FALSE)
eco_groups500_rel <- read.csv("eco_groups500_rel.csv")

#NORTH
#no intervals
NORTH <- read.csv("NORTH.csv")
North_rel = relative_abundance_normal(NORTH)
write.csv(North_rel, file = "North_rel.csv", row.names = FALSE)
North_rel <- read.csv("North_rel.csv")

#100intervals
NORTH_intervals100 <- read.csv("NORTH_intervals100.csv")
North100_rel = relative_abundance_intervals(NORTH_intervals100)
write.csv(North100_rel, file = "North100_rel.csv", row.names = FALSE)
North100_rel <- read.csv("North100_rel.csv")

#500intervals
NORTH_intervals500 <- read.csv("NORTH_intervals500.csv")
North500_rel = relative_abundance_intervals(NORTH_intervals500)
write.csv(North500_rel, file = "North500_rel.csv", row.names = FALSE)
North500_rel <- read.csv("North500_rel.csv")

#groups.no intervals
NORTH_groups <- read.csv("NORTH_groups.csv")
North.groups_rel = relative_abundance_normal(NORTH_groups)
write.csv(North.groups_rel, file = "North.groups_rel.csv", row.names = FALSE)
North.groups_rel <- read.csv("North.groups_rel.csv")

#groups.100intervals
NORTHgroups_interval100 <- read.csv("NORTHgroups_interval100.csv")
North.groups100_rel = relative_abundance_intervals(NORTHgroups_interval100)
write.csv(North.groups100_rel, file = "North.groups100_rel.csv", row.names = FALSE)
North.groups100_rel <- read.csv("North.groups100_rel.csv")

#groups.500intervals
NORTHgroups_interval500 <- read.csv("NORTHgroups_interval500.csv")
North.groups500_rel = relative_abundance_intervals(NORTHgroups_interval500)
write.csv(North.groups500_rel, file = "North.groups500_rel.csv", row.names = FALSE)
North.groups500_rel <- read.csv("North.groups500_rel.csv")


#SOUTH
#no intervals
SOUTH <- read.csv("SOUTH.csv")
South_rel = relative_abundance_normal(SOUTH)
write.csv(South_rel, file = "South_rel.csv", row.names = FALSE)
South_rel <- read.csv("South_rel.csv")

#100intervals
SOUTH_intervals100 <- read.csv("SOUTH_intervals100.csv")
South100_rel = relative_abundance_intervals(SOUTH_intervals100)
write.csv(South100_rel, file = "South100_rel.csv", row.names = FALSE)
South100_rel <- read.csv("South100_rel.csv")

#500intervals
SOUTH_intervals500 <- read.csv("SOUTH_intervals500.csv")
South500_rel = relative_abundance_intervals(SOUTH_intervals500)
write.csv(South500_rel, file = "South500_rel.csv", row.names = FALSE)
South500_rel <- read.csv("South500_rel.csv")

#groups.no intervals
SOUTH_groups <- read.csv("SOUTH_groups.csv")
South.groups_rel = relative_abundance_normal(SOUTH_groups)
write.csv(South.groups_rel, file = "South.groups_rel.csv", row.names = FALSE)
South.groups_rel <- read.csv("South.groups_rel.csv")

#groups.100intervals
SOUTHgroups_interval100 <- read.csv("SOUTHgroups_interval100.csv")
South.groups100_rel = relative_abundance_intervals(SOUTHgroups_interval100)
write.csv(South.groups100_rel, file = "South.groups100_rel.csv", row.names = FALSE)
South.groups100_rel <- read.csv("South.groups100_rel.csv")

#groups.500intervals
SOUTHgroups_interval500 <- read.csv("SOUTHgroups_interval500.csv")
South.groups500_rel = relative_abundance_intervals(SOUTHgroups_interval500)
write.csv(South.groups500_rel, file = "South.groups500_rel.csv", row.names = FALSE)
South.groups500_rel <- read.csv("South.groups500_rel.csv")


#AQVP
#no intervals
AQVP <- read.csv("AQVP.csv")
AQVP_rel = relative_abundance_normal(AQVP)
write.csv(AQVP_rel, file = "AQVP_rel.csv", row.names = FALSE)
AQVP_rel <- read.csv("AQVP_rel.csv")

#100intervals
AQVP_intervals <- read.csv("AQVP_intervals.csv")
AQVP100_rel = relative_abundance_intervals(AQVP_intervals)
write.csv(AQVP100_rel, file = "AQVP100_rel.csv", row.names = FALSE)
AQVP100_rel <- read.csv("AQVP100_rel.csv")

#500intervals
AQVP_intervals500 <- read.csv("AQVP_intervals500.csv")
AQVP500_rel = relative_abundance_intervals(AQVP_intervals500)
write.csv(AQVP500_rel, file = "AQVP500_rel.csv", row.names = FALSE)
AQVP500_rel <- read.csv("AQVP500_rel.csv")


#TRSH
#no intervals
TRSH <- read.csv("TRSH.csv")
TRSH_rel = relative_abundance_normal(TRSH)
write.csv(TRSH_rel, file = "TRSH_rel.csv", row.names = FALSE)
TRSH_rel <- read.csv("TRSH_rel.csv")

#100intervals
TRSH_intervals <- read.csv("TRSH_intervals.csv")
TRSH100_rel = relative_abundance_intervals(TRSH_intervals)
write.csv(TRSH100_rel, file = "TRSH100_rel.csv", row.names = FALSE)
TRSH100_rel <- read.csv("TRSH100_rel.csv")

#500intervals
TRSH_intervals500 <- read.csv("TRSH_intervals500.csv")
TRSH500_rel = relative_abundance_intervals(TRSH_intervals500)
write.csv(TRSH500_rel, file = "TRSH500_rel.csv", row.names = FALSE)
TRSH500_rel <- read.csv("TRSH500_rel.csv")

#UPHE
#no intervals
UPHE <- read.csv("UPHE.csv")
UPHE_rel = relative_abundance_normal(UPHE)
write.csv(UPHE_rel, file = "UPHE_rel.csv", row.names = FALSE)
UPHE_rel <- read.csv("UPHE_rel.csv")

#100intervals
UPHE_intervals <- read.csv("UPHE_intervals.csv")
UPHE100_rel = relative_abundance_intervals(UPHE_intervals)
write.csv(UPHE100_rel, file = "UPHE100_rel.csv", row.names = FALSE)
UPHE100_rel <- read.csv("UPHE100_rel.csv")

#500intervals
UPHE_intervals500 <- read.csv("UPHE_intervals500.csv")
UPHE500_rel = relative_abundance_intervals(UPHE_intervals500)
write.csv(UPHE500_rel, file = "UPHE500_rel.csv", row.names = FALSE)
UPHE500_rel <- read.csv("UPHE500_rel.csv")


#VACR
#no intervals
VACR <- read.csv("VACR.csv")
VACR_rel = relative_abundance_normal(VACR)
write.csv(VACR_rel, file = "VACR_rel.csv", row.names = FALSE)
VACR_rel <- read.csv("VACR_rel.csv")

#100intervals
VACR_intervals <- read.csv("VACR_intervals.csv")
VACR100_rel = relative_abundance_intervals(VACR_intervals)
write.csv(VACR100_rel, file = "VACR100_rel.csv", row.names = FALSE)
VACR100_rel <- read.csv("VACR100_rel.csv")

#500intervals
VACR_intervals500 <- read.csv("VACR_intervals500.csv")
VACR500_rel = relative_abundance_intervals(VACR_intervals500)
write.csv(VACR500_rel, file = "VACR500_rel.csv", row.names = FALSE)
VACR500_rel <- read.csv("VACR500_rel.csv")

#CLTV
#no intervals
CLTV <- read.csv("CLTV.csv")
CLTV_rel = relative_abundance_normal(CLTV)
write.csv(CLTV_rel, file = "CLTV_rel.csv", row.names = FALSE)
CLTV_rel <- read.csv("CLTV_rel.csv")

#100intervals
CLTV_intervals <- read.csv("CLTV_intervals.csv")
CLTV100_rel = relative_abundance_intervals(CLTV_intervals)
write.csv(CLTV100_rel, file = "CLTV100_rel.csv", row.names = FALSE)
CLTV100_rel <- read.csv("CLTV100_rel.csv")

#500intervals
CLTV_intervals500 <- read.csv("CLTV_intervals500.csv")
CLTV500_rel = relative_abundance_intervals(CLTV_intervals500)
write.csv(CLTV500_rel, file = "CLTV500_rel.csv", row.names = FALSE)
CLTV500_rel <- read.csv("CLTV500_rel.csv")
