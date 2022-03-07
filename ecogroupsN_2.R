### ecological group dataframe with count data ###

library("readxl")
library(dplyr)
library(data.table)
library(tidyverse)

# Load the dataframe "bigdf_familynames".
load("bigdf_familynames.Rda")


# Load the excel-file "eco.groups.xls".
#ecogroupsN_1 <- read_excel("eco.groups.xls")

# Adapt the dataframe.
#ecogroupsN_1 <- as.data.frame(t(ecogroupsN_1))
#names(ecogroupsN_1) <- ecogroupsN_1[1,]
#ecogroupsN_1 <- ecogroupsN_1[-1,]
#names(ecogroupsN_1) <- gsub(" ", ".", names(ecogroupsN_1))
#ecogroupsN_1 <- ecogroupsN_1[,order(colnames(ecogroupsN_1))]
#bigdf_familynames <- bigdf_familynames[,order(colnames(bigdf_familynames))]
#bigdf_names_eco.groups <- rbind(bigdf_familynames, ecogroupsN_1)
# Save the file
#save(bigdf_names_eco.groups,file="bigdf_names_eco.groups.Rda")
load(file = "bigdf_names_eco.groups.Rda")


#t
taxa_ecogroups.t <- t(bigdf_names_eco.groups)
taxa_ecogroups.t <- as.data.frame(taxa_ecogroups.t)

#not included are: UPBR, UNID, ALGAE, FUNG, DINO, AQBR

#TRSH (Trees and shrubs)
TRSH = filter(taxa_ecogroups.t, taxa_ecogroups.t$ecological.group == "TRSH")
TRSH = t(TRSH)
TRSH = TRSH[-c(4157), ]
TRSH = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, TRSH)

TRSH_sums = subset(TRSH, select = -c(dataset_ID, meantimes))
TRSH_sums = data.matrix(TRSH_sums)
TRSH_sums = rowSums(TRSH_sums, na.rm = TRUE)
TRSH_sums = as.data.frame(TRSH_sums)

#AQVP (Aquatic vascular plants)
AQVP <- filter(taxa_ecogroups.t, taxa_ecogroups.t$ecological.group == "AQVP")
AQVP = t(AQVP)
AQVP = AQVP[-c(4157), ]
AQVP = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, AQVP)
write.csv(AQVP, file = "AQVP.csv", row.names = FALSE)
AQVP <- read.csv("AQVP.csv")

AQVP_sums = subset(AQVP, select = -c(dataset_ID, meantimes))
AQVP_sums = data.matrix(AQVP_sums)
AQVP_sums = rowSums(AQVP_sums, na.rm = TRUE)
AQVP_sums = as.data.frame(AQVP_sums)

#UPHE (upland herbs)
UPHE <- filter(taxa_ecogroups.t, taxa_ecogroups.t$ecological.group == "UPHE")
UPHE = t(UPHE)
UPHE = UPHE[-c(4157), ]
UPHE = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, UPHE)

UPHE_sums = subset(UPHE, select = -c(dataset_ID, meantimes))
UPHE_sums = data.matrix(UPHE_sums)
UPHE_sums = rowSums(UPHE_sums, na.rm = TRUE)
UPHE_sums = as.data.frame(UPHE_sums)

#VACR (Terrestrial Vascular Cryptogams)
VACR <- filter(taxa_ecogroups.t, taxa_ecogroups.t$ecological.group == "VACR")
VACR = t(VACR)
VACR = VACR[-c(4157), ]
VACR = data.frame(dataset_ID=bigdf_familynames$dataset_ID, meantimes=bigdf_familynames$meantimes, VACR)
write.csv(VACR, file = "VACR.csv", row.names = FALSE)
VACR <- read.csv("VACR.csv")

VACR_sums = subset(VACR, select = -c(dataset_ID, meantimes))
VACR_sums = data.matrix(VACR_sums)
VACR_sums = rowSums(VACR_sums, na.rm = TRUE)
VACR_sums = as.data.frame(VACR_sums)

ecogroupsN_2 <- data.frame(dataset_ID = bigdf_familynames$dataset_ID, meantimes = bigdf_familynames$meantimes, TRSH_sums, UPHE_sums, AQVP_sums, VACR_sums)
write.csv(ecogroupsN_2, file = "ecogroupsN_2.csv", row.names = FALSE)
ecogroupsN_2 <- read.csv("ecogroupsN_2.csv")


### with CLTV ###

#CLTV (Cultivated plants)
CLTV <- select(bigdf_familynames, c("dataset_ID", "meantimes", "Avena.Triticum", "Triticum", "Cerealia", "Hordeum", "Secale", "Secale.cereale",
                                    "Humulus", "Humulus.Cannabis", "Humulus.lupulus", "Cannabis", "Cannabis.sativa", "Cannabaceae",
                                    "Rubiaceae", "Brassica", "Brassicaceae", "Fabaceae", "Apiaceae"))
write.csv(CLTV, file = "CLTV.csv", row.names = FALSE)
CLTV <- read.csv("CLTV.csv")

CLTV_sums = subset(CLTV, select = -c(dataset_ID, meantimes))
CLTV_sums = data.matrix(CLTV_sums)
CLTV_sums = rowSums(CLTV_sums, na.rm = TRUE)
CLTV_sums = data.frame(CLTV_sums)

#UPHE
UPHE = subset(UPHE, select = -c(Avena.Triticum, Triticum, Cerealia, Hordeum, Secale, Secale.cereale, Humulus, Humulus.Cannabis, Humulus.lupulus,
                                Cannabis, Cannabaceae, Cannabis.sativa, Brassica, Brassicaceae, Fabaceae, Apiaceae))
write.csv(UPHE, file = "UPHE.csv", row.names = FALSE)
UPHE <- read.csv("UPHE.csv")

UPHE_sums = subset(UPHE, select = -c(dataset_ID, meantimes))
UPHE_sums = data.matrix(UPHE_sums)
UPHE_sums = rowSums(UPHE_sums, na.rm = TRUE)
UPHE_sums = as.data.frame(UPHE_sums)

#TRSH
TRSH = subset(TRSH, select = -c(Rubiaceae))
write.csv(TRSH, file = "TRSH.csv", row.names = FALSE)
TRSH <- read.csv("TRSH.csv")

TRSH_sums = subset(TRSH, select = -c(dataset_ID, meantimes))
TRSH_sums = data.matrix(TRSH_sums)
TRSH_sums = rowSums(TRSH_sums, na.rm = TRUE)
TRSH_sums = as.data.frame(TRSH_sums)


ecogroupsN_2_CLTV <- data.frame(dataset_ID = bigdf_familynames$dataset_ID, meantimes = bigdf_familynames$meantimes, CLTV_sums, TRSH_sums, UPHE_sums, AQVP_sums, VACR_sums)
write.csv(ecogroupsN_2_CLTV, file = "ecogroupsN_2_CLTV.csv", row.names = FALSE)
ecogroupsN_2_CLTV <- read.csv("ecogroupsN_2_CLTV.csv")
