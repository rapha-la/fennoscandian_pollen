### LOG ###

### make log ###
FENNOSCANDIA_interval <- read.csv("FENNOSCANDIA_genera_every100.csv")
Fennoscandia100_log <- subset(FENNOSCANDIA_interval, select = -c(time.intervals, meantimes))
Fennoscandia100_log <- log(Fennoscandia100_log)
Fennoscandia100_log$meantimes <- FENNOSCANDIA_interval$meantimes
Fennoscandia100_log = Fennoscandia100_log %>% select(meantimes, everything())

NORTH_intervals100 <- read.csv("NORTH_intervals100.csv")
North100_log <- subset(NORTH_intervals100, select = -c(time.intervals, meantimes))
North100_log <- log(North100_log)
North100_log$meantimes <- NORTH_intervals100$meantimes
North100_log = North100_log %>% select(meantimes, everything())

SOUTH_intervals100 <- read.csv("SOUTH_intervals100.csv")
South100_log <- subset(SOUTH_intervals100, select = -c(time.intervals, meantimes))
South100_log <- log(South100_log)
South100_log$meantimes <- SOUTH_intervals100$meantimes
South100_log = South100_log %>% select(meantimes, everything())

eco_groups_intervals100 <- read.csv("eco_groups_intervals100.csv")
eco_groups100_log <- subset(eco_groups_intervals100, select = -c(meantimes))
eco_groups100_log <- log(eco_groups100_log)
eco_groups100_log$meantimes <- eco_groups_intervals100$meantimes
eco_groups100_log = eco_groups100_log %>% select(meantimes, everything())

noTRSH100_log <- subset(eco_groups_intervals100, select = -c(meantimes, TRSH))
noTRSH100_log <- log(noTRSH100_log)
noTRSH100_log$meantimes <- eco_groups_intervals100$meantimes
noTRSH100_log = noTRSH100_log %>% select(meantimes, everything())

eco_groups_interval500 <- read.csv("eco_groups_interval500.csv")
eco_groups500_log <- subset(eco_groups_interval500, select = -c(time.intervals, meantimes))
eco_groups500_log <- log(eco_groups500_log)
eco_groups500_log$meantimes <- eco_groups_interval500$meantimes
#eco_groups500_log = eco_groups500_log %>% select(meantimes, everything())

NORTHgroups_interval100 <- read.csv("NORTHgroups_interval100.csv")
Northgroups100_log <- subset(NORTHgroups_interval100, select = -c(time.intervals, meantimes))
Northgroups100_log <- log(Northgroups100_log)
Northgroups100_log$meantimes <- NORTHgroups_interval100$meantimes
Northgroups100_log = Northgroups100_log %>% select(meantimes, everything())

SOUTHgroups_interval100 <- read.csv("SOUTHgroups_interval100.csv")
Southgroups100_log <- subset(SOUTHgroups_interval100, select = -c(time.intervals, meantimes))
Southgroups100_log <- log(Southgroups100_log)
Southgroups100_log$meantimes <- SOUTHgroups_interval100$meantimes
Southgroups100_log = Southgroups100_log %>% select(meantimes, everything())

AQVP_intervals <- read.csv("AQVP_intervals.csv")
AQVP100_log <- subset(AQVP_intervals, select = -c(time.intervals, meantimes))
AQVP100_log <- log(AQVP100_log)
AQVP100_log$meantimes <- AQVP_intervals$meantimes
AQVP100_log = AQVP100_log %>% select(meantimes, everything())

TRSH_intervals <- read.csv("TRSH_intervals.csv")
TRSH100_log <- subset(TRSH_intervals, select = -c(time.intervals, meantimes))
TRSH100_log <- log(TRSH100_log)
TRSH100_log$meantimes <- TRSH_intervals$meantimes
TRSH100_log = TRSH100_log %>% select(meantimes, everything())

UPHE_intervals <- read.csv("UPHE_intervals.csv")
UPHE100_log <- subset(UPHE_intervals, select = -c(time.intervals, meantimes))
UPHE100_log <- log(UPHE100_log)
UPHE100_log$meantimes <- UPHE_intervals$meantimes
UPHE100_log = UPHE100_log %>% select(meantimes, everything())

VACR_intervals <- read.csv("VACR_intervals.csv")
VACR100_log <- subset(VACR_intervals, select = -c(time.intervals, meantimes))
VACR100_log <- log(VACR100_log)
VACR100_log$meantimes <- VACR_intervals$meantimes
VACR100_log = VACR100_log %>% select(meantimes, everything())

CLTV_intervals <- read.csv("CLTV_intervals.csv")
CLTV100_log <- subset(CLTV_intervals, select = -c(time.intervals, meantimes))
CLTV100_log <- log(CLTV100_log)
CLTV100_log$meantimes <- CLTV_intervals$meantimes
CLTV100_log = CLTV100_log %>% select(meantimes, everything())


### ecp log ###
ecp_Fennoscandia100_log <- ecp_divisive_rel(Fennoscandia100_log, "Fennoscandia100_log")
ecp_North100_log <- ecp_divisive_rel(North100_log, "North100_log")
ecp_South100_log <- ecp_divisive_rel(South100_log, "South100_log")
ecp_eco_groups100_log <- ecp_divisive_rel(eco_groups100_log, "eco_groups100_log")
#ecp_eco_groups500_log <- ecp_divisive_rel(eco_groups500_log, "eco_groups500_log")
ecp_noTRSH100_log <- ecp_divisive_rel(noTRSH100_log, "noTRSH100_log")
ecp_Northgroups100_log <- ecp_divisive_rel(Northgroups100_log, "Northgroups100_log")
ecp_Southgroups100_log <- ecp_divisive_rel(Southgroups100_log, "Southgroups100_log")
ecp_AQVP100_log <- ecp_divisive_rel(AQVP100_log, "AQVP100_log")
ecp_TRSH100_log <- ecp_divisive_rel(TRSH100_log, "TRSH100_log")
ecp_UPHE100_log <- ecp_divisive_rel(UPHE100_log, "UPHE100_log")
ecp_VACR100_log <- ecp_divisive_rel(VACR100_log, "VACR100_log")
ecp_CLTV100_log <- ecp_divisive_rel(CLTV100_log, "CLTV100_log")

#ecp-results
ecp_results_100_log <- bind_rows(
  ecp_Fennoscandia100_log,
  ecp_North100_log,
  ecp_South100_log,
  ecp_eco_groups100_log,
  ecp_noTRSH100_log,
  ecp_Northgroups100_log,
  ecp_Southgroups100_log,
  ecp_AQVP100_log,
  ecp_TRSH100_log,
  ecp_UPHE100_log,
  ecp_VACR100_log,
  ecp_CLTV100_log)

write.csv(ecp_results_100_log, file = "ecp_results_100_log.csv", row.names = FALSE)
ecp_results_100_log <- read.csv("ecp_results_100_log.csv")


### plot log ###
plot_ecp_groups_rel(eco_groups100_log, ecp_eco_groups100_log, "eco_groups100_log")
plot_ecp_groups_rel(eco_groups500_log, ecp_eco_groups100_log, "eco_groups500_log")


