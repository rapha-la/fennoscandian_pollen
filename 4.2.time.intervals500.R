### time-intervals for eco.groups ###

library(tidyverse)


# set up cut-off values 
breaks <- c(-60, 0, 100, 200, 300, 400, 600, 800, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000,
            6500, 7000, 7500, 8000, 8500, 9000, 9500, 10000)

# specify interval/bin labels
time.intervals <- c("[-60-0)","[0-100)", "[100-200)", "[200-300)", "[300-400)", "[400-600)","[600-800)", "[800-1000)",
                     "[1000-1500)", "[1500-2000)", "[2000-2500)", "[2500-3000)", "[3000-3500)", "[3500-4000)", "[4000-4500)",
                     "[4500-5000)", "[5000-5500)", "[5500-6000)", "[6000-6500)", "[6500-7000)", "[7000-7500)", "[7500-8000)",
                     "[8000-8500)", "[8500-9000)", "[9000-9500)", "[9500-10000)")



# ECO.GROUPS
eco_groups_header <- read.csv("eco_groups_CLTV.csv")

# bucketing values into bins
group_tags <- cut(eco_groups_header$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=time.intervals)

eco_groups_interval <- eco_groups_header
eco_groups_interval$time.intervals <- group_tags

# Sort
eco_groups_interval <- eco_groups_interval %>% select(dataset_ID, meantimes, time.intervals, everything())
eco_groups_interval <- subset(eco_groups_interval, select = -c(dataset_ID))

# group by
by_eco_groups_interval <- eco_groups_interval %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:6,na.rm=TRUE,sum))

# Save
write.csv(by_eco_groups_interval, file = "eco_groups_interval500.csv", row.names = FALSE)
eco_groups_interval500 <- read.csv("eco_groups_interval500.csv")


#NORTH
NORTH_groups <- read.csv("NORTH_groups.csv")
NORTH_interval <- NORTH_groups

# bucketing values into bins
group_tags <- cut(NORTH_interval$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=time.intervals)


NORTH_interval$time.intervals <- group_tags

# Sort
NORTH_interval <- NORTH_interval %>% select(dataset_ID, meantimes, time.intervals, everything())
NORTH_interval <- subset(NORTH_interval, select = -c(dataset_ID))

# group by
by_NORTH_interval <- NORTH_interval %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:6,na.rm=TRUE,sum))

# Save
write.csv(by_NORTH_interval, file = "NORTHgroups_interval500.csv", row.names = FALSE)
NORTHgroups_interval500 <- read.csv("NORTHgroups_interval500.csv")


#SOUTH
SOUTH_groups <- read.csv("SOUTH_groups.csv")
SOUTH_interval <- SOUTH_groups

# bucketing values into bins
group_tags <- cut(SOUTH_interval$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=time.intervals)

SOUTH_interval$time.intervals <- group_tags

# Sort
SOUTH_interval <- SOUTH_interval %>% select(dataset_ID, meantimes, time.intervals, everything())
SOUTH_interval <- subset(SOUTH_interval, select = -c(dataset_ID))

# group by
by_SOUTH_interval <- SOUTH_interval %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:6,na.rm=TRUE,sum))

# Save
write.csv(by_SOUTH_interval, file = "SOUTHgroups_interval500.csv", row.names = FALSE)
SOUTHgroups_interval500 <- read.csv("SOUTHgroups_interval500.csv")



#FENNOSCANDIA
load("bigdf_familynames.Rda")
bigdf_familynames <- subset(bigdf_familynames, select = -c(333))
FENNOSCANDIA_interval <- bigdf_familynames

# bucketing values into bins
group_tags <- cut(bigdf_familynames$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=time.intervals)

FENNOSCANDIA_interval$time.intervals <- group_tags

# Sort
FENNOSCANDIA_interval <- FENNOSCANDIA_interval %>% select(dataset_ID, meantimes, time.intervals, everything())
FENNOSCANDIA_interval <- subset(FENNOSCANDIA_interval, select = -c(dataset_ID))

# group by
by_FENNOSCANDIA_interval <- FENNOSCANDIA_interval %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:471,na.rm=TRUE,sum))

# Save
write.csv(by_FENNOSCANDIA_interval, file = "FENNOSCANDIA_intervals500.csv", row.names = FALSE)
FENNOSCANDIA_intervals500 <- read.csv("FENNOSCANDIA_intervals500.csv")


#NORTH
NORTH <- read.csv("NORTH.csv")

# bucketing values into bins
group_tags <- cut(NORTH$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

NORTH_interval$time.intervals <- group_tags

# Sort
NORTH_interval <- NORTH_interval %>% select(dataset_ID, meantimes, time.intervals, everything())
NORTH_interval <- subset(NORTH_interval, select = -c(dataset_ID))
NORTH_interval <- NORTH_interval[,colSums(is.na(NORTH_interval)) < nrow(NORTH_interval)]

# group by
by_NORTH_interval <- NORTH_interval %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:263,na.rm=TRUE,sum))

# Save
write.csv(by_NORTH_interval, file = "NORTH_intervals500.csv", row.names = FALSE)
NORTH_intervals500 <- read.csv("NORTH_intervals500.csv")


#SOUTH
SOUTH <- read.csv("SOUTH.csv")

# bucketing values into bins
group_tags <- cut(SOUTH$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

SOUTH_interval$time.intervals <- group_tags

# Sort
SOUTH_interval <- SOUTH_interval %>% select(dataset_ID, meantimes, time.intervals, everything())
SOUTH_interval <- subset(SOUTH_interval, select = -c(dataset_ID))
SOUTH_interval = SOUTH_interval[,colSums(is.na(SOUTH_interval)) < nrow(SOUTH_interval)]

# group by
by_SOUTH_interval <- SOUTH_interval %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:435,na.rm=TRUE,sum))

# Save
write.csv(by_SOUTH_interval, file = "SOUTH_intervals500.csv", row.names = FALSE)
SOUTH_intervals500 <- read.csv("SOUTH_intervals500.csv")


#AQVP
AQVP <- read.csv("AQVP.csv")

# bucketing values into bins
group_tags <- cut(AQVP$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

AQVP$time.intervals <- group_tags

# Sort
AQVP <- AQVP %>% select(dataset_ID, meantimes, time.intervals, everything())
AQVP <- subset(AQVP, select = -c(dataset_ID))

# group by
AQVP <- AQVP %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:60,na.rm=TRUE,sum))

# Save
write.csv(AQVP, file = "AQVP_intervals500.csv", row.names = FALSE)
AQVP_intervals500 <- read.csv("AQVP_intervals500.csv")


#TRSH
TRSH <- read.csv("TRSH.csv")

# bucketing values into bins
group_tags <- cut(TRSH$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

TRSH$time.intervals <- group_tags

# Sort
TRSH <- TRSH %>% select(dataset_ID, meantimes, time.intervals, everything())
TRSH <- subset(TRSH, select = -c(dataset_ID))

# group by
TRSH <- TRSH %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:99,na.rm=TRUE,sum))

# Save
write.csv(TRSH, file = "TRSH_intervals500.csv", row.names = FALSE)
TRSH_intervals500 <- read.csv("TRSH_intervals500.csv")

#UPHE
UPHE <- read.csv("UPHE.csv")

# bucketing values into bins
group_tags <- cut(UPHE$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

UPHE$time.intervals <- group_tags

# Sort
UPHE <- UPHE %>% select(dataset_ID, meantimes, time.intervals, everything())
UPHE <- subset(UPHE, select = -c(dataset_ID))

# group by
UPHE <- UPHE %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:242,na.rm=TRUE,sum))

# Save
write.csv(UPHE, file = "UPHE_intervals500.csv", row.names = FALSE)
UPHE_intervals500 <- read.csv("UPHE_intervals500.csv")


#VACR
VACR <- read.csv("VACR.csv")

# bucketing values into bins
group_tags <- cut(VACR$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

VACR$time.intervals <- group_tags

# Sort
VACR <- VACR %>% select(dataset_ID, meantimes, time.intervals, everything())
VACR <- subset(VACR, select = -c(dataset_ID))

# group by
VACR <- VACR %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:45,na.rm=TRUE,sum))

# Save
write.csv(VACR, file = "VACR_intervals500.csv", row.names = FALSE)
VACR_intervals500 <- read.csv("VACR_intervals500.csv")


#CLTV
CLTV <- read.csv("CLTV.csv")

# bucketing values into bins
group_tags <- cut(CLTV$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

CLTV$time.intervals <- group_tags

# Sort
CLTV <- CLTV %>% select(dataset_ID, meantimes, time.intervals, everything())
CLTV <- subset(CLTV, select = -c(dataset_ID))

# group by
CLTV <- CLTV %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:18,na.rm=TRUE,sum))

# Save
write.csv(CLTV, file = "CLTV_intervals500.csv", row.names = FALSE)
CLTV_intervals500 <- read.csv("CLTV_intervals500.csv")
