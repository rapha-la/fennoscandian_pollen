### time-intervals for North, South and whole Fennoscandia ###

library(tidyverse)


# set up cut-off values (every100)
breaks <- seq(from = -100, to = 13000, by = 100)

                  
#FENNOSCANDIA
# Load the dataframe "bigdf_familynames".
load("bigdf_familynames.Rda")
bigdf_familynames <- subset(bigdf_familynames, select = -c(333))

# bucketing values into bins
group_tags <- cut(bigdf_familynames$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

FENNOSCANDIA_interval <- bigdf_familynames
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
write.csv(by_FENNOSCANDIA_interval, file = "FENNOSCANDIA_genera_every100.csv", row.names = FALSE)
FENNOSCANDIA_interval <- read.csv("FENNOSCANDIA_genera_every100.csv")


#NORTH
NORTH_interval <- read.csv("NORTH.csv")

# bucketing values into bins
group_tags <- cut(NORTH_interval$meantimes, 
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
    across(2:262,na.rm=TRUE,sum))

# Save
write.csv(by_NORTH_interval, file = "NORTH_intervals100.csv", row.names = FALSE)
NORTH_intervals100 <- read.csv("NORTH_intervals100.csv")


#SOUTH
SOUTH_interval <- read.csv("SOUTH.csv")

# bucketing values into bins
group_tags <- cut(SOUTH_interval$meantimes, 
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
    across(2:434,na.rm=TRUE,sum))

# Save
write.csv(by_SOUTH_interval, file = "SOUTH_intervals100.csv", row.names = FALSE)
SOUTH_intervals100 <- read.csv("SOUTH_intervals100.csv")


#ECO.GROUPS

eco_groups_header <- read.csv("eco_groups_CLTV.csv")

# bucketing values into bins
group_tags <- cut(eco_groups_header$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

eco_groups_intervals <- eco_groups_header
eco_groups_intervals$time.intervals <- group_tags

# Sort
eco_groups_intervals <- eco_groups_intervals %>% select(dataset_ID, meantimes, time.intervals, everything())
eco_groups_intervals <- subset(eco_groups_intervals, select = -c(dataset_ID))

# group by
eco_groups_intervals <- eco_groups_intervals %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(2:6,na.rm=TRUE,sum))

# Save
write.csv(eco_groups_intervals, file = "eco_groups_intervals100.csv", row.names = FALSE)
eco_groups_intervals100 <- read.csv("eco_groups_intervals100.csv")


#NORTH
NORTH_groups <- read.csv("NORTH_groups.csv")
NORTH_interval <- NORTH_groups

# bucketing values into bins
group_tags <- cut(NORTH_interval$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

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
write.csv(by_NORTH_interval, file = "NORTHgroups_interval100.csv", row.names = FALSE)
NORTHgroups_interval100 <- read.csv("NORTHgroups_interval100.csv")


#SOUTH
SOUTH_groups <- read.csv("SOUTH_groups.csv")
SOUTH_interval <- SOUTH_groups

# bucketing values into bins
group_tags <- cut(SOUTH_groups$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)

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
write.csv(by_SOUTH_interval, file = "SOUTHgroups_interval100.csv", row.names = FALSE)
SOUTHgroups_interval100 <- read.csv("SOUTHgroups_interval100.csv")


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
write.csv(AQVP, file = "AQVP_intervals.csv", row.names = FALSE)
AQVP_intervals <- read.csv("AQVP_intervals.csv")


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
write.csv(TRSH, file = "TRSH_intervals.csv", row.names = FALSE)
TRSH_intervals <- read.csv("TRSH_intervals.csv")

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
write.csv(UPHE, file = "UPHE_intervals.csv", row.names = FALSE)
UPHE_intervals <- read.csv("UPHE_intervals.csv")

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
write.csv(VACR, file = "VACR_intervals.csv", row.names = FALSE)
VACR_intervals <- read.csv("VACR_intervals.csv")


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
write.csv(CLTV, file = "CLTV_intervals.csv", row.names = FALSE)
CLTV_intervals <- read.csv("CLTV_intervals.csv")

