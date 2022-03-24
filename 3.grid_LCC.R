### grid LCC's ###

library(tidyverse)
library(ade4)
library(funrar)
library(ecp)

LCC <- read.csv("LCC.csv")
all_temp_sites <- readRDS("all_temp_sites.RDS")

#arc-interval-function
make_interval100_arc = function(dataset){
  # set up cut-off values (every100)
  breaks <- seq(from = -100, to = 13000, by = 100)
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

#pol-interval
make_interval100_pol = function(dataset){
  # set up cut-off values (every100)
  breaks <- seq(from = -100, to = 13000, by = 100)
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

#plot_pol
plot_pol_LCC = function(dataset, ecp_data, titleinquotes) {
  ggplot(dataset, aes(x = meantimes)) + 
  scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                      labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
  geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
  geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
  geom_line(aes(y = wet_woodland_sum,colour="col3")) +
  geom_line(aes(y = pasture_sum,colour="col4")) + 
  geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
  geom_line(aes(y = arable_sum,colour="col6")) +
  geom_line(aes(y = heath_sum,colour="col7")) + 
  geom_vline(aes(xintercept = ecp_data$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_data$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_data$point3, colour = "changepoint")) +
  ggtitle(title) +
  labs(x = "Years BP",y = "relative abundance",colour = "Legend")
}

#clim_interval
make_interval100_clim = function(dataset){
  # set up cut-off values (every100)
  breaks <- seq(from = -100, to = 13000, by = 100)
  # bucketing values into bins
  group_tags <- cut(dataset$age, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE)
  dataset_interval <- dataset
  dataset_interval$time.intervals <- group_tags
  # group by
  dataset_interval <- dataset_interval %>%
    group_by(time.intervals) %>%
    summarise(
      mean_age = mean(age),
      mean_temp = mean(temp, na.rm = TRUE))
}

#make_relative_pol
relative_intervals_pol = function(dataframe){
  mat = subset(dataframe, select = -c(time.intervals, meantimes))
  mat = data.matrix(mat)
  mat_rel = make_relative(mat)
  df_rel_intervals = data.frame(mat_rel)
  df_rel_intervals$meantimes = dataframe$meantimes
  df_rel_intervals = df_rel_intervals %>% select(meantimes, everything())
}

#ecp_pol_int
ecp_pol = function(groupdf, df){
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

#ecp_pol
ecp_pol_rel = function(groupdf, df){
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

#ecp_clim
ecp_clim = function(groupdf, df){
  big=groupdf
  big_sorted = big[order(big$mean_age),]
  big_noids = subset(big_sorted,select=-c(mean_age))
  ecp_divisive_for_site = e.divisive(big_noids, k = NULL)
  point1 <- big$mean_age[ecp_divisive_for_site$estimates[2]]
  point2 <- big$mean_age[ecp_divisive_for_site$estimates[3]]
  point3 <- big$mean_age[ecp_divisive_for_site$estimates[4]]
  data.frame(df, point1, point2, point3)
}

### North_15 ###
#-------------------------------------------------------------------------------
### ARC ###
arc15_int = make_interval100_arc(arc15)
arc15_int <- arc15_int[-c(105),]

# Save
write.csv(arc15_int, file = "arc15_int.csv", row.names = FALSE)
arc15_int <- read.csv("arc15_int.csv")

# plot
ggplot(arc15_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
  geom_line(aes(y = nr_finds,colour="col1")) + 
  ggtitle("archaeological finds / North") +
  labs(x = "Year BP",y = "Nr of finds",colour = "Legend")


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
pol15_all_int <- make_interval100_pol(pol15_all)
pol15_int <- pol15_all_int[-c(129),]

#relative_abundance
pol15_int_rel = relative_intervals_pol(pol15_int)
write.csv(pol15_int_rel, file = "pol15_int_rel.csv", row.names = FALSE)
pol15_int_rel <- read.csv("pol15_int_rel.csv")

#ecp
ecp_pol15 <- ecp_pol(pol15_int, "pol15_int")
ecp_pol15_int_rel = ecp_pol_rel(pol15_int_rel, "pol15_int_rel")

#plot
ggplot(pol15_int_rel, aes(x = meantimes)) + 
  scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                      labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
  geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
  geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
  geom_line(aes(y = wet_woodland_sum,colour="col3")) +
  geom_line(aes(y = pasture_sum,colour="col4")) + 
  geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
  geom_line(aes(y = arable_sum,colour="col6")) +
  geom_line(aes(y = heath_sum,colour="col7")) + 
  geom_vline(aes(xintercept = ecp_pol15_int_rel$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol15_int_rel$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol15_int_rel$point3, colour = "changepoint")) +
  ggtitle("LCC_North") +
  labs(x = "Years BP",y = "relative abundance",colour = "Legend")


### CLIM15 ###

#test.list <- NULL
#for (site in clim15$site) {
#  age = all_temp_sites$site$paleoData[[1]]$measurementTable[[1]]$age$values
#  temp = all_temp_sites$site$paleoData[[1]]$measurementTable[[1]]$temperature$values
#  df <- data.frame(age, temp)
#  namelist = df
#  if(site=="850Lake.Shemesh.2001"){
#    test.list <- namelist
#  }
#  else{
#    test.list <- dplyr::bind_rows(test.list, namelist)
#  }
#}

print(clim15$site)

age = all_temp_sites$"850Lake.Shemesh.2001"$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$"850Lake.Shemesh.2001"$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_1 <- data.frame(age, temp)

age = all_temp_sites$AlanenLaanijarvi.Heinrichs.2005$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$AlanenLaanijarvi.Heinrichs.2005$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_2 <- data.frame(age, temp)

#
age = all_temp_sites$Njakajaure.Bigler.2006$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Njakajaure.Bigler.2006$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_3 <- data.frame(age, temp)

age = all_temp_sites$Njulla.Larocque.2004$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Njulla.Larocque.2004$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_4 <- data.frame(age, temp)

age = all_temp_sites$Tibetanus.Hammarlund.2002$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Tibetanus.Hammarlund.2002$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_5 <- data.frame(age, temp)

#
age = all_temp_sites$Tornetrask.Grudd.2002$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Tornetrask.Grudd.2002$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_6 <- data.frame(age, temp)

age = all_temp_sites[["Tsuolbmajavri.Korhola.2002"]][["paleoData"]][[2]][["measurementTable"]][[1]][["age"]][["values"]]
temp = all_temp_sites[["Tsuolbmajavri.Korhola.2002"]][["paleoData"]][[2]][["measurementTable"]][[1]][["temperature"]][["values"]]
clim15_7 <- data.frame(age, temp)

age = all_temp_sites$VuolepNjakajaure.Heinrichs.2006$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites[["VuolepNjakajaure.Heinrichs.2006"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-2"]][["values"]]
clim15_8 <- data.frame(age, temp)

age = all_temp_sites$Sokli.Shala.2017$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Sokli.Shala.2017$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_9 <- data.frame(age, temp)

age = all_temp_sites$Toskaljavri.Seppa.2002$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Toskaljavri.Seppa.2002$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_10 <- data.frame(age, temp)

age = all_temp_sites$Austerkjosen.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Austerkjosen.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_11 <- data.frame(age, temp)

age = all_temp_sites$Bjornfjell.Brooks.2006$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Bjornfjell.Brooks.2006$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_12 <- data.frame(age, temp)

age = all_temp_sites$Dalmutladdo.Bjune.2004$paleoData[[1]]$measurementTable[[2]]$age$values
temp = all_temp_sites$Dalmutladdo.Bjune.2004$paleoData[[1]]$measurementTable[[2]]$temperature$values
clim15_13 <- data.frame(age, temp)

age = all_temp_sites$Donvold.Nilssen.1983$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites[["Donvold.Nilssen.1983"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-1"]][["values"]]
clim15_14 <- data.frame(age, temp)

age = all_temp_sites$Gammelheimvatnet.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Gammelheimvatnet.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_15 <- data.frame(age, temp)

#
age = all_temp_sites$Lapland.Helama.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Lapland.Helama.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_16 <- data.frame(age, temp)

age = all_temp_sites$Liltlvatn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Liltlvatn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_17 <- data.frame(age, temp)

age = all_temp_sites$Myrvatn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Myrvatn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_18 <- data.frame(age, temp)

age = all_temp_sites$Chuna.Jones.2005$paleoData[[2]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Chuna.Jones.2005$paleoData[[2]]$measurementTable[[1]]$temperature$values
clim15_19 <- data.frame(age, temp)

age = all_temp_sites$KP2.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$KP2.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_20 <- data.frame(age, temp)

age = all_temp_sites$Yarnyshnoe.Seppa.2008$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Yarnyshnoe.Seppa.2008$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim15_21 <- data.frame(age, temp)

clim15_all <- rbind(clim15_1, clim15_2, clim15_4, clim15_5,clim15_7,clim15_9,clim15_10,clim15_11,
                    clim15_12,clim15_14,clim15_15,clim15_17,clim15_18,clim15_19,clim15_20,clim15_21)
# REMOVE
, clim15_3
,clim15_6
,clim15_16
,clim15_8
,clim15_13

clim15_int = make_interval100_clim(clim15_all)

# Save
write.csv(clim15_int, file = "clim15_int.csv", row.names = FALSE)
clim15_int <- read.csv("clim15_int.csv")

ecp_clim15_int = ecp_rel_clim(clim15_int, "clim15_int")

#plot
ggplot(clim15_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = mean_temp,colour="col1")) + 
  ylim(0,15) +
  geom_vline(aes(xintercept = ecp_clim15_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim15_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim15_int$point3, colour = "changepoint")) +
  ggtitle("temperature / North") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")



### SouthWest_1 ###
#-------------------------------------------------------------------------------
### ARC ###
arc1_int = make_interval100_arc(arc1)

# Save
write.csv(arc1_int, file = "arc1_int.csv", row.names = FALSE)
arc1_int <- read.csv("arc1_int.csv")

# plot
ggplot(arc1_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
  geom_line(aes(y = nr_finds,colour="col1")) + 
  ggtitle("archaeological finds / North") +
  labs(x = "Year BP",y = "Nr of finds",colour = "Legend")


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
pol1_all_interval = make_interval100_pol(pol1_all)
pol1_int = pol1_all_interval[-c(129),]

# Save
write.csv(pol1_int, file = "pol1_int.csv", row.names = FALSE)
pol1_int <- read.csv("pol1_int.csv")

#relative_abundance
pol1_int_rel = relative_intervals_pol(pol1_int)
write.csv(pol1_int_rel, file = "pol1_int_rel.csv", row.names = FALSE)
pol1_int_rel <- read.csv("pol1_int_rel.csv")

#ecp
ecp_pol1_int <- ecp_pol(pol1_int, "pol1_int")
ecp_pol1_int_rel <- ecp_pol_rel(pol1_int_rel, "pol1_int_rel")

#plot
ggplot(pol1_int_rel, aes(x = meantimes)) + 
  scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                      labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
  geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
  geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
  geom_line(aes(y = wet_woodland_sum,colour="col3")) +
  geom_line(aes(y = pasture_sum,colour="col4")) + 
  geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
  geom_line(aes(y = arable_sum,colour="col6")) +
  geom_line(aes(y = heath_sum,colour="col7")) + 
  geom_vline(aes(xintercept = ecp_pol1_int_rel$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol1_int_rel$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol1_int_rel$point3, colour = "changepoint")) +
  ggtitle("LCC_SouthWest") +
  labs(x = "Years BP",y = "relative abundance",colour = "Legend")


### CLIM1 ###
print(clim1$site)

age = all_temp_sites$Dalene.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Dalene.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim1_1 <- data.frame(age, temp)

age = all_temp_sites$Flotatjonn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Flotatjonn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim1_2 <- data.frame(age, temp)

age = all_temp_sites$Grostjorn.Eide.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Grostjorn.Eide.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim1_3 <- data.frame(age, temp)

age = all_temp_sites$Holebudalen.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Holebudalen.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim1_4 <- data.frame(age, temp)

age = all_temp_sites$Isbenttjonn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Isbenttjonn.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim1_5 <- data.frame(age, temp)

age = all_temp_sites$Reiarsdalvatnet.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Reiarsdalvatnet.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim1_6 <- data.frame(age, temp)

age = all_temp_sites$VestreOykjamyrtorn.EPD$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites[["VestreOykjamyrtorn.EPD"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperature-1"]][["values"]]
clim1_7 <- data.frame(age, temp)

age = all_temp_sites$VestreOykjamytjorn.Velle.2005$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$VestreOykjamytjorn.Velle.2005$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim1_8 <- data.frame(age, temp)


clim1_all <- rbind(clim1_2, clim1_3, clim1_4, clim1_5,clim1_6,clim1_8)

# REMOVE
clim1_1, 
clim1_7,


clim1_int = make_interval100_clim(clim1_all)

# Save
write.csv(clim1_int, file = "clim1_int.csv", row.names = FALSE)
clim1_int <- read.csv("clim1_int.csv")

ecp_clim1_int = ecp_rel_clim(clim1_int, "clim1_int")

#plot
ggplot(clim1_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = mean_temp,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim1_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim1_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim1_int$point3, colour = "changepoint")) +
  ggtitle("temperature / SouthWest") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### MidWest_2 ###
#-------------------------------------------------------------------------------
### ARC ###
arc2_int = make_interval100_arc(arc2)

# Save
write.csv(arc2_int, file = "arc2_int.csv", row.names = FALSE)
arc2_int <- read.csv("arc2_int.csv")

# plot
ggplot(arc2_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
  geom_line(aes(y = nr_finds,colour="col1")) + 
  ggtitle("archaeological finds / North") +
  labs(x = "Year BP",y = "Nr of finds",colour = "Legend")


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
pol2_all_interval = make_interval100_pol(pol2_all)
pol2_int = pol2_all_interval[-c(129),]

# Save
write.csv(pol2_int, file = "pol2_int.csv", row.names = FALSE)
pol2_int <- read.csv("pol2_int.csv")

#relative_abundance
pol2_int_rel = relative_intervals_pol(pol2_int)
write.csv(pol2_int_rel, file = "pol2_int_rel.csv", row.names = FALSE)
pol2_int_rel <- read.csv("pol2_int_rel.csv")

#ecp
ecp_pol2_int_rel <- ecp_pol_rel(pol2_int_rel, "pol2_int_rel")
ecp_pol2_int = ecp_pol(pol2_int, "pol2_int")

#plot
ggplot(pol2_int_rel, aes(x = meantimes)) + 
  scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                      labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
  geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
  geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
  geom_line(aes(y = wet_woodland_sum,colour="col3")) +
  geom_line(aes(y = pasture_sum,colour="col4")) + 
  geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
  geom_line(aes(y = arable_sum,colour="col6")) +
  geom_line(aes(y = heath_sum,colour="col7")) + 
  geom_vline(aes(xintercept = ecp_pol2_int_rel$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol2_int_rel$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol2_int_rel$point3, colour = "changepoint")) +
  ggtitle("LCC_MidWest") +
  labs(x = "Years BP",y = "relative abundance",colour = "Legend")


### clim2 ###
print(clim2$site)

age = all_temp_sites$Ratasjoen.Velle.2005$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Ratasjoen.Velle.2005$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim2_1 <- data.frame(age, temp)

age = all_temp_sites$Tiavatnet.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Tiavatnet.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim2_2 <- data.frame(age, temp)

age = all_temp_sites$Topptjonna.Paus.2011$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Topptjonna.Paus.2011$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim2_3 <- data.frame(age, temp)

age = all_temp_sites$Trettetjorn.Bjune.2005$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Trettetjorn.Bjune.2005$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim2_4 <- data.frame(age, temp)

clim2_all <- rbind(clim2_1, clim2_2, clim2_3, clim2_4)

clim2_int = make_interval100_clim(clim2_all)

# Save
write.csv(clim2_int, file = "clim2_int.csv", row.names = FALSE)
clim2_int <- read.csv("clim2_int.csv")

ecp_clim2_int = ecp_rel_clim(clim2_int, "clim2_int")

#plot
ggplot(clim2_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = mean_temp,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim2_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim2_int$point3, colour = "changepoint")) +
  ggtitle("temperature / North") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### SouthMid_36 ###
#-------------------------------------------------------------------------------
### ARC ###
arc36_int = make_interval100_arc(arc36)

# Save
write.csv(arc36_int, file = "arc36_int.csv", row.names = FALSE)
arc36_int <- read.csv("arc36_int.csv")

# plot
ggplot(arc36_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
  geom_line(aes(y = nr_finds,colour="col1")) + 
  ggtitle("archaeological finds / North") +
  labs(x = "Year BP",y = "Nr of finds",colour = "Legend")


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
pol36_all_interval = make_interval100_pol(pol36_all)
pol36_int = pol36_all_interval[-c(112),]

# Save
write.csv(pol36_int, file = "pol36_int.csv", row.names = FALSE)
pol36_int <- read.csv("pol36_int.csv")

#relative_abundance
pol36_int_rel = relative_intervals_pol(pol36_int)
write.csv(pol36_int_rel, file = "pol36_int_rel.csv", row.names = FALSE)
pol36_int_rel <- read.csv("pol36_int_rel.csv")

#ecp
ecp_pol36_int <- ecp_pol(pol36_int, "pol36_int")
ecp_pol36_int_rel <- ecp_pol_rel(pol36_int_rel, "pol36_int_rel")

#plot
ggplot(pol36_int_rel, aes(x = meantimes)) + 
  scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                      labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
  geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
  geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
  geom_line(aes(y = wet_woodland_sum,colour="col3")) +
  geom_line(aes(y = pasture_sum,colour="col4")) + 
  geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
  geom_line(aes(y = arable_sum,colour="col6")) +
  geom_line(aes(y = heath_sum,colour="col7")) + 
  geom_vline(aes(xintercept = ecp_pol36_int_rel$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol36_int_rel$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol36_int_rel$point3, colour = "changepoint")) +
  ggtitle("LCC_SouthMid") +
  labs(x = "Years BP",y = "relative abundance",colour = "Legend")


### clim36 ###
print(clim36$site)

age = all_temp_sites$AgeroedsMosse.Nilsson.1964$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites[["AgeroedsMosse.Nilsson.1964"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
clim36_1 <- data.frame(age, temp)

age = all_temp_sites$Flarken.Berglund.1966$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites[["Flarken.Berglund.1966"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
clim36_2 <- data.frame(age, temp)

age = all_temp_sites$Flarken.Seppa.2005$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Flarken.Seppa.2005$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim36_3 <- data.frame(age, temp)

age = all_temp_sites$Gloppsjon.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Gloppsjon.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim36_4 <- data.frame(age, temp)

age = all_temp_sites$Kansjon.EPD$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites[["Kansjon.EPD"]][["paleoData"]][[1]][["measurementTable"]][[1]][["temperatureComposite"]][["values"]]
clim36_5 <- data.frame(age, temp)

age = all_temp_sites$Trehorningen.Antonsson.2007$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Trehorningen.Antonsson.2007$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim36_6 <- data.frame(age, temp)

clim36_all <- rbind(clim36_1, clim36_2, clim36_3, clim36_4, clim36_5, clim36_6)

clim36_int = make_interval100_clim(clim36_all)

# Save
write.csv(clim36_int, file = "clim36_int.csv", row.names = FALSE)
clim36_int <- read.csv("clim36_int.csv")

ecp_clim36_int = ecp_rel_clim(clim36_int, "clim36_int")

#plot
ggplot(clim36_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = mean_temp,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim36_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim36_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim36_int$point3, colour = "changepoint")) +
  ggtitle("temperature / SouthMid") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### MidMid_47 ###
#-------------------------------------------------------------------------------
### ARC ###
arc47_int = make_interval100_arc(arc47)

# Save
write.csv(arc47_int, file = "arc47_int.csv", row.names = FALSE)
arc47_int <- read.csv("arc47_int.csv")

# plot
ggplot(arc47_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
  geom_line(aes(y = nr_finds,colour="col1")) + 
  ggtitle("archaeological finds / North") +
  labs(x = "Year BP",y = "Nr of finds",colour = "Legend")


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
pol47_all_interval = make_interval100_pol(pol47_all)
pol47_int = pol47_all_interval[-c(129),]

# Save
write.csv(pol47_int, file = "pol47_int.csv", row.names = FALSE)
pol47_int <- read.csv("pol47_int.csv")

#relative_abundance
pol47_int_rel = relative_intervals_pol(pol47_int)
write.csv(pol47_int_rel, file = "pol47_int_rel.csv", row.names = FALSE)
pol47_int_rel <- read.csv("pol47_int_rel.csv")

#ecp
ecp_pol47_int <- ecp_pol(pol47_int, "pol47_int")
ecp_pol47_int_rel <- ecp_pol_rel(pol47_int_rel, "pol47_int_rel")

#plot
ggplot(pol47_int_rel, aes(x = meantimes)) + 
  scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                      labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
  geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
  geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
  geom_line(aes(y = wet_woodland_sum,colour="col3")) +
  geom_line(aes(y = pasture_sum,colour="col4")) + 
  geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
  geom_line(aes(y = arable_sum,colour="col6")) +
  geom_line(aes(y = heath_sum,colour="col7")) + 
  geom_vline(aes(xintercept = ecp_pol47_int_rel$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol47_int_rel$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol47_int_rel$point3, colour = "changepoint")) +
  ggtitle("LCC_MidMid") +
  labs(x = "Years BP",y = "relative abundance",colour = "Legend")


### clim47 ###
print(clim47$site)

age = all_temp_sites$Gilltjarnen.Antonsson.2006$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Gilltjarnen.Antonsson.2006$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim47_1 <- data.frame(age, temp)

age = all_temp_sites$Holtjaernen.Giesecke.2008$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Holtjaernen.Giesecke.2008$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim47_2 <- data.frame(age, temp)

age = all_temp_sites$Klotjaernen.Pollen.Sweden$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Klotjaernen.Pollen.Sweden$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim47_3 <- data.frame(age, temp)

age = all_temp_sites$Spaime.Hammarlund.2004$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Spaime.Hammarlund.2004$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim47_4 <- data.frame(age, temp)

age = all_temp_sites$Haugtjern.Eide.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Haugtjern.Eide.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim47_5 <- data.frame(age, temp)

age = all_temp_sites$Kinnshaugen.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Kinnshaugen.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim47_6 <- data.frame(age, temp)

clim47_all <- rbind(clim47_1, clim47_2, clim47_3, clim47_4, clim47_5, clim47_6)

clim47_int = make_interval100_clim(clim47_all)

# Save
write.csv(clim47_int, file = "clim47_int.csv", row.names = FALSE)
clim47_int <- read.csv("clim47_int.csv")

ecp_clim47_int = ecp_rel_clim(clim47_int, "clim47_int")

#plot
ggplot(clim47_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = mean_temp,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim47_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim47_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim47_int$point3, colour = "changepoint")) +
  ggtitle("temperature / North") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")


### SouthEast_911 ###
#-------------------------------------------------------------------------------
### ARC ###
arc911_int = make_interval100_arc(arc911)

# Save
write.csv(arc911_int, file = "arc911_int.csv", row.names = FALSE)
arc911_int <- read.csv("arc911_int.csv")

# plot
ggplot(arc911_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="orange"),labels=c("arc. finds")) +
  geom_line(aes(y = nr_finds,colour="col1")) + 
  ggtitle("archaeological finds / North") +
  labs(x = "Year BP",y = "Nr of finds",colour = "Legend")


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
pol911_all_interval = make_interval100_pol(pol911_all)
pol911_int = pol911_all_interval

# Save
write.csv(pol911_int, file = "pol911_int.csv", row.names = FALSE)
pol911_int <- read.csv("pol911_int.csv")


#relative_abundance
pol911_int_rel = relative_intervals_pol(pol911_int)
write.csv(pol911_int_rel, file = "pol911_int_rel.csv", row.names = FALSE)
pol911_int_rel <- read.csv("pol911_int_rel.csv")

#ecp
ecp_pol911_int <- ecp_pol(pol911_int, "pol911_int")
ecp_pol911_int_rel <- ecp_pol_rel(pol911_int_rel, "pol911_int_rel")

#plot
ggplot(pol911_int_rel, aes(x = meantimes)) + 
  scale_colour_manual(values=c(col1="darkgreen",col2="green2", col3="darkslategray3", col4="violet", col5="tan3", col6="red2", col7="blue", changepoint="black") ,
                      labels=c("coniferous_woodland","deciduous_woodland","wet_woodland","pasture/meadow","wet_meadow","arable","heath", "changepoint")) +
  geom_line(aes(y = coniferous_woodland_sum,colour="col1")) + 
  geom_line(aes(y = deciduous_woodland_sum,colour="col2")) + 
  geom_line(aes(y = wet_woodland_sum,colour="col3")) +
  geom_line(aes(y = pasture_sum,colour="col4")) + 
  geom_line(aes(y = wet_meadow_sum,colour="col5")) + 
  geom_line(aes(y = arable_sum,colour="col6")) +
  geom_line(aes(y = heath_sum,colour="col7")) + 
  geom_vline(aes(xintercept = ecp_pol911_int_rel$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol911_int_rel$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_pol911_int_rel$point3, colour = "changepoint")) +
  ggtitle("LCC_SouthEast") +
  labs(x = "Years BP",y = "relative abundance",colour = "Legend")


### clim911 ###
print(clim911$site)

age = all_temp_sites$Hirvijaervi.Luoto.2010$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Hirvijaervi.Luoto.2010$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim911_1 <- data.frame(age, temp)

age = all_temp_sites$Kaartlamminsuo.Rankama.1988$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Kaartlamminsuo.Rankama.1988$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim911_2 <- data.frame(age, temp)

age = all_temp_sites$Laihalampi.Heikkila.2003$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Laihalampi.Heikkila.2003$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim911_3 <- data.frame(age, temp)

age = all_temp_sites$Laihalampi.Giesecke.2008$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Laihalampi.Giesecke.2008$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim911_4 <- data.frame(age, temp)

age = all_temp_sites$Nautajarvi.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Nautajarvi.Seppa.2009$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim911_5 <- data.frame(age, temp)

age = all_temp_sites$Ylimysneva.Huttunen.1990$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Ylimysneva.Huttunen.1990$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim911_6 <- data.frame(age, temp)

age = all_temp_sites$Medvedevskoe.Nazarova.2018$paleoData[[1]]$measurementTable[[1]]$age$values
temp = all_temp_sites$Medvedevskoe.Nazarova.2018$paleoData[[1]]$measurementTable[[1]]$temperature$values
clim911_7 <- data.frame(age, temp)

clim911_all <- rbind(clim911_1, clim911_2, clim911_3, clim911_4, clim911_5, clim911_6, clim911_7)

clim911_int = make_interval100_clim(clim911_all)

# Save
write.csv(clim911_int, file = "clim911_int.csv", row.names = FALSE)
clim911_int <- read.csv("clim911_int.csv")

ecp_clim911_int = ecp_rel_clim(clim911_int, "clim911_int")

#plot
ggplot(clim911_int, aes(x = mean_age)) + 
  scale_colour_manual(values=c(col1="blue2",changepoint="black"),labels=c("temp", "changepoint")) +
  geom_line(aes(y = mean_temp,colour="col1")) + 
  geom_vline(aes(xintercept = ecp_clim911_int$point1, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim911_int$point2, colour = "changepoint")) +
  geom_vline(aes(xintercept = ecp_clim911_int$point3, colour = "changepoint")) +
  ggtitle("temperature / North") +
  labs(x = "Year BP",y = "Count Nr",colour = "Legend")

