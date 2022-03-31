### differentSums (functions and files) ###

#install.packages("plyr")
#install.packages("dplyr")
library(plyr)
library(dplyr)
library(tidyverse)
library(ade4)
library(funrar)
library(ecp)
library(ggplot2)

#FILES
coniferous_woodland <- read.csv("coniferous_woodland.csv")
LCC <- read.csv("LCC.csv")

#FUNCTIONS
#CallSites_N
CallSites_N = function(df){
  pol15_1 <-  subset(df, dataset_ID == "20")
  pol15_2 <-  subset(df, dataset_ID == "317")
  pol15_3 <-  subset(df,dataset_ID == "720")
  pol15_4 <-  subset(df,dataset_ID == "4257")
  pol15_5 <-  subset(df,dataset_ID == "4286")
  pol15_6 <-  subset(df,dataset_ID == "4372")
  pol15_7 <-  subset(df,dataset_ID == "4468")
  pol15_8 <-  subset(df,dataset_ID == "20034")
  pol15_9 <-  subset(df,dataset_ID == "20279")
  pol15_10 <-  subset(df,dataset_ID == "20285")
  pol15_11 <-  subset(df,dataset_ID == "20293")
  pol15_12 <-  subset(df,dataset_ID == "44941")
  pol15_13 <-  subset(df,dataset_ID == "45311")
  pol15_14 <-  subset(df,dataset_ID == "45636")
  pol15_15 <-  subset(df,dataset_ID == "45639")
  pol15_16 <-  subset(df,dataset_ID == "45642")
  pol15_17 <-  subset(df,dataset_ID == "24757")
  pol15_18 <-  subset(df,dataset_ID == "4169")
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
}

#make_interval_pol
make_interval_pol = function(dataset, interval, earliest_date){
  dataset_interval <- dataset
  dataset_interval$lower_ends = floor(dataset_interval$meantimes/interval)*interval
  dataset_interval <- subset(dataset_interval, select = -c(dataset_ID))
  dataset_interval <- dataset_interval %>%
    group_by(lower_ends) %>%
    summarise(
      meantimes = mean(meantimes),
      across(2:ncol(dataset_interval)-1,na.rm=TRUE,sum))
  dataset_interval <- dataset_interval[-c(103:nrow(dataset_interval)),]
}

#make increments
makeIncrements = function(df){
  df_inc <- df[2:(nrow(df)),2:ncol(df)]-df[1:(nrow(df)-1),2:ncol(df)]
  df_inc$lower_ends = df$lower_ends[2:nrow(df)]
  return(df_inc)
}

#ecp_inc
ecp_inc = function(groupdf, df){
  big=groupdf
  big_sorted = big[order(big$lower_ends),]
  big_restricted = big_sorted
  big_restricted_nonas = big_restricted[,colSums(is.na(big_restricted)) < nrow(big_restricted)]
  big_noids = subset(big_restricted_nonas,select=-c(lower_ends))
  big_scaled = scale(big_noids,scale = FALSE)
  ecp_divisive_for_site = e.divisive(big_scaled, k = NULL, min.size = 5)
  point1 <- big$lower_ends[ecp_divisive_for_site$estimates[2]]
  point2 <- big$lower_ends[ecp_divisive_for_site$estimates[3]]
  point3 <- big$lower_ends[ecp_divisive_for_site$estimates[4]]
  data.frame(df, point1, point2, point3)
}

#mcp on LCC
mcp_LCC = function(df){
  mcp_data = df
  model = list(LCC~age+1, 1~age+1)  # two intercept-only segments?
  model_null = list(LCC~age+1)
  fit_mcp = mcp(model, data = mcp_data, par_x = "age")
  fit_mcp_null = mcp(model_null, data = mcp_data, par_x = "age")
  summary(fit_mcp)
  plot(fit_mcp)
}

#ecp_sums
ecp_sums = function(groupdf, df){
  big=groupdf
  big_sorted = big[order(big$age),]
  ecp_divisive_for_site = e.divisive(big_sorted, k = NULL, min.size = 5)
  point1 <- big$age[ecp_divisive_for_site$estimates[2]]
  point2 <- big$age[ecp_divisive_for_site$estimates[3]]
  point3 <- big$age[ecp_divisive_for_site$estimates[4]]
  data.frame(df, point1, point2, point3)
}

#make sqrt
sqrt_sums = function(df){
  df_sqrt <- sqrt(df[2:ncol(df)])
  df_sqrt$age <- df$age
  df_sqrt <- df_sqrt %>% select(age, everything())
}

#make increments
makeIncrements_singleLCC = function(df){
  df[2:(nrow(df)),2:ncol(df)]-df[1:(nrow(df)-1),2:ncol(df)]
}
