library(dplyr)
library(tidyverse)

CountTaxaPerInterval = function(df, interval_length){
  dfcopy = df
  dfcopy$lower_ends = floor(dfcopy$meantimes/interval_length)*interval_length
  outdf = cbind(unique(dfcopy$lower_ends),unique(dfcopy$lower_ends))
  dfcopy = dfcopy %>% select(lower_ends, dataset_ID, meantimes, everything())
  dfcopy2 = data.frame(dfcopy[4:ncol(dfcopy)])
  dfcopy2[is.na(dfcopy2)] <- 0
  dfcopy2[dfcopy2 != 0] <- 1
  dfcopy = data.frame(dfcopy[1], dfcopy2)
  for(i in 1:nrow(outdf)){
    df_restricted = dfcopy[dfcopy$lower_ends==outdf[i,1],]
    df_restricted = df_restricted[2:ncol(df_restricted)]
    df_restricted = colSums(df_restricted)
    outdf[1,2] = length(df_restricted != 0)
  }
  outdf = outdf[order(outdf[,1]),]
  return(outdf)
}
