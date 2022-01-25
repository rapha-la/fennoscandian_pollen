### ecp multivariate change point test ###

# the dataframe needed must be loaded!

library(ecp)
library(ggplot2)


# ecp-function
ecp_divisive_for_site = function(dataset_ID){
  big12=bigdf_genera[bigdf_genera$dataset_ID==dataset_ID,]
  big12_sorted = big12[order(big12$meantimes),]
  big12_restricted = big12_sorted
  big12_restricted_nonas = big12_restricted[,colSums(is.na(big12_restricted)) < nrow(big12_restricted)]
  big12_noids = subset(big12_restricted_nonas,select=-c(meantimes,dataset_ID))
  big12_scaled = scale(big12_noids,scale = FALSE)
  ecp_divisive_for_site = e.divisive(big12_scaled, k = NULL)
}


### plot function ###

# This function works.
plot_ecp_site = function(SiteNR, changepoint1, changepoint2){
  big = bigdf_genera[bigdf_genera$dataset_ID==SiteNR,]
  big = big[order(big$meantimes),]
  big_main = big[,c(2,7,24,25)]
  ecp = ecp_divisive_for_site(SiteNR)
  ecp.df = data.frame(big_main, ecp$cluster)
  colours <- c("Betula" = "green", "Picea" = "blue", "Pinus" = "orange")
  ggplot(ecp.df, aes(x = meantimes)) + 
    geom_line(aes(y = Betula, colour = "Betula")) + 
    geom_line(aes(y = Picea, colour = "Picea")) +
    geom_line(aes(y = Pinus, colour = "Pinus")) +
    geom_vline(aes(xintercept = changepoint1, colour = "black")) +
    geom_vline(aes(xintercept = changepoint2, colour = "black")) +
    ggtitle(SiteNR) +
    labs(x = "Year BP",y = "Count Nr",colour = "Legend") +
    scale_colour_manual(values = colours)
}

# 1. PROBLEM: I tried to include the arguments "genus1", "genus2" and "genus3", so we can choose the different genera
# that we want to visualise. This doesn't work, because to create "big_main" the column names need to be in
# quotation marks and if the names are in quotation marks the function will not recognize them. I tried to
# solve this with deparse(substitute()) or as in the following function as "big[[]]" but I still get an error message.

# 2. PROBLEM: I tried to find a solution on how to automatically include the change points, but it doesn't work.
# Also it isn't a good solution to specify the change point numbers as I did: "ecp$estimates[2]".

plot_ecp_site = function(SiteNR, genus1, genus2, genus3){
  big = bigdf_genera[bigdf_genera$dataset_ID==SiteNR,]
  big = big[order(big$meantimes),]
  ecp = ecp_divisive_for_site(SiteNR)
  colours <- c(big[[genus1]] = "green", big[[genus2]] = "blue", big[[genus3]] = "orange")
  ggplot(big, aes(x = meantimes)) + 
    geom_line(aes(y = genus1, colour = big[[genus3]])) + 
    geom_line(aes(y = genus2, colour = big[[genus3]])) +
    geom_line(aes(y = genus3, colour = big[[genus3]])) +
    geom_vline(aes(xintercept = ecp$estimates[2], colour = "black")) +
    geom_vline(aes(xintercept = ecp$estimates[3], colour = "black")) +
    ggtitle(SiteNR) +
    labs(x = "Year BP",y = "Count Nr",colour = "Legend") +
    scale_colour_manual(values = colours)
}
