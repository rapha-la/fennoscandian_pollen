### ecogroupsN_1 ###

#All_Age_Depth_Curves
All_Age_Depth_Curves = readRDS(file = "./Output/All_Age_Depth_Curves_2021-11-23") 

## pollen_all
pollen_all = readRDS(file = "./Output/KMH_S1_pollen_all_2021-11-22")

# function to extract the eco.groups from Neotoma for every taxa in our dataset
eco.groups.neotoma = function(sitenum)
{
  taxon.name <- pollen_all[[sitenum]]$taxon.list$taxon.name
  ecological.group <- pollen_all[[sitenum]]$taxon.list$ecological.group
  df <- data.frame(taxon.name, ecological.group)
  eco.groups.neotoma = df
}


#make groupdf
groupdf = NULL
for(name in names(All_Age_Depth_Curves))
{
  sitenumdf = eco.groups.neotoma(name)
  if(name=="12"){
    groupdf = sitenumdf
  }
  else{
    groupdf = dplyr::bind_rows(groupdf, sitenumdf)
  }
}

groupdf$taxon.name <- sub("\\.type", "", groupdf$taxon.name)
groupdf$taxon.name <- sub("\\-type", "", groupdf$taxon.name)
groupdf$taxon.name <- sub("\\cf..", "", groupdf$taxon.name)
groupdf$taxon.name <- sub("\\undiff.", "", groupdf$taxon.name)
groupdf <- unique(groupdf)

write.csv(groupdf, file = "eco.groups.csv", row.names = FALSE)

# then open the file in excel and adapt the taxon.names according to the taxon names in bigdf_familynames.

ecogroupsN_1 <- read.csv("eco.groups.csv")
