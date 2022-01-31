### ECOLOGICAL GROUPS ###

eco.groups.neotoma = function(pollen_all, All_Age_Depth_Curves, sitenum)
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












sitenumbers = c(12,
      20,
      317,
      720,
      977,
      1438,
      4092,
      4133,
      4136,
      4156,
      4168,
      4257,
      4259,
      4286,
      4372,
      4393,
      4403,
      4420,
      4468,
      4472,
      4539,
      19906,
      19909,
      19913,
      20018,
      20034,
      20042,
      20046,
      20050,
      20279,
      20285,
      20293,
      21790,
      44941,
      45311,
      45329,
      45331,
      45345,
      45347,
      45349,
      45351,
      45636,
      45639,
      45642,
      45698,
      45701,
      45704,
      45707,
      45710,
      45713,
      45716,
      45719,
      45722,
      45725,
      45728,
      45731,
      45757,
      4169,
      4163,
      4543,
      4017,
      3928)

for (i in 1:sitenumbers) {
  eco.group.[i] <- eco.groups.neotoma("[i]")
}


