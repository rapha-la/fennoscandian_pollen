# North and South

load("bigdf_familynames.Rda")

StandardDF = function(pollen_all, sitenum)
{
  data.frame(site=sitenum, long=pollen_all[[sitenum]]$dataset$site.data$long ,
             lat=pollen_all[[sitenum]]$dataset$site.data$lat )
}

#make bigdf
bigdf = NULL
for(dataset_ID in bigdf_familynames$dataset_ID)
{
  namedf = StandardDF(pollen_all, dataset_ID)
  if(dataset_ID=="12"){
    bigdf = namedf
  }
  else{
    bigdf = dplyr::bind_rows(bigdf, namedf)
  }
}

bigdf.S <- subset(bigdf, lat <= 65)
unique(bigdf.S$site)


#SOUTH
SOUTH <- bigdf_familynames

SOUTH1 <-  filter(SOUTH, dataset_ID == "12")
SOUTH2 <-  filter(SOUTH, dataset_ID == "977")
SOUTH3 <-  filter(SOUTH, dataset_ID == "1438")
SOUTH4 <-  filter(SOUTH, dataset_ID == "4092")
SOUTH5 <-  filter(SOUTH, dataset_ID == "4133")
SOUTH6 <-  filter(SOUTH, dataset_ID == "4156")
SOUTH7 <-  filter(SOUTH, dataset_ID == "4168")
SOUTH8 <-  filter(SOUTH, dataset_ID == "4259")
SOUTH9 <-  filter(SOUTH, dataset_ID == "4393")
SOUTH10 <-  filter(SOUTH, dataset_ID == "4403")
SOUTH11 <-  filter(SOUTH, dataset_ID == "4420")
SOUTH12 <-  filter(SOUTH, dataset_ID == "4472")
SOUTH13 <-  filter(SOUTH, dataset_ID == "4539")
SOUTH14 <-  filter(SOUTH, dataset_ID == "19906")
SOUTH15 <-  filter(SOUTH, dataset_ID == "19909")
SOUTH16 <-  filter(SOUTH, dataset_ID == "19913")
SOUTH17 <-  filter(SOUTH, dataset_ID == "20018")
SOUTH18 <-  filter(SOUTH, dataset_ID == "20042")
SOUTH19 <-  filter(SOUTH, dataset_ID == "20046")
SOUTH20 <-  filter(SOUTH, dataset_ID == "20050")
SOUTH21 <-  filter(SOUTH, dataset_ID == "21790")
SOUTH22 <-  filter(SOUTH, dataset_ID == "45329")
SOUTH23 <-  filter(SOUTH, dataset_ID == "45331")
SOUTH24 <-  filter(SOUTH, dataset_ID == "45345")
SOUTH25 <-  filter(SOUTH, dataset_ID == "45347")
SOUTH26 <-  filter(SOUTH, dataset_ID == "45349")
SOUTH27 <-  filter(SOUTH, dataset_ID == "45351")
SOUTH28 <-  filter(SOUTH, dataset_ID == "45698")
SOUTH29 <-  filter(SOUTH, dataset_ID == "45701")
SOUTH30 <-  filter(SOUTH, dataset_ID == "45704")
SOUTH31 <-  filter(SOUTH, dataset_ID == "45707")
SOUTH32 <-  filter(SOUTH, dataset_ID == "45710")
SOUTH33 <-  filter(SOUTH, dataset_ID == "45713")
SOUTH34 <-  filter(SOUTH, dataset_ID == "45716")
SOUTH35 <-  filter(SOUTH, dataset_ID == "45719")
SOUTH36 <-  filter(SOUTH, dataset_ID == "45722")
SOUTH37 <-  filter(SOUTH, dataset_ID == "45725")
SOUTH38 <-  filter(SOUTH, dataset_ID == "45728")
SOUTH39 <-  filter(SOUTH, dataset_ID == "45731")
SOUTH40 <-  filter(SOUTH, dataset_ID == "4543")
SOUTH41 <-  filter(SOUTH, dataset_ID == "4017")
SOUTH42 <-  filter(SOUTH, dataset_ID == "3928")

SOUTH <- rbind(SOUTH1,
                        SOUTH2,
                        SOUTH3,
                        SOUTH4,
                        SOUTH5,
                        SOUTH6,
                        SOUTH7,
                        SOUTH8,
                        SOUTH9,
                        SOUTH10,
                        SOUTH11,
                        SOUTH12,
                        SOUTH13,
                        SOUTH14,
                        SOUTH15,
                        SOUTH16,
                        SOUTH17,
                        SOUTH18,
                        SOUTH19,
                        SOUTH20,
                        SOUTH21,
                        SOUTH22,
                        SOUTH23,
                        SOUTH24,
                        SOUTH25,
                        SOUTH26,
                        SOUTH27,
                        SOUTH28,
                        SOUTH29,
                        SOUTH30,
                        SOUTH31,
                        SOUTH32,
                        SOUTH33,
                        SOUTH34,
                        SOUTH35,
                        SOUTH36,
                        SOUTH37,
                        SOUTH38,
                        SOUTH39,
                        SOUTH40,
                        SOUTH41,
                        SOUTH42)

# Save
write.csv(SOUTH, file = "SOUTH.csv", row.names = FALSE)
SOUTH <- read.csv("SOUTH.csv")


#NORTH
bigdf.N <- subset(bigdf, lat > 65)
unique(bigdf.N$site)

NORTH <- bigdf_familynames
NORTH1 <-  filter(NORTH, dataset_ID == "20")
NORTH2 <-  filter(NORTH, dataset_ID == "317")
NORTH3 <-  filter(NORTH, dataset_ID == "720")
NORTH4 <-  filter(NORTH, dataset_ID == "4136")
NORTH5 <-  filter(NORTH, dataset_ID == "4257")
NORTH6 <-  filter(NORTH, dataset_ID == "4286")
NORTH7<-  filter(NORTH, dataset_ID == "4372")
NORTH8 <-  filter(NORTH, dataset_ID == "4468")
NORTH9 <-  filter(NORTH, dataset_ID == "20034")
NORTH10 <-  filter(NORTH, dataset_ID == "20279")
NORTH11 <-  filter(NORTH, dataset_ID == "20285")
NORTH12 <-  filter(NORTH, dataset_ID == "20293")
NORTH13 <-  filter(NORTH, dataset_ID == "44941")
NORTH14 <-  filter(NORTH, dataset_ID == "45311")
NORTH15 <-  filter(NORTH, dataset_ID == "45636")
NORTH16 <-  filter(NORTH, dataset_ID == "45639")
NORTH17 <-  filter(NORTH, dataset_ID == "45642")
NORTH18 <-  filter(NORTH, dataset_ID == "4169")
NORTH19 <-  filter(NORTH, dataset_ID == "4163")
NORTH20 <-  filter(NORTH, dataset_ID == "24757")

NORTH <- rbind(NORTH1, NORTH2,
                        NORTH3,
                        NORTH4,
                        NORTH5,
                        NORTH6,
                        NORTH7,
                        NORTH8,
                        NORTH9,
                        NORTH10,
                        NORTH11,
                        NORTH12,
                        NORTH13,
                        NORTH14,
                        NORTH15,
                        NORTH16,
                        NORTH17,
                        NORTH18,
                        NORTH19,
                        NORTH20)

# Save
write.csv(NORTH, file = "NORTH.csv", row.names = FALSE)
NORTH <- read.csv("NORTH.csv")
