### THE ECOLOGICAL GROUPS ###

library(ecp)
library(ggplot2)

#run ecp.R first.

# plot-function
plot_ecp_site = function(SiteNR, genus1, genus2, genus3){ #the genera need to be written in quotation marks
  big = bigdf_genera[bigdf_genera$dataset_ID==SiteNR,]
  big = big[order(big$meantimes),]
  ecp = ecp_divisive_for_site(SiteNR)
  df_for_plotting = data.frame(meantimes = big$meantimes,c1 = big[[genus1]],c2 = big[[genus2]], c3 = big[[genus3]])
  ggplot(df_for_plotting, aes(x = meantimes)) + 
    scale_colour_manual(values=c(col1="red2",col2="blue2",col3="forestgreen",changepoint="black"),labels=c(genus1,genus2,genus3,"changepoint")) +
    geom_line(aes(y = c1,colour="col1")) + 
    geom_line(aes(y = c2,colour="col2")) +
    geom_line(aes(y = c3,colour="col3")) +
    geom_vline(aes(xintercept = big$meantimes[ecp$estimates[2]], colour = "changepoint")) +
    geom_vline(aes(xintercept = big$meantimes[ecp$estimates[3]], colour = "changepoint")) +
    geom_vline(aes(xintercept = big$meantimes[ecp$estimates[4]], colour = "changepoint")) +
    ggtitle(SiteNR) +
    labs(x = "Year BP",y = "Count Nr",colour = "Legend")
}

#eco_rel
plot_ecp_eco_rel = function(groupdf, ecpdf, title, genus1, genus2, genus3){ #the genera need to be written in quotation marks
  groupdf = data.frame(meantimes = groupdf$meantimes,c1 = groupdf[[genus1]],c2 = groupdf[[genus2]], c3 = groupdf[[genus3]])
  ggplot(groupdf, aes(x = meantimes)) + 
    scale_colour_manual(values=c(col1="red2",col2="blue2",col3="forestgreen",changepoint="black"),labels=c(genus1,genus2,genus3,"changepoint")) +
    geom_line(aes(y = c1,colour="col1")) + 
    geom_line(aes(y = c2,colour="col2")) +
    geom_line(aes(y = c3,colour="col3")) +
    geom_vline(aes(xintercept = ecpdf$point1, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecpdf$point2, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecpdf$point3, colour = "changepoint")) +
    ggtitle(title) +
    labs(x = "Year BP",y = "Count Nr",colour = "Legend")
}

#groups_rel
plot_ecp_groups_rel = function(groupdf, ecpdf, title){ #the genera need to be written in quotation marks
  groupdf = data.frame(meantimes = groupdf$meantimes,c1 = groupdf$CLTV,c2 = groupdf$AQVP, c3 = groupdf$TRSH, c4 = groupdf$UPHE, c5 = groupdf$VACR)
  ggplot(groupdf, aes(x = meantimes)) + 
    scale_colour_manual(values=c(col1="red2",col2="blue2",col3="forestgreen", col4="magenta3", col5="orange",changepoint="black"),labels=c("CLTV","AQVP","TRSH","UPHE", "VACR", "changepoint")) +
    geom_line(aes(y = c1,colour="col1")) + 
    geom_line(aes(y = c2,colour="col2")) +
    geom_line(aes(y = c3,colour="col3")) +
    geom_line(aes(y = c4,colour="col4")) +
    geom_line(aes(y = c5,colour="col5")) +
    geom_vline(aes(xintercept = ecpdf$point1, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecpdf$point2, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecpdf$point3, colour = "changepoint")) +
    ggtitle(title) +
    labs(x = "Year BP",y = "Count Nr",colour = "Legend")
}

#groups_rel (without TRSH)
plot_ecp_groups_4 = function(groupdf, ecpdf, title){ #the genera need to be written in quotation marks
  groupdf = data.frame(meantimes = groupdf$meantimes,c1 = groupdf$CLTV,c2 = groupdf$AQVP, c4 = groupdf$UPHE, c5 = groupdf$VACR)
  ggplot(groupdf, aes(x = meantimes)) + 
    scale_colour_manual(values=c(col1="red2",col2="blue2", col4="magenta3", col5="orange",changepoint="black"),labels=c("CLTV","AQVP","UPHE", "VACR", "changepoint")) +
    geom_line(aes(y = c1,colour="col1")) + 
    geom_line(aes(y = c2,colour="col2")) +
    geom_line(aes(y = c4,colour="col4")) +
    geom_line(aes(y = c5,colour="col5")) +
    geom_vline(aes(xintercept = ecpdf$point1, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecpdf$point2, colour = "changepoint")) +
    geom_vline(aes(xintercept = ecpdf$point3, colour = "changepoint")) +
    ggtitle(title) +
    labs(x = "Year BP",y = "Count Nr",colour = "Legend")
}



#ECO.GROUPS
#500intervals
eco_groups500_rel <- read.csv("eco_groups500_rel.csv")
plot_ecp_groups_rel(eco_groups500_rel, ecp_eco_groups100_rel, "eco_groups500_rel")
plot_ecp_groups_4(eco_groups500_rel, ecp_noTRSH100, "noTRSH500_rel")

#100intervals rel
eco_groups100_rel <- read.csv("eco_groups100_rel.csv")
plot_ecp_groups_rel(eco_groups100_rel, ecp_eco_groups100_rel, "eco_groups100_rel")
plot_ecp_groups_4(eco_groups100_rel, ecp_noTRSH100_rel, "noTRSH_100_rel")

#100intervals count
eco_groups_intervals100 <- read.csv("eco_groups_intervals100.csv")
eco_groups_intervals100 <- subset(eco_groups_intervals100, select = -c(time.intervals))
plot_ecp_groups_rel(eco_groups_intervals100, ecp_eco_groups_intervals100, "eco_groups_intervals100")
plot_ecp_groups_4(eco_groups_intervals100, ecp_eco_groups_intervals100, "eco_groups_intervals100")

#500 count
eco_groups_interval500 <- read.csv("eco_groups_interval500.csv")
eco_groups_interval500 <- subset(eco_groups_interval500, select = -c(time.intervals))
plot_ecp_groups_rel(eco_groups_interval500, ecp_eco_groups_intervals100, "eco_groups_interval500")
plot_ecp_groups_4(eco_groups_interval500, ecp_eco_groups_intervals100, "eco_groups_interval500")


#NORTH
#groups.500intervals
North.groups500_rel <- read.csv("North.groups500_rel.csv")
plot_ecp_groups_rel(North.groups500_rel, ecp_North.groups100_rel, "North.groups500_rel")
plot_ecp_groups_4(North.groups500_rel, ecp_North.groups100_rel, "North.groups500_rel")

#SOUTH
#groups.500intervals
South.groups500_rel <- read.csv("South.groups500_rel.csv")
plot_ecp_groups_rel(South.groups500_rel, ecp_South.groups100_rel, "South.groups500_rel")
plot_ecp_groups_4(South.groups500_rel, ecp_South.groups100_rel, "South.groups500_rel")



#FENNOSCANDIA
#500yr steps intervals
Fennoscandia500_rel <- read.csv("Fennoscandia500_rel.csv")
plot_ecp_rel(Fennoscandia500_rel, "Fennoscandia500_rel", ...)

#AQVP
#500intervals
AQVP500_rel <- read.csv("AQVP500_rel.csv")
plot_ecp_eco_rel(AQVP500_rel, ecp_AQVP100_rel, "AQVP500_rel", "Callitriche", "Drosera", "Eriophorum")

#TRSH
#500intervals
TRSH500_rel <- read.csv("TRSH500_rel.csv")

#UPHE
#500intervals
UPHE500_rel <- read.csv("UPHE500_rel.csv")

#VACR
#500intervals
VACR500_rel <- read.csv("VACR500_rel.csv")

#CLTV
#500intervals
CLTV500_rel <- read.csv("CLTV500_rel.csv")

