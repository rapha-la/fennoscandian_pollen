### variation partitioning ###

#load package
library(vegan)
library(matrixStats)

#temp-data
tempN <- as.matrix(clim15NP_int[c("t1.1", "t4", "t10")])
meantempN <- rowMeans(tempN, na.rm = TRUE)
devN <- rowSds(tempN, na.rm = TRUE) #meantemp is higher, so we take meantemp and not median

tempSE <- as.matrix(clim911NP_int[c("t1", "t2")])
meantempSE <- rowMeans(tempSE, na.rm = TRUE)
devSE <- rowSds(tempSE, na.rm = TRUE) #meantemp is higher, so we take meantemp and not median

tempMW <- as.matrix(clim2NP_int[c("t1")]) #only one temperature dataset

tempMM <- as.matrix(clim47NP_int[c("t2.1")]) #only one temperature dataset

tempSW <- as.matrix(clim1NP_int[c("t1")]) #only one temperature dataset

tempSM <- as.matrix(clim36_int[c("t1", "t2", "t3", "t4", "t5", "t6")])
meantempSM <- rowMeans(tempSM, na.rm = TRUE)
devSM <- rowSds(tempSM, na.rm = TRUE) #meantemp is higher, so we take meantemp and not median



#North
#alldataN <- data.frame(yearsBP=spdN$calBP[1:81], SPD=spdN$SPD_med[1:81], clim=meantempN[16:96], conN=conN[16:96],
#                       decN=decN[16:96], wetwN=wetwN[16:96],wetmN=wetmN[16:96],pasN=pasN[16:96],araN=araN[16:96],heaN=heaN[16:96])
#write.csv(alldataN, file = "alldataN.csv", row.names = FALSE)
alldataN = read.csv("alldataN.csv")

test <- varpart(alldataN$conN, ~SPD, ~clim, data=alldataN)
test
plot(test, digits=2, Xnames=c("SPD", "clim"))

#SouthMid
#alldataSM <- data.frame(yearsBP=spdSM$calBP[1:84], SPD=spdSM$SPD_med[1:84], clim=meantempSM[16:99], conSM=conSM[16:99],
#                       decSM=decSM[16:99], wetwSM=wetwSM[16:99],wetmSM=wetmSM[16:99],pasSM=pasSM[16:99],araSM=araSM[16:99],heaSM=heaSM[16:99])
#write.csv(alldataSM, file = "alldataSM.csv", row.names = FALSE)
alldataSM = read.csv("alldataSM.csv")

test <- varpart(alldataSM$pasSM, ~SPD, ~clim, data=alldataSM)
test
plot(test, digits=2, Xnames=c("SPD", "clim"))

