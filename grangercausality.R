### Granger causality test ###

library(vars)
library(bruceR)
library(glm2)
library(lmtest)
library(stats)


# multivariate granger causality test from the bruceR package.------------------

### NORTH ###
#-------------------------------------------------------------------------------
#alldataN <- data.frame(yearsBP=spdN$calBP, SPD=spdN$SPD_med, clim=paleoviewN$Area.Mean[1:84], conN=conN[1:84],
#                       decN=decN[1:84], wetwN=wetwN[1:84],wetmN=wetmN[1:84],pasN=pasN[1:84],araN=araN[1:84],heaN=heaN[1:84])
#write.csv(alldataN, file = "alldataN.csv", row.names = FALSE)
alldataN = read.csv("alldataN.csv")

# model selection according to the first value after the comma? --> 3
# select according to the full value. --> 1
# selecting according to the absolut lowest. --> 5
VARselect(alldataN$conN, lag.max=7, type="none") #Inclusion of exogenous variables?
vm <- VAR(alldataN[,c(2,3,4)], p=5) 
granger_causality(varmodel = vm, var.y = "conN", var.x = c("SPD", "clim")) #choose which vm you want. #SIG

VARselect(alldataNs$decN, lag.max=7, type="none")
vm <- VAR(alldataN[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decN", var.x = c("SPD", "clim"))

VARselect(wetwN, lag.max=7, type="none")
vm <- VAR(alldataN[,c(2,3,6)], p=5)
granger_causality(varmodel = vm, var.y = "wetwN", var.x = c("SPD", "clim"))

VARselect(wetmN, lag.max=7, type="none")
vm <- VAR(alldataN[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmN", var.x = c("SPD", "clim")) # SIG

VARselect(pasN, lag.max=7, type="none")
vm <- VAR(alldataN[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasN", var.x = c("SPD", "clim"))

VARselect(araN, lag.max=7, type="none")
vm <- VAR(alldataN[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araN", var.x = c("SPD", "clim"))

VARselect(heaN, lag.max=7, type="none")
vm <- VAR(alldataN[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaN", var.x = c("SPD", "clim"))

### SOUTHEAST ###
#-------------------------------------------------------------------------------
#alldataSE <- data.frame(yearsBP=spdSE$calBP, SPD=spdSE$SPD_med, clim=paleoviewSE$Area.Mean[1:93], conSE=conSE[1:93],
#                        decSE=decSE[1:93], wetwSE=wetwSE[1:93],wetmSE=wetmSE[1:93],pasSE=pasSE[1:93],araSE=araSE[1:93],heaSE=heaSE[1:93])
#write.csv(alldataSE, file = "alldataSE.csv", row.names = FALSE)
alldataSE = read.csv("alldataSE.csv")

VARselect(conSE, lag.max=10, type="none")
vm <- VAR(alldataSE[,c(2,3,4)], p=5)
granger_causality(varmodel = vm, var.y = "conSE", var.x = c("SPD", "clim"))

VARselect(decSE, lag.max=7, type="none")
vm <- VAR(alldataSE[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decSE", var.x = c("SPD", "clim"))

VARselect(wetwSE, lag.max=7, type="none")
vm <- VAR(alldataSE[,c(2,3,6)], p=5)
granger_causality(varmodel = vm, var.y = "wetwSE", var.x = c("SPD", "clim")) #SIG

VARselect(wetmSE, lag.max=7, type="none")
vm <- VAR(alldataSE[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmSE", var.x = c("SPD", "clim"))

VARselect(pasSE, lag.max=7, type="none")
vm <- VAR(alldataSE[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasSE", var.x = c("SPD", "clim")) #SIG

VARselect(araSE, lag.max=7, type="none")
vm <- VAR(alldataSE[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araSE", var.x = c("SPD", "clim"))

VARselect(heaSE, lag.max=7, type="none")
vm <- VAR(alldataSE[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaSE", var.x = c("SPD", "clim")) #SIG


### MIDWEST ###
#-------------------------------------------------------------------------------
#alldataMW <- data.frame(yearsBP=spdMW$calBP, SPD=spdMW$SPD_med, clim=paleoviewMW$Area.Mean[1:91], conMW=conMW[1:91],
#                        decMW=decMW[1:91], wetwMW=wetwMW[1:91],wetmMW=wetmMW[1:91],pasMW=pasMW[1:91],araMW=araMW[1:91],heaMW=heaMW[1:91])
#write.csv(alldataMW, file = "alldataMW.csv", row.names = FALSE)
alldataMW = read.csv("alldataMW.csv")

VARselect(conMW, lag.max=7, type="none")
vm <- VAR(alldataMW[,c(2,3,4)], p=5)
granger_causality(varmodel = vm, var.y = "conMW", var.x = c("SPD", "clim"))

VARselect(decMW, lag.max=7, type="none")
vm <- VAR(alldataMW[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decMW", var.x = c("SPD", "clim")) #SIG

VARselect(wetwMW, lag.max=7, type="none")
vm <- VAR(alldataMW[,c(2,3,6)], p=5)
granger_causality(varmodel = vm, var.y = "wetwMW", var.x = c("SPD", "clim"))

VARselect(wetmMW, lag.max=7, type="none")
vm <- VAR(alldataMW[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmMW", var.x = c("SPD", "clim")) #SIG

VARselect(pasMW, lag.max=7, type="none")
vm <- VAR(alldataMW[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasMW", var.x = c("SPD", "clim")) #SIG

VARselect(araMW, lag.max=7, type="none")
vm <- VAR(alldataMW[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araMW", var.x = c("SPD", "clim"))

VARselect(heaMW, lag.max=7, type="none")
vm <- VAR(alldataMW[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaMW", var.x = c("SPD", "clim")) #SIG


### MIDMID ###
#-------------------------------------------------------------------------------
#alldataMM <- data.frame(yearsBP=spdMM$calBP, SPD=spdMM$SPD_med, clim=paleoviewMM$Area.Mean[1:91], conMM=conMM[1:91],
#                       decMM=decMM[1:91], wetwMM=wetwMM[1:91],wetmMM=wetmMM[1:91],pasMM=pasMM[1:91],araMM=araMM[1:91],heaMM=heaMM[1:91])
#write.csv(alldataMM, file = "alldataMM.csv", row.names = FALSE)
alldataMM = read.csv("alldataMM.csv")

VARselect(conMM, lag.max=7, type="none")
vm <- VAR(alldataMM[,c(2,3,4)], p=5)
granger_causality(varmodel = vm, var.y = "conMM", var.x = c("SPD", "clim"))

VARselect(decMM, lag.max=7, type="none")
vm <- VAR(alldataMM[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decMM", var.x = c("SPD", "clim")) #SIG

VARselect(wetwMM, lag.max=7, type="none")
vm <- VAR(alldataMM[,c(2,3,6)], p=5)
granger_causality(varmodel = vm, var.y = "wetwMM", var.x = c("SPD", "clim")) #SIG

VARselect(wetmMM, lag.max=7, type="none")
vm <- VAR(alldataMM[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmMM", var.x = c("SPD", "clim")) #SIG

VARselect(pasMM, lag.max=7, type="none")
vm <- VAR(alldataMM[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasMM", var.x = c("SPD", "clim")) #SIG

VARselect(araMM, lag.max=7, type="none")
vm <- VAR(alldataMM[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araMM", var.x = c("SPD", "clim")) #SIG

VARselect(heaMM, lag.max=7, type="none")
vm <- VAR(alldataMM[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaMM", var.x = c("SPD", "clim")) #SIG


### SOUTHWEST ###
#-------------------------------------------------------------------------------
#alldataSW <- data.frame(yearsBP=spdSW$calBP, SPD=spdSW$SPD_med, clim=paleoviewSW$Area.Mean[1:91], conSW=conSW[1:91],
#                        decSW=decSW[1:91], wetwSW=wetwSW[1:91],wetmSW=wetmSW[1:91],pasSW=pasSW[1:91],araSW=araSW[1:91],heaSW=heaSW[1:91])
#write.csv(alldataSW, file = "alldataSW.csv", row.names = FALSE)
alldataSW = read.csv("alldataSW.csv")

VARselect(conSW, lag.max=7, type="none")
vm <- VAR(alldataSW[,c(2,3,4)], p=5)
granger_causality(varmodel = vm, var.y = "conSW", var.x = c("SPD", "clim")) #SIG

VARselect(decSW, lag.max=7, type="none")
vm <- VAR(alldataSW[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decSW", var.x = c("SPD", "clim")) #SIG

VARselect(wetwSW, lag.max=7, type="none")
vm <- VAR(alldataSW[,c(2,3,6)], p=5)
granger_causality(varmodel = vm, var.y = "wetwSW", var.x = c("SPD", "clim"))

VARselect(wetmSW, lag.max=7, type="none")
vm <- VAR(alldataSW[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmSW", var.x = c("SPD", "clim")) #SIG

VARselect(pasSW, lag.max=7, type="none")
vm <- VAR(alldataSW[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasSW", var.x = c("SPD", "clim")) #SIG

VARselect(araSW, lag.max=7, type="none")
vm <- VAR(alldataSW[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araSW", var.x = c("SPD", "clim")) #SIG

VARselect(heaSW, lag.max=7, type="none")
vm <- VAR(alldataSW[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaSW", var.x = c("SPD", "clim")) #SIG


### SOUTHMID ###
#-------------------------------------------------------------------------------
#alldataSM <- data.frame(yearsBP=spdSM$calBP, SPD=spdSM$SPD_med, clim=paleoviewSM$Area.Mean[1:87], conSM=conSM[1:87],
#                        decSM=decSM[1:87], wetwSM=wetwSM[1:87],wetmSM=wetmSM[1:87],pasSM=pasSM[1:87],araSM=araSM[1:87],heaSM=heaSM[1:87])
#write.csv(alldataSM, file = "alldataSM.csv", row.names = FALSE)
alldataSM = read.csv("alldataSM.csv")

VARselect(conSM, lag.max=7, type="none")
vm <- VAR(alldataSM[,c(2,3,4)], p=5)
granger_causality(varmodel = vm, var.y = "conSM", var.x = c("SPD", "clim")) #SIG

VARselect(decSM, lag.max=7, type="none")
vm <- VAR(alldataSM[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decSM", var.x = c("SPD", "clim")) #SIG

VARselect(wetwSM, lag.max=7, type="none")
vm <- VAR(alldataSM[,c(2,3,6)], p=5)
granger_causality(varmodel = vm, var.y = "wetwSM", var.x = c("SPD", "clim"))

VARselect(wetmSM, lag.max=7, type="none")
vm <- VAR(alldataSM[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmSM", var.x = c("SPD", "clim")) #SIG

VARselect(pasSM, lag.max=7, type="none")
vm <- VAR(alldataSM[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasSM", var.x = c("SPD", "clim")) #SIG

VARselect(araSM, lag.max=7, type="none")
vm <- VAR(alldataSM[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araSM", var.x = c("SPD", "clim")) #SIG

VARselect(heaSM, lag.max=7, type="none")
vm <- VAR(alldataSM[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaSM", var.x = c("SPD", "clim")) #SIG
