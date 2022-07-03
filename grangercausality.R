### Granger causality test ###

install.packages("bruceR")
install.packages("vars")
install.packages("lmtest")
install.packages("SCGLR")
library(vars)
library(bruceR)
library(glm2)
library(lmtest)
library(SCGLR)


#North
#-------------------------------------------------------------------------------
#alldataN <- data.frame(yearsBP=spdN$calBP[1:81], SPD=spdN$SPD_med[1:81], clim=paleoviewN$Area.Mean[16:96], conN=conN[16:96],
#                       decN=decN[16:96], wetwN=wetwN[16:96],wetmN=wetmN[16:96],pasN=pasN[16:96],araN=araN[16:96],heaN=heaN[16:96])
#alldataN = alldataN[1:79,]
#alldataN = arrange(alldataN, -row_number())
#write.csv(alldataN, file = "alldataN.csv", row.names = FALSE)
alldataN = read.csv("alldataN.csv")


#granger.tableN1 <- data.frame(LCC1=conN[1:99], LCC2=conN[2:100], LCC3=conN[3:101], resp=conN[4:102])
#glm2(resp ~ c(LCC1,LCC2,LCC3), poisson, granger.tableN1) #doesn't work like this

# univariate causality granger test from the lmtest package.
grangertest(conN ~ clim, order=3, data=alldataN)

# multivariate granger causality test from the bruceR package.
vm1 <- VAR(alldataN[,2:4]) #no lag
vm2 <- VAR(alldataN[,2:4], p=3) #lag of 3 time bins
granger_causality(varmodel = vm1, var.y = "conN", var.x = c("SPD", "clim")) #choose which vm you want.

# attempt to make a multivariate granger causality test with the multivariateGlm.fit function from the 
# SCGLR package and manually constructed matrices.
predictorsN <- data.frame(LCC1=conN[1:99], LCC2=conN[2:100], LCC3=conN[3:101])
predictorsN <- data.matrix(predictorsN)
responseN <- data.frame(resp=conN[4:102])
responseN <- data.matrix(responseN)
fam <- c("poisson")
offset <- 
size <- 
multivariateGlm.fit(responseN, predictorsN, fam, )

