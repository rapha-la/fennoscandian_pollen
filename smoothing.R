### smoothing attempts ###

install.packages("quantmod")
install.packages("smooth")
install.packages("lubridate")
install.packages("fpp2")
install.packages("TTR")

#library(RRatepol)
library(ggplot2)
library(tidyverse)      # data manipulation and visualization
library(lubridate)      # easily work with dates and times
library(fpp2)           # working with time series data
library(zoo) 
library(TTR)


#smoothing HOW TO MAKE MODEL SELECTION?
sma(conN, ic="AIC", h=0, interval="none") #3
sma(decN, ic="AIC", h=0, interval="none") #7
sma(wetwN, ic="AIC", h=0, interval="none") #3
sma(wetmN, ic="AIC", h=0, interval="none") #10
sma(pasN, ic="AIC", h=0, interval="none") #7
sma(araN, ic="AIC", h=0, interval="none") #7
sma(heaN, ic="AIC", h=0, interval="none") #7

alldataN$conNs <- SMA(alldataN$conN, n=7)
alldataN$decNs <- SMA(alldataN$decN, n=7)
alldataN$wetwNs <- SMA(alldataN$wetwN, n=7)
alldataN$wetmNs <- SMA(alldataN$wetmN, n=7)
alldataN$pasNs <- SMA(alldataN$pasN, n=7)
alldataN$araNs <- SMA(alldataN$araN, n=7)
alldataN$heaNs <- SMA(alldataN$heaN, n=7)
alldataNs <- alldataN[7:nrow(alldataN),]

VARselect(alldataNs$conNs, lag.max=7, type="none") #Inclusion of exogenous variables?
vm3 <- VAR(alldataNs[,2:4], p=3) #4
granger_causality(varmodel = vm3, var.y = "conNs", var.x = c("SPD", "clim")) #choose which vm you want.

VARselect(alldataNs$decNs, lag.max=7, type="none")
vm4 <- VAR(alldataNs[,c(2,3,5)], p=4) #2
granger_causality(varmodel = vm4, var.y = "decN", var.x = c("SPD", "clim"))

sma(conSE, ic="AIC", h=0, interval="none") #3
sma(decSE, ic="AIC", h=0, interval="none") #4
sma(wetwSE, ic="AIC", h=0, interval="none") #12
sma(wetmSE, ic="AIC", h=0, interval="none") #9
sma(pasSE, ic="AIC", h=0, interval="none") #2
sma(araSE, ic="AIC", h=0, interval="none") #1
sma(heaSE, ic="AIC", h=0, interval="none") #7

sma(conMW, ic="AIC", h=0, interval="none") #101
sma(decMW, ic="AIC", h=0, interval="none") #4
sma(wetwMW, ic="AIC", h=0, interval="none") #12
sma(wetmMW, ic="AIC", h=0, interval="none") #9
sma(pasMW, ic="AIC", h=0, interval="none") #2
sma(araMW, ic="AIC", h=0, interval="none") #1
sma(heaMW, ic="AIC", h=0, interval="none") #7