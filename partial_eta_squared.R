
# code from stack overflow #

nested_model_partial_eta_sq <- function(full_mod, rest_mod) {
  # full_mod is the full model (an lm object)
  # rest_mod is the model that omits one or more explanatory variables
  SS_full <- sum(full_mod$residuals^2)
  SS_rest <- sum(rest_mod$residuals^2)
  return((SS_rest - SS_full) / SS_rest)
}

custom_granger <- function(x, y, p = 1) {
  # Does x Granger cause y? What is the effect size?
  # We want to test Granger causality,
  # but then also get partial eta squared to measure effect size
  # First it will be convenient to store the variable names
  varnames <- c(deparse(substitute(x)), deparse(substitute(y)))
  lagnames <- paste0(varnames, "_lag", rep(1:p, each = 2))
  # Then we created the lagged variables / data for models
  VAR_data <- embed(as.matrix(cbind(x, y)), p + 1)
  colnames(VAR_data) <- c(varnames, lagnames)
  VAR_data <- VAR_data[, -1]
  # Run the full model
  model_formula <- paste(colnames(VAR_data)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[2], " ~ ", model_formula))
  full_mod <- lm(model_formula, data = as.data.frame(VAR_data))
  # Take out the lags of x and run the nested model
  VAR_data <- VAR_data[ , seq(from = 1, to = p * 2 + 1, by = 2)]
  model_formula <- paste(colnames(VAR_data)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[2], " ~ ", model_formula))
  rest_mod <- lm(model_formula, data = as.data.frame(VAR_data))
  # Then we can do the Granger test
  granger_test <- anova(full_mod, rest_mod)
  # and get partial eta squared
  SS_full <- granger_test$RSS[1]
  SS_rest <- granger_test$RSS[2]
  partial_eta_squared <- (SS_rest - SS_full) / SS_rest
  # And return all of it
  return(list(VAR_result = full_mod,
              granger_test = granger_test,
              partial_eta_squared = partial_eta_squared))
}


### ALL LCCs THAT SHOWED SIGNIFICANCE ###--------------------------------------

#conN SPD
vm <- VAR(alldataN[,c(2,3,4)], p=5) 
granger_causality(varmodel = vm, var.y = "conN", var.x = c("SPD", "clim"))
conN <- alldataN$conN
SPD <- alldataN$SPD
custom_granger(SPD, conN, p=5) #np2=0.19

#wetmN both?
vm <- VAR(alldataN[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmN", var.x = c("SPD", "clim")) # SIG
conN <- alldataN$conN
SPD <- alldataN$SPD
custom_granger(SPD, conN, p=5)  #np2=

#araN SPD
vm <- VAR(alldataN[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araN", var.x = c("SPD", "clim"))
araN <- alldataN$araN
SPD <- alldataN$SPD
custom_granger(SPD, araN, p=5)  #np2=0.134

#conSE clim
vm <- VAR(alldataSE[,c(2,3,4)], p=5)
granger_causality(varmodel = vm, var.y = "conSE", var.x = c("SPD", "clim"))
conSE <- alldataSE$conSE
clim <- alldataSE$clim
custom_granger(clim, conSE, p=5)  #np2=0.123

#araSE SPD
vm <- VAR(alldataSE[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araSE", var.x = c("SPD", "clim"))
summary(vm) #np2=
araSE <- alldataSE$araSE
SPD <- alldataSE$SPD
custom_granger(SPD, araSE, p=5)  #np2=0.185

#pasMW SPD
vm <- VAR(alldataMW[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasMW", var.x = c("SPD", "clim")) #SIG
pasMW <- alldataMW$pasMW
SPD <- alldataMW$SPD
custom_granger(SPD, pasMW, p=5)  #np2=0.287

#wetwMM clim, SPD
vm <- VAR(alldataMM[,c(2,3,6)], p=5)
granger_causality(varmodel = vm, var.y = "wetwMM", var.x = c("SPD", "clim")) #SIG
wetwMM <- alldataMM$wetwMM
SPD <- alldataMM$SPD
clim <- alldataMM$clim
custom_granger(SPD, wetwMM, p=5)  #np2=0.151
custom_granger(clim, wetwMM, p=5)  #np2=0.215

#wetmMM SPD
vm <- VAR(alldataMM[,c(2,3,7)], p=5)
granger_causality(varmodel = vm, var.y = "wetmMM", var.x = c("SPD", "clim")) #SIG
wetmMM <- alldataMM$wetmMM
SPD <- alldataMM$SPD
custom_granger(SPD, wetmMM, p=5)  #np2=0.160

#pasMM SPD
vm <- VAR(alldataMM[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasMM", var.x = c("SPD", "clim")) #SIG
pasMM <- alldataMM$pasMM
SPD <- alldataMM$SPD
custom_granger(SPD, pasMM, p=5)  #np2=0.289

#araMM SPD
vm <- VAR(alldataMM[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araMM", var.x = c("SPD", "clim")) #SIG
araMM <- alldataMM$araMM
SPD <- alldataMM$SPD
custom_granger(SPD, araMM, p=5)  #np2=0.241

#heaMM SPD
vm <- VAR(alldataMM[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaMM", var.x = c("SPD", "clim")) #SIG
heaMM <- alldataMM$heaMM
SPD <- alldataMM$SPD
custom_granger(SPD, heaMM, p=5)  #np2=0.193

#decSW clim, SPD --> why so low results? how can this be significant?
vm <- VAR(alldataSW[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decSW", var.x = c("SPD", "clim")) #SIG
decSW <- alldataSW$decSW
SPD <- alldataSW$SPD
clim <- alldataSW$clim
custom_granger(SPD, decSW, p=5)  #np2=0.059
custom_granger(clim, decSW, p=5)  #np2=0.088

#araSW SPD
vm <- VAR(alldataSW[,c(2,3,9)], p=5)
granger_causality(varmodel = vm, var.y = "araSW", var.x = c("SPD", "clim")) #SIG
araSW <- alldataSW$araSW
SPD <- alldataSW$SPD
custom_granger(SPD, araSW, p=5)  #np2=0.391

#decSM SPD
vm <- VAR(alldataSM[,c(2,3,5)], p=5)
granger_causality(varmodel = vm, var.y = "decSM", var.x = c("SPD", "clim")) #SIG
decSM <- alldataSM$decSM
SPD <- alldataSM$SPD
custom_granger(SPD, decSM, p=5)  #np2=0.231

#pasSM clim, SPD
vm <- VAR(alldataSM[,c(2,3,8)], p=5)
granger_causality(varmodel = vm, var.y = "pasSM", var.x = c("SPD", "clim")) #SIG
pasSM <- alldataSM$pasSM
SPD <- alldataSM$SPD
clim <- alldataSM$clim
custom_granger(SPD, pasSM, p=5)  #np2=0.239
custom_granger(clim, pasSM, p=5)  #np2=0.231

#heaSM SPD
vm <- VAR(alldataSM[,c(2,3,10)], p=5)
granger_causality(varmodel = vm, var.y = "heaSM", var.x = c("SPD", "clim")) #SIG
heaSM <- alldataSM$heaSM
SPD <- alldataSM$SPD
custom_granger(SPD, heaSM, p=5)  #np2=0.120


### BEFORE THE ONSET OF FARMING #-----------------------------------------------

#North: 

#SouthEast: 

#MidWest:

#MidMid:

#SouthWest:

#SouthMid:


### AFTER THE ONSET OF FARMING #------------------------------------------------

#North: 

#SouthEast: 

#MidWest:

#MidMid:

#SouthWest:

#SouthMid:
