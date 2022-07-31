### commonality analysis function ###

#vectors
SPD.N <- alldataNs$SPD
clim.N <- alldataNs$clim
conNs <- alldataNs$conNs
decNs <- alldataNs$decNs
wetwNs <- alldataNs$wetwNs
wetmNs <- alldataNs$wetmNs
pasNs <- alldataNs$pasNs
araNs <- alldataNs$araNs
heaNs <- alldataNs$heaNs

SPD.SE <- alldataSEs$SPD
clim.SE <- alldataSEs$clim
conSEs <- alldataSEs$conSEs
decSEs <- alldataSEs$decSEs
wetwSEs <- alldataSEs$wetwSEs
wetmSEs <- alldataSEs$wetmSEs
pasSEs <- alldataSEs$pasSEs
araSEs <- alldataSEs$araSEs
heaSEs <- alldataSEs$heaSEs

SPD.MW <- alldataMWs$SPD
clim.MW <- alldataMWs$clim
conMWs <- alldataMWs$conMWs
decMWs <- alldataMWs$decMWs
wetwMWs <- alldataMWs$wetwMWs
wetmMws <- alldataMWs$wetmMWs
pasMWs <- alldataMWs$pasMWs
araMWs <- alldataMWs$araMWs
heaMWs <- alldataMWs$heaMWs

SPD.MM <- alldataMMs$SPD
clim.MM <- alldataMM$clim
conMMs <- alldataMMs$conMMs
decMMs <- alldataMMs$decMMs
wetwMMs <- alldataMMs$wetwMMs
wetmMMs <- alldataMMs$wetmMMs
pasMMs <- alldataMMs$pasMMs
araMMs <- alldataMMs$araMMs
heaMMs <- alldataMMs$heaMMs

SPD.SW <- alldataSWs$SPD
clim.SW <- alldataSWs$clim
conSWs <- alldataSWs$conSWs
decSWs <- alldataSWs$decSWs
wetwSWs <- alldataSWs$wetwSWs
wetmSWs <- alldataSWs$wetmSWs
pasSWs <- alldataSWs$pasSWs
araSWs <- alldataSWs$araSWs
heaSWs <- alldataSWs$heaSWs

SPD.SM <- alldataSMs$SPD
clim.SM <- alldataSMs$clim
conSMs <- alldataSMs$conSMs
decSMs <- alldataSMs$decSMs
wetwSMs <- alldataSMs$wetwSMs
wetmSMs <- alldataSMs$wetmSMs
pasSMs <- alldataSMs$pasSMs
araSMs <- alldataSMs$araSMs
heaSMs <- alldataSMs$heaSMs

#function
custom_commonality_analysis <- function(LCC, SPD, clim, p) {
  #commonality analysis
  varnames <- c(deparse(substitute(LCC)), deparse(substitute(SPD)), deparse(substitute(clim)))
  lagnames <- paste0(varnames, "_lag", rep(1:p, each = 3))
  # Then we created the lagged variables / data for models
  VAR_data <- embed(as.matrix(cbind(LCC, SPD, clim)), p + 1)
  colnames(VAR_data) <- c(varnames, lagnames)
  VAR_data <- VAR_data[, -c(2,3)]
  # the full model with LCC, SPD and clim
  model_formula <- paste(colnames(VAR_data)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  full_mod <- lm(model_formula, data = as.data.frame(VAR_data))
  # LCC and clim
  VAR_data_c <- VAR_data[,c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22)]
  model_formula <- paste(colnames(VAR_data_c)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  clim_mod <- lm(model_formula, data = as.data.frame(VAR_data_c))
  # LCC and SPD
  VAR_data_s <- VAR_data[,c(1,2,3,5,6,8,9,11,12,14,15,17,18,20,21)]
  model_formula <- paste(colnames(VAR_data_s)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  SPD_mod <- lm(model_formula, data = as.data.frame(VAR_data_s))
  # only LCC
  VAR_data_l <- VAR_data[,c(1,2,5,8,11,14,17,20)]
  model_formula <- paste(colnames(VAR_data_l)[-1], collapse = " + ")
  model_formula <- formula(paste0(varnames[1], " ~ ", model_formula))
  LCC_mod <- lm(model_formula, data = as.data.frame(VAR_data_l))
  # Granger test
  granger_test_f <- anova(LCC_mod, full_mod)
  granger_test_s <- anova(LCC_mod, SPD_mod)
  granger_test_c <- anova(LCC_mod, clim_mod)
  # getting R squared values
  R2_lsc = (granger_test_f$RSS[1] - granger_test_f$RSS[2]) / granger_test_f$RSS[1]
  R2_ls = (granger_test_s$RSS[1] - granger_test_s$RSS[2]) / granger_test_s$RSS[1]
  R2_lc = (granger_test_c$RSS[1] - granger_test_c$RSS[2]) / granger_test_c$RSS[1]
  # commonality analysis
  Us <- R2_lsc - R2_lc
  Uc <- R2_lsc - R2_ls
  Csc <- R2_lsc - Us - Uc
  # Return the results
  return(list(R2_lsc =R2_lsc,
              R2_ls =R2_ls,
              R2_lc =R2_lc,
              Us=Us,
              Uc=Uc,
              Csc=Csc))
}

### commonality analysis ###
test <- custom_commonality_analysis(pasMWs, SPD.MW, clim.MW, 7)

