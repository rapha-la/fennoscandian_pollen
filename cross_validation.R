### cross validation ###

library(modelr)

varnames <- c(deparse(substitute(LCCadapt)), deparse(substitute(SPD)), deparse(substitute(clim)))
lagnames <- paste0(varnames, "_lag", rep(1:7, each = 3))
# Then we created the lagged variables / data for models
VAR_data <- embed(as.matrix(cbind(conNs, SPD.N, clim.N)), 7 + 1)
colnames(VAR_data) <- c(varnames, lagnames)
VAR_data <- as.data.frame(VAR_data[, -c(2,3)])

dataconN_cv  <- crossv_kfold(VAR_data, k = 5)

# the full model with LCC, SPD and clim
formula7 <- paste(colnames(VAR_data)[-1], collapse = " + ")
formula7 <- formula(paste0(varnames[1], " ~ ", formula7))

formula6 <- select(VAR_data,-contains("7"))
formula6 <- paste(colnames(formula6)[-1], collapse = " + ")
formula6 <- formula(paste0(varnames[1], " ~ ", formula6))

formula1 <- select(VAR_data,-contains(c("7", "6", "5", "4", "3", "2")))
formula1 <- paste(colnames(formula1)[-1], collapse = " + ")
formula1 <- formula(paste0(varnames[1], " ~ ", formula1))

models1  <- map(dataconN_cv$train, ~lm(formula7, data = VAR_data))
models2  <- map(dataconN_cv$train, ~lm(formula6, data = VAR_data))
models3  <- map(dataconN_cv$train, ~lm(formula1, data = VAR_data))

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}
pred1  <- map2_df(models1, dataconN_cv$test, get_pred, .id = "Run")
pred2  <- map2_df(models2, dataconN_cv$test, get_pred, .id = "Run")
pred3  <- map2_df(models3, dataconN_cv$test, get_pred, .id = "Run")

MSE1  <- pred1 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE2  <- pred2 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE3  <- pred3 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

mean(MSE1$MSE)
mean(MSE2$MSE)
mean(MSE3$MSE)
