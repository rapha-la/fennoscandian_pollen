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

formula5 <- select(VAR_data,-contains(c("7", "6")))
formula5 <- paste(colnames(formula5)[-1], collapse = " + ")
formula5 <- formula(paste0(varnames[1], " ~ ", formula5))

formula4 <- select(VAR_data,-contains(c("7", "6", "5")))
formula4 <- paste(colnames(formula4)[-1], collapse = " + ")
formula4 <- formula(paste0(varnames[1], " ~ ", formula4))

formula3 <- select(VAR_data,-contains(c("7", "6", "5", "4")))
formula3 <- paste(colnames(formula3)[-1], collapse = " + ")
formula3 <- formula(paste0(varnames[1], " ~ ", formula3))

formula2 <- select(VAR_data,-contains(c("7", "6", "5", "4", "3")))
formula2 <- paste(colnames(formula2)[-1], collapse = " + ")
formula2 <- formula(paste0(varnames[1], " ~ ", formula2))

formula1 <- select(VAR_data,-contains(c("7", "6", "5", "4", "3", "2")))
formula1 <- paste(colnames(formula1)[-1], collapse = " + ")
formula1 <- formula(paste0(varnames[1], " ~ ", formula1))

model7  <- map(dataconN_cv$train, ~glm2(formula7, data = VAR_data))
model6  <- map(dataconN_cv$train, ~glm2(formula6, data = VAR_data))
model5  <- map(dataconN_cv$train, ~glm2(formula5, data = VAR_data))
model4  <- map(dataconN_cv$train, ~glm2(formula4, data = VAR_data))
model3  <- map(dataconN_cv$train, ~glm2(formula3, data = VAR_data))
model2  <- map(dataconN_cv$train, ~glm2(formula2, data = VAR_data))
model1  <- map(dataconN_cv$train, ~glm2(formula1, data = VAR_data))

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}
pred7  <- map2_df(model7, dataconN_cv$test, get_pred, .id = "Run")
pred6  <- map2_df(model6, dataconN_cv$test, get_pred, .id = "Run")
pred5  <- map2_df(model5, dataconN_cv$test, get_pred, .id = "Run")
pred4  <- map2_df(model4, dataconN_cv$test, get_pred, .id = "Run")
pred3  <- map2_df(model3, dataconN_cv$test, get_pred, .id = "Run")
pred2  <- map2_df(model2, dataconN_cv$test, get_pred, .id = "Run")
pred1  <- map2_df(model1, dataconN_cv$test, get_pred, .id = "Run")

MSE7  <- pred7 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE6  <- pred6 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE5  <- pred5 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE4  <- pred4 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE3  <- pred3 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE2  <- pred2 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

MSE1  <- pred1 %>% group_by(Run) %>%
  summarise(MSE = mean( (LCCadapt - pred)^2))

mean(MSE1$MSE)
mean(MSE2$MSE)
mean(MSE3$MSE)
mean(MSE4$MSE)
mean(MSE5$MSE)
mean(MSE6$MSE)
mean(MSE7$MSE)
