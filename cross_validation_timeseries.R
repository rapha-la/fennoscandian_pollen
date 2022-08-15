### cross validation for time series ###

library(kfoldcv4ts)

model1 = accuracy.kfold(df.VAR = alldataNs[,c(2,3,11)], k=5, n_ahead = 6, lags = 1, var_index = 3)
model2 = accuracy.kfold(df.VAR = alldataNs[,c(2,3,11)], k=5, n_ahead = 6, lags = 2, var_index = 3)
model3 = accuracy.kfold(df.VAR = alldataNs[,c(2,3,11)], k=5, n_ahead = 6, lags = 3, var_index = 3)
model4 = accuracy.kfold(df.VAR = alldataNs[,c(2,3,11)], k=5, n_ahead = 6, lags = 4, var_index = 3)
model5 = accuracy.kfold(df.VAR = alldataNs[,c(2,3,11)], k=5, n_ahead = 6, lags = 5, var_index = 3)
model6 = accuracy.kfold(df.VAR = alldataNs[,c(2,3,11)], k=5, n_ahead = 6, lags = 6, var_index = 3)
model7 = accuracy.kfold(df.VAR = alldataNs[,c(2,3,11)], k=5, n_ahead = 6, lags = 7, var_index = 3)

f.extract = function(x){x[2,2]}
m1 = rbind(lapply(model1, f.extract))
m2 = rbind(lapply(model2, f.extract))
m3 = rbind(lapply(model3, f.extract))
m4 = rbind(lapply(model4, f.extract))
m5 = rbind(lapply(model5, f.extract))
m6 = rbind(lapply(model6, f.extract))
m7 = rbind(lapply(model7, f.extract))


table_summary = data.frame(rbind(m1,m2,m3,m4,m5,m6,m7), row.names = c('Model 1', 'Model 2', 'Model 3', "Model 4", "Model 5", "Model 6", "Model 7"))
colnames(table_summary) = c('k=1','k=2','k=3', "k=4", "k=5")
#kableExtra::kable(table_summary, caption = 'MSE for each model and each fold') %>% 
#  kableExtra::kable_styling()