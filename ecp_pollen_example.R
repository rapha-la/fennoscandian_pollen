### ecp ###
install.packages("ecp")
library(ecp)

big12=bigdf_genera[bigdf_genera$dataset_ID==12,]

df_ecp = as.matrix(big12$Betula)
fit_ecp1 = e.cp3o(Z = df_ecp, K = 2)  
fit_ecp2 = e.cp3o_delta(df_ecp, K = 2)  
fit_ecp3 = e.divisive(df_ecp, k = 2)  
fit_ecp4 = ks.cp3o(df_ecp, K = 2)  
fit_ecp5 = ks.cp3o_delta(df_ecp, K = 2)  

# Show the change point estimates
print(fit_ecp3$estimates)


