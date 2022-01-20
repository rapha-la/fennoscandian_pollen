## mcp - change point detection
#-------------------------------------

#install.packages('mcp')
library('mcp')

# Simulate
set.seed(42)  # I always use 42; no fiddling

### Betula_site12
big12=bigdf_genera[bigdf_genera$dataset_ID==12,]

model = list(Betula~meantimes+1, 1~meantimes+1)  # three intercept-only segments
model_null = list(Betula~meantimes+1)

fit_mcp = mcp(model, data = big12, par_x = "meantimes")

fit_mcp_null = mcp(model_null, data = big12, par_x = "meantimes")

fit_mcp$loo = loo(fit_mcp)
fit_mcp_null$loo = loo(fit_mcp_null)

loo::loo_compare(fit_mcp$loo, fit_mcp_null$loo)
summary(fit_mcp)
plot(fit_mcp)


##Betula_all
model = list(Betula~meantimes+1, 1~meantimes+1)  # three intercept-only segments
model_null = list(Betula~meantimes+1)

fit_mcp = mcp(model, data = bigdf_genera, par_x = "meantimes")

fit_mcp_null = mcp(model_null, data = bigdf_genera, par_x = "meantimes")

fit_mcp$loo = loo(fit_mcp)
fit_mcp_null$loo = loo(fit_mcp_null)

loo::loo_compare(fit_mcp$loo, fit_mcp_null$loo)
summary(fit_mcp)
plot(fit_mcp)


##Cerealia
model = list(Cerealia~meantimes+1, 1~meantimes+1)  # three intercept-only segments
model_null = list(Cerealia~meantimes+1)

fit_mcp = mcp(model, data = bigdf_genera, par_x = "meantimes")

fit_mcp_null = mcp(model_null, data = bigdf_genera, par_x = "meantimes")

fit_mcp$loo = loo(fit_mcp)
fit_mcp_null$loo = loo(fit_mcp_null)

loo::loo_compare(fit_mcp$loo, fit_mcp_null$loo)
summary(fit_mcp)
plot(fit_mcp)