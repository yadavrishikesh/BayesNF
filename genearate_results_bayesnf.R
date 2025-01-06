rm(list =ls())
setwd(this.path::here())

## Eco data sets 
par(mfrow=c(2,3))
forecast<- read.csv("../code-DL/output_rdata/Eco/forecast_predictions.csv")

true <- exp(forecast$True_Values)
est <- exp(forecast$Predicted_Mean)
data <- data.frame(True = c(true), Estimated = c(est))
plot_range <- range(c(tru.e, est), na.rm = TRUE)
p_Eco_forecast_bayesnf <- ggplot(data, aes(x = True, y = Estimated)) +
  geom_point() +
  xlim(plot_range) +
  ylim(plot_range) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "BayesNF", x = "True", y = "Estimated") +
  theme_minimal()
p_Eco_forecast_bayesnf
save(p_Eco_forecast_bayesnf, file = "../codeRpackage-DGLM/test_code/appl/scatterplot_data/p_Eco_forecast_bayesnf.RData")


spatintpl<- read.csv("../code-DL/output_rdata/Eco/spatintpl_predictions.csv")
true <- exp(spatintpl$True_Values)
est <- exp(spatintpl$Predicted_Mean)
data <- data.frame(True = c(true), Estimated = c(est))
plot_range <- range(c(true, est), na.rm = TRUE)
p_Eco_spatintpl_bayesnf <- ggplot(data, aes(x = True, y = Estimated)) +
  geom_point() +
  xlim(plot_range) +
  ylim(plot_range) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "BayesNF", x = "True", y = "Estimated") +
  theme_minimal()
p_Eco_spatintpl_bayesnf
save(p_Eco_spatintpl_bayesnf, file = "../codeRpackage-DGLM/test_code/appl/scatterplot_data/p_Eco_spatintpl_bayesnf.RData")


spacetime_pred<- read.csv("../code-DL/output_rdata/Eco/spaceTime_predictions.csv")
true <- exp(spacetime_pred$True_Values)
est <- exp(spacetime_pred$Predicted_Mean)
data <- data.frame(True = c(true), Estimated = c(est))
plot_range <- range(c(true, est), na.rm = TRUE)
p_Eco_st_bayesnf <- ggplot(data, aes(x = True, y = Estimated)) +
  geom_point() +
  xlim(plot_range) +
  ylim(plot_range) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "BayesNF", x = "True", y = "Estimated") +
  theme_minimal()
p_Eco_st_bayesnf
save(p_Eco_st_bayesnf, file = "../codeRpackage-DGLM/test_code/appl/scatterplot_data/p_Eco_st_bayesnf.RData")


## Portland data sets
forecast<- read.csv("../code-DL/output_rdata/Portland//forecast_predictions.csv")
true <- forecast$True_Values
est <- exp(forecast$Predicted_Mean)
data <- data.frame(True = c(true), Estimated = c(est))
plot_range <- range(c(true, est), na.rm = TRUE)
p_prtland_forecast_bayesnf <- ggplot(data, aes(x = True, y = Estimated)) +
  geom_point() +
  xlim(plot_range) +
  ylim(plot_range) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "BayesNF", x = "True", y = "Estimated") +
  theme_minimal()
p_prtland_forecast_bayesnf
save(p_prtland_forecast_bayesnf, file = "../codeRpackage-DGLM/test_code/appl/scatterplot_data/p_prtland_forecast_bayesnf.RData")


spatintpl<- read.csv("../code-DL/output_rdata/Portland/spatialinpl_predictions.csv")
true <-  spatintpl$True_Values
est <- exp(spatintpl$Predicted_Mean)
data <- data.frame(True = c(true), Estimated = c(est))
plot_range <- range(c(true, est), na.rm = TRUE)
p_prtland_spatintpl_bayesnf <- ggplot(data, aes(x = True, y = Estimated)) +
  geom_point() +
  xlim(plot_range) +
  ylim(plot_range) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "BayesNF", x = "True", y = "Estimated") +
  theme_minimal()
p_prtland_spatintpl_bayesnf
save(p_prtland_spatintpl_bayesnf, file = "../codeRpackage-DGLM/test_code/appl/scatterplot_data/p_prtland_spatintpl_bayesnf.RData")


spacetime_pred<- read.csv("../code-DL/output_rdata/Portland/spacetime_predictions.csv")
true <-  spacetime_pred$True_Values
est <- exp(spacetime_pred$Predicted_Mean)
data <- data.frame(True = c(true), Estimated = c(est))
plot_range <- range(c(true, est), na.rm = TRUE)
p_prtland_st_bayesnf <- ggplot(data, aes(x = True, y = Estimated)) +
  geom_point() +
  xlim(plot_range) +
  ylim(plot_range) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "BayesNF", x = "True", y = "Estimated") +
  theme_minimal()
p_prtland_st_bayesnf
save(p_prtland_st_bayesnf, file = "../codeRpackage-DGLM/test_code/appl/scatterplot_data/p_prtland_st_bayesnf.RData")

