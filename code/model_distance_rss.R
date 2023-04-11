

# fitting exponential model to estimate the signal strength-distance relationship

# drawing heavily from: Paxton, K.L., Baker, K.M., Crytser, Z.B., Guinto, R.M.P., Brinck, K.W., Rogers, H.S., Paxton, E.H., 2022. Optimizing trilateration estimates for tracking fine-scale movement of wildlife using automated radio telemetry networks. Ecology and Evolution 12, e8561. https://doi.org/10.1002/ece3.8561
# and assiciated code: https://github.com/kpaxton75/EcolEvol.Manuscript_Optimizing.Trilateration

library(dplyr)
library(lubridate)
library(ggplot2)
library(here)



test_data <- readRDS(here("data/node_data/cleaned_test_data")) %>% 
  filter(date == "2022-10-25")



exp.mod <- nls(Signal ~ SSasymp(ant.tag.distance, Asym, R0, lrc), data = test_data)


summary(exp.mod)


exp(coef(exp.mod)[["lrc"]])

## 
a <- coef(exp.mod)[["R0"]]
S <- exp(coef(exp.mod)[["lrc"]])
K <- coef(exp.mod)[["Asym"]]


  # Final Model
nls.mod <- nls(Signal ~ a * exp(-S * ant.tag.distance) + K, start = list(a = a, S = S, K= K), 
               data = test_data)
  # Model Summary
summary(nls.mod)

coef(nls.mod)



## Check the fit of the model and get predicted values

  # Get residuals and fit of model and add variables to main table
test_data$E <- residuals(nls.mod)
test_data$fit <- fitted(nls.mod)

  # Plot residuals by fit or distance
ggplot(test_data, aes(x = ant.tag.distance, y = E, color = NodeID)) +
         geom_point(size = 2)
ggplot(test_data, aes(x = fit, y = E, color = NodeID)) +
  geom_point(size = 2)


  # Get model predictions
zpred <- predict(nls.mod, newdata = data.frame(ant.tag.distance = seq(0, 900, by = 20))) %>% 
  data.frame() %>% 
  rename("zpred" = 1) %>% 
  mutate(ant.tag.distance = seq(0, 900, by = 20))

ggplot(test_data)+
  geom_point(aes(x = ant.tag.distance, y = Signal, color = NodeID)) +
  geom_line(data = zpred, aes(x = ant.tag.distance, y = zpred))

