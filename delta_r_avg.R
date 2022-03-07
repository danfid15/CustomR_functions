# This function is used to calculate the delta_R and delta_R_uncertainty for a given set of samples 
## This is used if we are pooling more than 1 sample to account for reservoir effects during C14 calibration

delta_r_avg <- function(delta_r,delta_error){

library(tidyverse)
  
data <- data.frame(cbind(delta_r = delta_r, delta_error = delta_error))

summary_delta <- data%>%
  mutate(delta_r_var = delta_r/delta_error^2,
         delta_error_var = 1/delta_error^2) %>%
  summarise(weighted_mean = round(sum(delta_r_var)/sum(delta_error_var),0),
            uncertainty = 1/sum(delta_error_var))

return(summary_delta)

}