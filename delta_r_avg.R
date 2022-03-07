# This function is used to calculate the delta_R and delta_R_uncertainty for a given set of samples 
## This is used if we are pooling more than 1 sample to account for reservoir effects during C14 calibration

delta_r_avg <- function(delta_r,delta_error){

library(tidyverse)
  
data <- data.frame(cbind(delta_r = delta_r,delta_error = delta_error))

summary_delta <- data%>%
  select(delta_r,delta_error)%>%
  summarise(weighted_mean = round(sum(delta_r)/sum(delta_error),0),
            uncertainty = 1/sum(delta_error))

return(summary_delta)

}