# A function to calculate the mode for a given vector
# This function does not for entire data frames, only single vectors.

mode<- function(vector){
  
  vector<- as.factor(vector)
  table_vector<- table(vector)
  max_index<- max(table(vector))
  result<- names(which(table_vector==max_index))
  
  return(result)
}

# Example 1 - mode of cut for diamonds 
library(ggplot2) # dataset
diamonds<- diamonds
mode(diamonds$cut)



