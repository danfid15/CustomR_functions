# A function to calculate the mode for a given vector
# This function does not for for entire data.frames, only single vectors.

mode<- function(vector){
  
  #transfor the vector into a factor 
  vector<- as.factor(vector)
  #Use the table function to count each of the factor
  table_vector<- table(vector)
  #Which factor repeats itself the most
  max_index<- max(table(vector))
  #print the name of the factor
  result<- names(which(table_vector==max_index))
  
  return(result)
}

# Example 1 - mode of cut for diamonds 
library(ggplot2) # dataset
diamonds<- diamonds
mode(diamonds$cut)



