# Dichotomizing variables

#This function is used for dichotomizing numeric vectors into "0" and "1", based on user provided breaking points.

#By default, values equal or above the breaking point will be transformed to "1". Values below the breaking point will be transformed to "0".
#If greater.or.equal = FALSE, values equal or below the breaking point will be transformed to "1". Values above the breaking point will be transformed to "0".

#Missing values will always be considered as NA.


dichotomize <- function(trait,breaking.point,greater.or.equal=TRUE){
  
  n <- length(trait)
  results<- rep(NA,n)
  
  if(greater.or.equal==TRUE){
    for(a in 1:n){
      if (is.na(trait[a]==TRUE)){
        results[a]<-NA
      }else{
        if (trait[a]>=breaking.point){
          results[a]<- 1}
        if(trait[a]<breaking.point){
          results[a]<- 0
        }
      }
    }
  }else{
    for(a in 1:n){
      if (is.na(trait[a]==TRUE)){
        results[a]<-NA
      }else{
        if (trait[a]<=breaking.point){
          results[a]<- 1}
        if(trait[a]>breaking.point){
          results[a]<- 0
        }
      }
    }
    
  }
  return(results)
}

# #example 1 - dichomotize car speed above or equal to 13.
# dichotomize(cars$speed,13)
# 
# #example 2 - dichomotize car speed below or equal to 13.
# dichotomize(cars$speed,13, greater.or.equal = FALSE)
