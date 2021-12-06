#Chi-square function 
#Tables raw data between two categorical variables (these must be in the first column).

chi2 <- function(data){
  
  #Getting the data from the dataset
  data<- data.frame(data)
  data[,1]<- as.factor(data[,1])
  nvars<- ncol(data)
  
  #Checking if the dataset as two categorical variables
  if(nlevels(data[,1])>2){
    stop("The first column has more than two categorical variables")
  }
  
  if(nlevels(data[,1])==1){
    stop("The first column only has one categorical variables, it needs two")
  }
  
  #Results table
  results<- data.frame(matrix(NA,nrow = nvars-1,ncol = 5))
  colnames(results)<- c("Variable","N","x2","degree of freedom","p.value")
  results[,1]<- colnames(data[2:nvars])
  
  
  #Chi-square test 
  #Building the contingency table and applies x2 test for each variable
  
  for (a in 1:(nvars-1)) {
    test <- chisq.test(table(data[,1],data[,a+1])) 
    results[a,2] <- sum(table(data[,1],data[,a+1]))
    results[a,3] <- round(test$statistic,digits = 3)
    results[a,4] <- test$parameter
    results[a,5] <- round(test$p.value,digits = 2)
    
  }
  
  #Returning values
  return(results)
}

