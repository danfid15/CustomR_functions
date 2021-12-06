#Chi-square function that ruans chi-square test for every column except for the first.
# This function also produces a barplot for each variable
# The first column must contain only two categorical variables.

chi2 <- function(data){
  
  #Required packages
  require(ggplot2)
  
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
  results<- data.frame(matrix(NA,nrow = nvars-1,ncol = 4))
  colnames(results)<- c("Variable","x2","degree of freedom","p.value")
  results[,1]<- colnames(data[2:nvars])
  
  
  #Chi-square test 
  #Building the contingency table and applies x2 test for each variable
  
  for (a in 1:(nvars-1)) {
    test <- chisq.test(table(data[,1],data[,a+1])) 
    results[a,2] <- round(test$statistic,digits = 3)
    results[a,3] <- test$parameter
    results[a,4] <- round(test$p.value,digits = 2)
    
  }
  
  #Create a barplot
  
  for (a in 1:(nvars-1)) {
    print(
      ggplot(data = data, aes(x= data[,a+1], fill=data[,1]))+
        labs(title = paste("Barplot of",colnames(data[a+1])), x = "",subtitle = paste("Chi-square test of independence x2=",results[a,2], "p.value=",results[a,4]))+
        geom_bar(position=position_dodge(),colour="black")+
        geom_text(stat="count",aes(label=..count..),position = position_dodge(0.9),vjust = -0.5)+
        scale_fill_brewer(palette="Dark2")+
        theme_classic()+
        theme(legend.title = element_blank())
    )
  }
  
  
  #Returning values
  return(results)
}

