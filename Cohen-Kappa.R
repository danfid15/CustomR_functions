
# Cohen's Kappa agreement Coefficient

# This function calculates the agreement coefficient between all variables of two matrices.
# The output is a table with all the results, and a horizontal chart.
# Agreement interpretations are based on Landis, J.R.; Koch, G.G. (1977). “The measurement of observer agreement for categorical data”. Biometrics. 33 (1): 159–174. doi:10.2307/2529310. JSTOR 2529310. PMID 843571.

# Rule 1 - Two datasets are required , and the variables must be in the exact same order in both datasets.
# Rule 2 - Only columns with numeric variables are allowed in the datasets.
# Rule 3 - As default, any NAs will be replaced by 999.

kappa2.table <- function(rater1,rater2){
  
  if(nrow(rater1)!=nrow(rater2)){
    stop("number of rows is not the same")
  }
  
  if(ncol(rater1)!=ncol(rater2)){
    stop("number of variables is not the same")
  }
  
  if(all.equal(row.names(rater1),row.names(rater2))!=TRUE){
    stop("rownames are not the same")
  }
  
  if(all.equal(colnames(rater1),colnames(rater2))!=TRUE){
    stop("variables are not the same")
  }
  
  #Packages
  require(irr)
  require(ggplot2)
  require(RColorBrewer)
  
  # Results table (final table to be filled and printed)
  nvars <- ncol(rater1)
  
  results <- data.frame(matrix(NA,nvars,5))
  colnames(results)<- c("Variable","Accuracy %","Kappa","p.value","Agreement")
  results[,1]<- colnames(rater1[,1:nvars])
  
  
  #Replacing NAs in the two matrices for another number
  
  rater1[is.na(rater1)] = 999
  rater2[is.na(rater2)] = 999
  
  
  # Precision table (used to calculate the Precision % in results[,2])
  nrows <-nrow(rater1)
  precision.table <- data.frame(matrix(0,nrows,nvars))
  
  for (a in 1:(nvars)) {
    for (b in 1:nrows) {
      if(rater1[b,a]==rater2[b,a]){
        precision.table[b,a]<- +1
      }
    }
  }
  
  
  # Filling the results table
  for (a in 1:(nvars)) {
    
    test <- kappa2(cbind(rater1[,a],rater2[,a]))
    
    results[a,2] <- round(sum(precision.table[,a])/nrows*100,digits = 1)
    results[a,3] <- round(test$value,digits = 3)
    results[a,4] <- round(test$p.value,digits = 3)
    
    
    if(results[a,3]<=0.200){
      results[a,5]<- "no agreement"
    } 
    
    if(results[a,3]>0.200){
      results[a,5]<- "fair agreement"
    }
    
    if(results[a,3]>0.400){
      results[a,5]<- "moderate agreement"
    }
    
    if(results[a,3]>0.600){
      results[a,5]<- "substantial agreement"
    }
    
    if(results[a,3]>0.800){
      results[a,5]<- "almost perfect agreement"
    }
    
  }
  
  #filtering p.values <= 0.05 for the plotting
  results.graph <- results
  
  for(a in 1:(nvars)) {
    if(results.graph[a,4]>0.05){
      results.graph[a,4]<-NA
    }
  }
  
  
  #Making the graph to plot with the results table
  kappa.plot<- ggplot(data = results.graph, mapping = aes(x = reorder(Variable, Kappa), y = Kappa))+
    theme_classic()+
    theme(legend.title = element_blank(),legend.position="bottom")+
    labs(title= "Cohen's Kappa",subtitle = "Agreement between two raters",caption = "Dotted line >= substantial agreement")+
    xlab("Variables")+
    ylab("Kappa value")+
    geom_col(aes(colour = Agreement,fill = Agreement),alpha=0.6,width = 0.8)+
    geom_text(mapping = aes(label = ifelse(is.na(results.graph[,4]),"",ifelse(results.graph[,4]<0.001,"p<0.001",paste("p=",results.graph[,4])))),position = position_stack(vjust = 0.5),fontface = "italic",size = 3.5)+
    geom_hline(yintercept = 0.6, size = 1, linetype = 3)+
    scale_y_continuous(breaks=seq(-1,1,0.1))+
    scale_color_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2")+
    coord_flip()
  
  
  #Final edit on the p.values of the results table
  for (a in 1:(nvars)){
    results[a,4] <- round(test$p.value,digits = 3)
    if(results[a,4]<=0.001){
      results[a,4] <-"p<0.001"
    }
  }
  
  #Printing the results and graph
  print(kappa.plot)
  return(results)
  
}

## Example 1
Matrix1 <- data.frame(matrix(runif(200,0,1), ncol = 20))
Matrix1 <- round(Matrix1, digits = 0)
 
Matrix2 <- data.frame(matrix(runif(200,0,1), ncol = 20))
Matrix2 <- round(Matrix2, digits = 0)

kappa2.table(Matrix1,Matrix2)
