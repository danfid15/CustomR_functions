
#This function calculates the Shapiro-Wilk test of normality.
#If p.value >0.05, you can say that the variable is not normally distributed. 

# this function automatically removes all non-numeric variables from the dataset
# this function automatically calculates the Shapiro-Wilk test for all numeric variables
# this function also has a plot function. Bins and density represent the data.
# Dashed red lines in the plot represent a normal distribution.

shap.wilk <-function(data){
  
  numcol.index <- unlist(lapply(data, is.numeric)) 
  data <- data[ , numcol.index]
  data <- data.frame(data)
  
  #1. Packages
  require(ggplot2)
  require(viridis)
  
  #2. Results table
  nvars<- ncol(data)
  
  results <- data.frame(matrix(NA,nrow = nvars,ncol= 6))
  colnames(results)<- c("Variable","N","Mean","SD","Shapiro-Wilks W","Shapiro-Wilks p.value")
  
  for (a in 1:(nvars)) {
    
    results[a,1]<- colnames(data)[a]
    results[a,2]<- length(data[,a])-sum(is.na(data[,a]))
    results[a,3]<- mean(data[,a])
    results[a,4]<- sd(data[,a])
    
    shapiro <- shapiro.test(data[,a])
    
    results[a,5]<- shapiro$statistic
    results[a,6]<- shapiro$p.value
    
  }
  
  #Plotting the graph
  colors <- viridis(nvars)
  
  for (a in 1:nvars) {
    
    x<-seq(min(data[,a]),max(data[,a]),length=500)
    norm_dens<-data.frame(x=x,y=dnorm(x,mean=mean(data[,a]),sd=sd(data[,a])))
    
    #Histogram
    print(ggplot(data, aes(x = data[,a], y = ..density..))+
            geom_histogram(fill = colors[a], color = "black")+
            geom_density(fill = "white", color = "black", alpha = 0.3)+
            geom_line(data=norm_dens,aes(x=x,y=y),linetype="dashed",colour="red")+
            theme_classic()+
            theme(panel.grid.minor = element_blank())+
            labs(title = paste("Histogram of",results[a,1]), 
                 x = results[a,1],
                 caption = paste("Shapiro-Wilk p.value=",round(results[a,6],digits = 3))))
    
  }
  
  return(results)
}

## Example 1 
# shap.wilk(iris)

## Example 2
# shap.wilk(cars)