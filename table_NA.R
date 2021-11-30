# table.NA function

# This function automatically calculates the number and percentage of missing values for each column in a data frame. 
# It also plots these respective values in a horizontal plot

table_NA<- function(data){
  
  require(ggplot2)
  
  na.table<- matrix(NA,ncol(data),3)
  na.table[,1] <- colnames(data)
  na.table<- data.frame(na.table)
  colnames(na.table)<- c("Variable","n_missing","missing_percent")
  
  for (a in 1:(ncol(data))) {
    
    na.table[a,2]<- sum(is.na(data[,a]))
    na.table[a,3]<- paste(round((sum(is.na(data[,a]))/nrow(data)*100),1),"%")
    
  }
  
  p<- ggplot(na.table, aes(x=Variable, y=n_missing,fill=Variable,label=missing_percent))+
    geom_col()+
    geom_text(vjust=-0.4)+
    theme_classic()
  
  return(list(table.NA = na.table,
              barplot.NA = p))
}

#Example 1
table_NA(airquality)

#Example 2 (no missing values)
table_NA(diamonds)
