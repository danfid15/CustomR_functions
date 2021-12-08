#this function does pairwise correlations between a set of numeric variables
#Default test is spearman, but it can be changed to others (e.g., "pearson)
#The lower triangle of the matrix gives p.values, and the upper triangle the correlations.
#It also provides two plots with colored results for p.values and correlations

pairwise_correlation<-function(data,cor.method = "spearman"){
  
  require(ggplot2)
  require(gridExtra)
  require(reshape2)
  
  data <- data.frame(data)
  
  nvars<-ncol(data)
  var.names<-colnames(data)
  
  results<- matrix(NA,nvars,nvars)
  colnames(results)<- var.names
  rownames(results)<- var.names
  
  
  for (a in 1:nvars-1) {
    for (b in 1:(nvars)) {
      
      cor.result<-cor.test(data[,b],data[,a+1],method = cor.method)
      results[a+1,b]<- cor.result[["estimate"]]
      results[b,a+1]<- cor.result[["p.value"]]
      
    }
  }
  
  for (a in 1:nvars) {
    results[a,a]<-1
  }
  
  # Plot r values
  r.triangle<- results
  r.triangle[upper.tri(r.triangle)]<-NA
  
  for (a in 1:ncol(r.triangle)) {
    r.triangle[a,a]<-NA
  }
  
  r.triangle<- melt(r.triangle,na.rm = TRUE)
  
  r.plot <- ggplot(r.triangle,aes(x=Var1,y=Var2,fill=value))+
    theme_classic()+
    geom_tile(color="white")+
    geom_text(aes(Var1,Var2,label= round(value,digits = 1)),color="black",size=3)+
    scale_fill_viridis_b(alpha = 0.9)+
    labs(fill="Correlation",
         title=paste(cor.method),
         x="",
         y="")+
    coord_fixed()+
    theme(axis.text.x = element_text(angle = 90))+
    guides(fill=guide_colorbar(title.position = "top",title.hjust = 0.5))
  
  # Plot p.values
  
  p.triangle<- results
  p.triangle[lower.tri(p.triangle)]<-NA
  
  for (a in 1:ncol(p.triangle)) {
    p.triangle[a,a]<-NA
  }
  
  p.triangle<- melt(p.triangle,na.rm = TRUE)
  
  p.plot <- ggplot(p.triangle,aes(x=Var1,y=Var2,fill=value))+
    theme_classic()+
    geom_tile(color="white")+
    geom_text(aes(Var1,Var2,label= round(value,digits = 2)),color="black",size=3)+
    scale_fill_viridis_b(alpha = 0.9)+
    labs(fill="p.value",
         title=paste(cor.method),
         x="",
         y="")+
    coord_fixed()+
    theme(axis.text.x = element_text(angle = 90))+
    guides(fill=guide_colorbar(title.position = "top",title.hjust = 0.5))
  
  
  return(list(matrix=results,
              r.graph=r.plot,
              p.graph=p.plot))
  
}

##Example 1
# pairwise_correlation(mtcars)