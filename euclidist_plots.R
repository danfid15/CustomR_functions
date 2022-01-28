# This function calculates Euclidean distances, and plots them in 3 different ways: 
## 1)heatmap 
## 2)MDS scatterplot 
## 3)Ward clustering

# The first column must be the name of the elements we are analyzing
# If TRUE, color of the MDS scatterplot is based on the second column
# If TRUE, shape of the MDS scatterplot is based on the third column
# ward_k is sets the number of color clusters in Wards cluster plot.

euclidist_plots<- function(data,color=TRUE,shape=TRUE,ward_k=3){
  
  require(MASS)
  require(ggplot2)
  require(ggfortify)
  require(reshape2)
  require(factoextra)
  
  attributes<- as.factor(data[,1])
  nvars<- ncol(data)
  start<- 2
  
  # Use the second column to color the data points
  if(color==TRUE){
    color<- as.factor(data[,2])
    start<- start+1
  }else{
    color<- NULL
  }
  
  # Use the third column to change the shape of the data points
  if(shape==TRUE){
    shape<- as.factor(data[,3])
    start<- start+1
  }else{
    shape<- NULL
  }
  
  #1. Euclidean distance matrix
  
  eucldist <- dist(data[start:nvars])
  eucldist <-as.matrix(eucldist)
  row.names(eucldist)<-colnames(eucldist)<-data[,1]
  
  #1.1 Plot Matrix
  eucldist.plot<- eucldist
  eucldist.plot[upper.tri(eucldist.plot)]<-NA
  
  for (a in 1:ncol(eucldist.plot)) {
    eucldist.plot[a,a]<-NA
  }
  
  eucldist.plot<- melt(eucldist.plot,na.rm = TRUE)
  
  p1 <- ggplot(eucldist.plot,aes(x=Var1,y=Var2,fill=value))+
    theme_classic()+
    geom_tile(color="white")+
    geom_text(aes(Var1,Var2,label= round(value,digits = 2)),color="black",size=4)+
    scale_fill_viridis_c(alpha = 0.8)+
    labs(fill="Euclidean\n Distance", x="",y="")+
    coord_fixed()+
    theme(legend.direction = "horizontal",
          axis.text.x = element_text(angle = 90),
          legend.position = c(0.3,0.8))+
    guides(fill=guide_colorbar(title.position = "top",title.hjust = 0.5))
  
  #2.Multidimensional Scaling
  MDS <- isoMDS(as.matrix(eucldist))
  MDS.stress<-MDS$stress
  MDSPoints1 <- MDS$points[,1]
  MDSPoints2 <- MDS$points[,2]
  
  p2<- ggplot(MDS, aes(x = MDSPoints1, y = MDSPoints2, 
                       color = color,shape = shape))+
    geom_point(aes(size=5))+
    geom_text(aes(label= attributes),vjust=-0.8,size=5,show.legend = FALSE)+
    theme_classic()+ 
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.title = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 10),
          plot.caption = element_text(size = 12))+
    labs(x = "MDS1",y = "MDS2",
         caption = paste("MDS Stress:",round(MDS$stress,digits = 2),"%"))+
    guides(size="none")
  
  #Hierarchical cluster - Ward.D
  
  Ward.EU <-hclust(as.dist(eucldist),method="ward.D")
  
  ward_k <- ward_k
  
  p3<- fviz_dend(x=Ward.EU,cex = 0.8,lwd = 0.8,
                          k= ward_k,k_colors = "lancet",
                          rect = TRUE,rect_border = "lancet",rect_fill = TRUE,
                          horiz = TRUE,
                          ggtheme = theme_void(),
                          type = "rectangle",repel = TRUE)
  
  
  return(list(euclidean_matrix = eucldist,
              euclidean_heatmap = p1,
              euclidean_MDS = p2,
              euclidean_ward = p3))
  
}

