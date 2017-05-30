#file to create dataset 
#function that returns a dataset containing just PCA

create_pca_dataset<-function(dataset){
  pers_dep_pca<-prcomp(dataset[2:length(dataset[1,])], retx=TRUE, center=TRUE, scale=TRUE)
  pers_dep_data<-pers_dep_pca$x[,1:ncol(dataset)-1] #pca object
  pers_dep_label<-dataset[,1] #labels from the original dataset
  
  #bind the pca components with labels
  pers_dep<-matrix(nrow = length(dataset[,1]), ncol = ncol(dataset))
  
  #conevrting large matrix into data frame matrix - indexing reasoning
  pers_dep <- as.data.frame(matrix(unlist(pers_dep), nrow = length(dataset[,1])))
  pers_dep[,1]<-pers_dep_label
  pers_dep[,-1]<-pers_dep_data
  
  return(pers_dep)
}

create_p_d_pca_object<-function(dataset){
  pd_pca <- prcomp(dataset, retx=TRUE, center=TRUE, scale=TRUE)
  eig.val <- get_eigenvalue(pd_pca)
  p_d_max <- matrix(0, nrow=1, ncol = 4)
  
  cv<-eig.val[,3] #save cumulative variance vector
  perc<-c(80, 90, 95, 99) #percentage to represent
  for(j in 1:length(p_d_max)){
    for(i in 2:length(cv)){
      if(cv[i]>=perc[j]){
        p_d_max[j]<-i
        break
      } 
    }
  } 
  return(p_d_max)
}