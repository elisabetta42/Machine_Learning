#Data from all persons where the training set does not include data from the test set
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

########################## PCA  ###########################################