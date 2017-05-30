#data from all the person with a training set that includes data from all persons
require(gmodels)
require(caret)
require(class)
#source('/Users/elisabetta/Desktop/ML/Machine_Learning/PROJECTMD/load_dataset.R') 
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_dataset.R')
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/createDatasets.R')

# Create PCA Dataset
pca<-create_pca_dataset(dataset)

# CHANGE K PARAMETERS and DO CROSS VALIDATION
cross_val_knn<-function(pca_dataset){
runs=9
fold_index<-seq(1, length(dataset[,1]),round(length(dataset[,1])/100*10))#create chunk of 10% per time
var_k <-c(3, 5, 10, 15, 20, 30, 40, 50, 100, 150)
vark_results<-matrix(nrow = length(var_k), ncol = length(runs))
for(a in 1:length(runs)){
  for(b in 1:length(var_knn)){
    pers_dep<-pca_dataset[sample(nrow(pca_dataset)),]
    pers_dep_train<-pers_dep[-(fold_index[a]:fold_index[a+1]),]
    pers_dep_test<-pers_dep[fold_index[a]:fold_index[a+1],]
    pers_dep_prediction<-knn(train = pers_dep_train[,-1], test = pers_dep_test[,-1], 
                             cl = pers_dep_train[,1], k = var_k[a])
    
    confusion <- confusionMatrix(pers_dep_prediction, pers_dep_test[,1])
    knn_cross_val[a,b] <- confusion$overall['Accuracy']
    }
  }
      return( knn_cross_val )
}


# PCA PARAMETERS

#insert the necessary dataset, cross validation for pca
  cross_val_pca<-function(pers_dep){
  
  knn_cross_val<-matrix(nrow = 1,ncol=length(pers_dep)) 
  fold_index<-seq(1, length(dataset[,1]),round(length(dataset[,1])/100*10))#create chunk of 10% per time
  
    for(b in 3:ncol( pers_dep)){
      pers_dep<-pers_dep[sample(nrow(pers_dep)),]
      pca<-pers_dep[,1:b]
      pers_dep_train<-pca[1:(nrow(pca)/2),]
      pers_dep_test<-pca[(nrow(pca)/2+1):(nrow(pca)),]
      pers_dep_prediction<-knn(train = pers_dep_train[,-1], test = pers_dep_test[,-1], 
                               cl = pers_dep_train[,1], k = 1)
      
      confusion <- confusionMatrix(pers_dep_prediction, pers_dep_test[,1])
      knn_cross_val[b] <- confusion$overall['Accuracy']
      }
    
  return(knn_cross_val)
}


# run cross validation
cross_val_result<-cross_val_pca(pca) # tune pca parameters
knn_cross_val<-cross_val_knn(pca) #tune knn parameters

