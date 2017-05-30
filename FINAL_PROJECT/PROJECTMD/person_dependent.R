#data from all the person with a training set that includes data from all persons
#require(gmodels)
#require(caret)
#require(class)
require(factoextra)
#source('/Users/elisabetta/Desktop/ML/Machine_Learning/PROJECTMD/load_dataset.R') 
#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_dataset.R')
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/createDatasets.R')
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/kmeans.R')
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_persons_modified.R')

dataset<-loadYearData(100,2017)

# FIND BEST PCA THAT EXPLAINS A CERTAIN VARIANCE
knn_with_pca<-function(dataset,p_d_max){
  dataset<-dataset[sample(nrow(dataset)),]
  pers_dep<-dataset
  var_k <-c(1,3,5,7,11,21,31,41,51,61)
  var_k_acc  <- matrix(nrow=length(var_k), ncol=length(p_d_max))
  var_k_time <- matrix(nrow=length(var_k), ncol=length(p_d_max))
  
  for(i in 1:length(p_d_max)){
    dataset <- pers_dep[,1:p_d_max[i]]  
    # define classification labels for dataset - 50% - 50% split
    classification_table <- dataset[,2:length(dataset[1,])]
    training_label <- dataset[1:(nrow(dataset)/2),1]
    test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset),1]
    
    # divide into classification sets: 50/50 training testing
    training_class <- classification_table[1:(nrow(dataset)/2),]
    test_class <- classification_table[(nrow(dataset)/2+1):(nrow(dataset)),]
    
    ## Exercise 1.3
    for (j in 1:length(var_k)){
      #print(length(dataset))
      start.time <- Sys.time()
      #running knn with k = var_k(i)
      test_pred <- knn(train = training_class, test = test_class, 
                       cl = training_label, k = var_k[j])
      time.taken <- Sys.time() - start.time #difftime
      
      #create confusion matrix and add accuracy to list
      confusion <- confusionMatrix(test_pred, test_label)
      var_k_acc[j,i] <- confusion$overall['Accuracy']
      var_k_time[j,i] <- time.taken
    }  
  }
  return(var_k_acc)
  
}

# CROSS VALIDATION
# Check which cluster better suit the dataset

cross_val_pca<-function(pers_dep_train,pers_dep_test,runs){
  var_k <-c(1,3,5,7,11,21,31,41,51,61,100)
  cross_val_results<-matrix(nrow = runs,ncol=length(var_k)) 
 
  for(i in 1:runs){
    for(j in 1:length(var_k)){
      pers_dep_prediction<-knn(train = pers_dep_train[,-1], test = pers_dep_test[,-1], 
                               cl = pers_dep_train[,1], k = var_k[j])
      
      confusion <- confusionMatrix(pers_dep_prediction, pers_dep_test[,1])
      cross_val_results[i,j] <- confusion$overall['Accuracy']
    }
  }
  return(cross_val_results)
}
createsequence<-function(lower,higher,interval){
  fold_index<-seq(lower, higher,interval)#create chunk of 10% per time
  return(fold_index)
}

# RUNNING STUFF 

# PCA PARAMETERS
pca_parameters<-create_p_d_pca_object(dataset)
# CREATE PCA DATASET

dataset<-dataset[sample(nrow(dataset)),]
pca<-create_pca_dataset(dataset)
# TUNE PCA PARAMETERS WITH (80% - 90% - 95% - 99%)
knn_cross_val<-knn_with_pca(pca,pca_parameters) #tune knn parameters

# TUNE PARAMETERS FOR THE CLUSTER
k=c(20,50,70,90,100,200,300,400,500,600,700,800,900,1000)#number of clusters
pers_dep<-pca[,1:22]
pers_dep<-pers_dep[order(pers_dep[,1]),]
amountofEachCipher=length(which(pers_dep[,1]==0))
result<-vector('list')
test_dataset<-pers_dep
fold_index<-createsequence(0, length(dataset[,1]),round(length(dataset[,1])/100*10))

for(c in 1:length(k)){
  
  # create the dataset with a predefined number of clusters
  cluster_knn<-kMeansClusterPerformIncludeSpilt(test_dataset,k[c],amountofEachCipher)
  pers_dep<-pers_dep[sample(nrow(pers_dep)),]
  labels=cluster_knn$labels
  cluster=cluster_knn$centers
  pca_cluster_table<-cbind(labels,cluster)
  pca_cluster_table<-pca_cluster_table[sample(nrow(pca_cluster_table)),]
  # tune the cluster parameters
  pers_dep_test<-pers_dep[fold_index[c]:fold_index[c+1],]
  cluster_acc<-cross_val_pca(pca_cluster_table,pers_dep_test,10)
  result[[c]]<-cluster_acc
}




#pca_table<-cross_val_pca(pca[,1:pca_parameters[1]],10,100/10)
#
#fold_index<-seq(0, length(dataset[,1]),interval)#create chunk of 10% per time

#pers_dep<-dataset[sample(nrow(pers_dep)),]
#pers_dep_train<-pers_dep[-(fold_index[i]:fold_index[i+1]),]
#pers_dep_test<-pers_dep[fold_index[i]:fold_index[i+1],]
# CROSS VALIDATION WITH PCA

