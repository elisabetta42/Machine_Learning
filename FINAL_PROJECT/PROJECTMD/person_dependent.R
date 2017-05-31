#data from all the person with a training set that includes data from all persons
#require(gmodels)
#require(caret)
#require(class)
require(factoextra)
#source('/Users/elisabetta/Desktop/ML/Machine_Learning/PROJECTMD/load_dataset.R') 
#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_dataset.R')
#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/createDatasets.R')
#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/kmeans.R')
#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_persons_modified.R')

#dataset<-loadYearData(100,2017)

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
  var_k <-c(1,3,5,7,11,21,31,41,51,61,71,81,91,101)
  results<-matrix(nrow = runs,ncol=length(var_k)) 
  #results <- vector('list')
  times <- matrix(nrow = runs,ncol=length(var_k)) 
  
  for(i in 1:runs){
    for(j in 1:length(var_k)){
      
      start.time <- Sys.time()
      pers_dep_prediction<-knn(train = pers_dep_train[,-1], test = pers_dep_test[,-1], 
                               cl = pers_dep_train[,1], k = var_k[j])
      time.taken <- Sys.time() - start.time
      
      confusion <- confusionMatrix(pers_dep_prediction, pers_dep_test[,1])
      results[i,j] <- confusion$overall['Accuracy']
      times[i,j] <- time.taken
    }
  }
  #results=rbind(results,times)
  return(list(v1=results,v2=times))
}

createsequence<-function(lower,higher,interval){
  fold_index<-seq(lower, higher,interval)#create chunk of 10% per time
  return(fold_index)
}
