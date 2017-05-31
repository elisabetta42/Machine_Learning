################################## RUN PCA #################################################

person_dep_function<-function(dataset){
  pers_dep<-create_pca_dataset(dataset)
  train_sample_num<-round((nrow(dataset)/100)*50)
  test_sample_num<-round((nrow(dataset)/100)*50)
  
  #eliminate the last index from the range to use the right number of people
  #create a sequence to access data one person per time
  pca_results<-matrix(nrow = length(dataset)) #results using 11 PCAs
  
  #run for an increasing number of PCAs with a split 70-30#####################
  for(i in 3:ncol(dataset)){
    
    #shaffle data of the person
    pers_dep<-pers_dep[sample(nrow(pers_dep)),]
    
    #take a certain number of principal component
    pers_dep_dataset<-pers_dep[,1:i]
    
    #divide between test and train
    pers_dep_train<-pers_dep[1:(nrow(pers_dep_dataset)/2),]
    pers_dep_test<-pers_dep[(nrow(pers_dep_dataset)/2+1):(nrow(pers_dep_dataset)),]
    pers_dep_prediction<-knn(train = pers_dep_train[,-1], test = pers_dep_test[,-1], 
                             cl = pers_dep_train[,1], k = 1)
    
    confusion <- confusionMatrix(pers_dep_prediction, pers_dep_test[,1])
    pca_results[i] <- confusion$overall['Accuracy']
  }
  return(pca_results)
}

#printing results ordered by accuracy
#print(pca_results)
#result<-rowMeans(knn_results, na.rm = FALSE, dims = 1)
#qplot(unlist(1:length(pca_results)),unlist(pca_results), geom = "line",xlab="Number of principal components",ylab="Accuracy (in percentace)") + ggtitle("Accuracy for increasing number of principal components")
#max_pca_number<-which.max(pca_results)

accuracies<-person_dep_function(dataset)