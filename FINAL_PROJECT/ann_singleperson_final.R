# Imports
require(caret)
require(RSNNS)
require(gmodels)
require(ggplot2)
require(class)
require(factoextra)
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_persons_modified.R')
#source('C:/Users/Christian/Documents/GitHub/Machine_Learning/FINAL_PROJECT/ann_load_dataset.R')
# Import the function to plot neural networks from Github
# Not sure we are going to use it though. Takes a shit ton of time!
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


fulldataset<-loadYearData(100,2017)
# You can now iterate trough the list
person_index<-seq(0, length(fulldataset[,1]),4000) #create a sequence to run one person per time
length<-length(person_index)-1

list.accuracy <- table(1:length)
list.accuracy.pca <- table(1:length)
list.time.train.pca <- table(1:length)
list.time.ann <- table(1:length)
list.time.ann.pca <- table(1:length)
list.time.predict <- table(1:length)
list.time.predict.pca <- table(1:length)

for(i in 1:length) {
  
  lower<-person_index[i]+1
  dataset<-fulldataset[lower:person_index[i+1],] #dataset for one person
  dataset <- dataset[sample(nrow(dataset)),]
  
  # Divide into training and test set.
  training_label <- factor(dataset[1:(nrow(dataset)/2), 1])
  levels <- levels(training_label)
  training_set <- dataset[1:(nrow(dataset)/2), -1]
  test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset), 1]
  test_set <- dataset[((nrow(dataset)/2)+1):nrow(dataset), -1]
  
  # Divide into pca training and test set.
  pca.training_label <- as.factor(dataset[1:(nrow(dataset)/2), 1])
  pca.levels <- levels(training_label)
  start.time <- Sys.time()
  pca.training_set.temp <- prcomp((dataset[1:(nrow(dataset)/2), -1]), retx = TRUE, center = TRUE, scale. = TRUE)
  pca.training_set <- pca.training_set.temp$x[,1:100]
  pca.test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset), 1]
  pca.test_set <- dataset[((nrow(dataset)/2)+1):nrow(dataset), -1]
  pca.test_set.pca <- prcomp(pca.test_set, retx = TRUE, center = TRUE, scale. = TRUE)
  pca.test_set <- predict(pca.test_set.pca, pca.test_set)
  list.time.train.pca[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Prepare nn set
  
  # No PCA
  temp.trainingClass <- matrix(nrow = length(training_label), ncol = 10, data = 0)
  # Prepare nn test set
  for (z in 1:length(training_label)) {
    matchList <- match(levels, toString(training_label[z]))
    matchList[is.na(matchList)] <- 0
    temp.trainingClass[z,] <- matchList
  }
  trainingClass <- as.data.frame(temp.trainingClass)
  
  # PCA
  pca.temp.trainingClass <- matrix(nrow = length(pca.training_label), ncol = 10, data = 0)
  # Prepare nn test set
  for (z in 1:length(pca.training_label)) {
    matchList <- match(pca.levels, toString(pca.training_label[z]))
    matchList[is.na(matchList)] <- 0
    pca.temp.trainingClass[z,] <- matchList
  }
  pca.trainingClass <- as.data.frame(pca.temp.trainingClass)
  
  #
  # Define network
  networkSize = c(100, 40, 30) # ex c(#nodes, #nodes) = two layers
  networkMaxEpochs = 150
  networkLearningFunc = "Std_Backpropagation"
  networkLearningFuncParam = c(0.1, 0)

  pca.networkSize = c(40, 20, 20)
  pca.networkMaxEpochs = 50
  pca.networkLearningFuncParam = c(0.2, 0)
  
  
  #
  # Train neural network model
  
  # No PCA
  start.time <- Sys.time()
  model <- mlp(x = training_set, y = trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
  list.time.ann[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  # PCA
  start.time <- Sys.time()
  pca.model <- mlp(x = pca.training_set, y = pca.trainingClass, size = pca.networkSize, maxit = pca.networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = pca.networkLearningFuncParam)
  list.time.ann.pca[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  #Prediction

  # No PCA
  start.time <- Sys.time()
  prediction <- predict(model, newdata = test_set)
  list.time.predict[i] <-  difftime(Sys.time(), start.time, units = "secs")
  
  # PCA
  start.time <- Sys.time()
  pca.prediction <- predict(pca.model, newdata = pca.test_set[,1:100])
  list.time.predict.pca[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Calculate the accuracy of the predictions 
  #
  # Person dependent, no PCA
  responselist <- matrix(nrow = length(prediction[,1]), ncol = 1, data = "Na")
  for(z in 1:nrow(prediction)) {
    responselist[z, ] <- toString(which(prediction[z, ] == max(prediction[z, ])) - 1)
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculate accuracy
  agreement <- responselist[,1] == test_label
  tt <- prop.table(table(agreement))
  list.accuracy[i] <- tt['TRUE']
  
  #PCA
  pca.responselist <- matrix(nrow = length(pca.prediction[,1]), ncol = 1, data = "Na")
  for(z in 1:nrow(pca.prediction)) {
    pca.responselist[z, ] <- toString(which(pca.prediction[z, ] == max(pca.prediction[z, ])) - 1)
  }
  pca.responselist <- data.frame(pca.responselist)
  pca.responselist[,1] <- as.factor(pca.responselist[,1])
  # Calculate accuracy
  pca.agreement <- pca.responselist[,1] == pca.test_label
  pca.tt<- prop.table(table(pca.agreement))
  list.accuracy.pca[i] <- pca.tt['TRUE']
  
  print("At iteration: ")
  print(i)
}
