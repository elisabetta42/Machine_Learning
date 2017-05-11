# Imports
require(caret)
require(RSNNS)
require(gmodels)
require(ggplot2)
require(class)
require(factoextra)
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/FINAL_PROJECT/ann_load_dataset.R')
# Import the function to plot neural networks from Github
# Not sure we are going to use it though. Takes a shit ton of time!
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

idList <- getAllData(dataList)
# You can now iterate trough the list

list.accuracy <- table(1:length(idList))
list.accuracy.pca <- table(1:length(idList))
list.time.train.pca <- table(1:length(idList))
list.time.ann <- table(1:length(idList))
list.time.ann.pca <- table(1:length(idList))
list.time.predict <- table(1:length(idList))
list.time.predict.pca <- table(1:length(idList))

for(i in 1:length(idList)) {
  dataset <- idList[i]
  dataset <- data.frame(dataset)
  dataset <- dataset[sample(nrow(dataset)),]
  
  # Divide into training and test set.
  training_label <- as.factor(dataset[1:(nrow(dataset)/2), 1])
  levels <- levels(training_label)
  training_set <- dataset[1:(nrow(dataset)/2), -1]
  test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset), 1]
  test_set <- dataset[((nrow(dataset)/2)+1):nrow(dataset), -1]
  
  # Divide into pca training and test set.
  pca.training_label <- as.factor(dataset[1:(nrow(dataset)/2), 1])
  pca.levels <- levels(training_label)
  start.time <- Sys.time()
  pca.training_set.temp <- prcomp((dataset[1:(nrow(dataset)/2), -1]), retx = TRUE, center = TRUE, scale. = TRUE)
  pca.training_set <- pca.training_set.temp$x#[,1:50]
  pca.test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset), 1]
  pca.test_set <- dataset[((nrow(dataset)/2)+1):nrow(dataset), -1]
  pca.test_set.pca <- prcomp(pca.test_set, retx = TRUE, center = TRUE, scale. = TRUE)
  pca.test_set <- predict(pca.test_set.pca, pca.test_set)
  list.time.train.pca[i] <- Sys.time() - start.time
  
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
  networkSize = c(20) # ex c(#nodes, #nodes) = two layers
  networkMaxEpochs = 600
  networkLearningFunc = "Std_Backpropagation"
  networkLearningFuncParam = c(0.045, 0)
  
  #
  # Train neural network model
  
  # No PCA
  start.time <- Sys.time()
  model <- mlp(x = training_set, y = trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
  list.time.ann[i] <- Sys.time() - start.time
  
  # PCA
  start.time <- Sys.time()
  pca.model <- mlp(x = pca.training_set, y = pca.trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
  list.time.ann.pca[i] <- Sys.time() - start.time
  
  #
  #Prediction

  # No PCA
  start.time <- Sys.time()
  prediction <- predict(model, newdata = test_set)
  list.time.predict[i] <-  Sys.time() - start.time
  
  # PCA
  start.time <- Sys.time()
  pca.prediction <- predict(pca.model, newdata = pca.test_set)
  list.time.predict.pca[i] <- Sys.time() - start.time
  
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
  
}
