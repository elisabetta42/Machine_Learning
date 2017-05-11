#### ANN 10-Fold Cross Validation ####

# Imports
require(caret)
require(RSNNS)
require(gmodels)
require(ggplot2)
require(class)
require(factoextra)

# Load datasets 
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/FINAL_PROJECT/load_dataset.R')

# Set seed
set.seed(123)

# For person dependent
dependent_folds <- createFolds(dataset$X1, k = 10)
pca.dependent_folds <- createFolds(dataset$X1, k = 10)

# For person independent
dataset.shuffled <- dataset[sample(nrow(dataset)),]
independent_folds <- createFolds(dataset.shuffled$X1, k = 10)
pca.independent_folds <- createFolds(dataset.shuffled$X1, k = 10)

# Define network size. This was the 'best' result from our previous exercises.
# Each entry represents a hidden layer, the value the number of neurons
networkSize = c(20) # ex c(#nodes, #nodes) = two layers
networkMaxEpochs = 600
networkLearningFunc = "Std_Backpropagation"
networkLearningFuncParam = c(0.045, 0)

#
#### Person dependent no PCA ####
dependent.accuracy <- table(1:length(dependent_folds))
dependent.time.ann <- table(1:length(dependent_folds))
dependent.time.predict <- table(1:length(dependent_folds))

for (i in 1:length(dependent_folds)){
  #
  # Devide into training and test
  training_label <- as.factor(dataset[-dependent_folds[[i]], 1])
  levels <- levels(training_label)
  training_set <- dataset[-dependent_folds[[i]], -1]
  test_label <- dataset[dependent_folds[[i]], 1]
  test_set <- dataset[dependent_folds[[i]], -1]
  
  #
  # Define network training set
  temp.trainingClass <- matrix(nrow = length(training_label), ncol = 10, data = 0)
  
  # Prepare nn test set
  for (z in 1:length(training_label)) {
    matchList <- match(levels, toString(training_label[z]))
    matchList[is.na(matchList)] <- 0
   temp.trainingClass[z,] <- matchList
  }
  trainingClass <- as.data.frame(temp.trainingClass)
  
  #
  # Train neural network model
  start.time <- Sys.time()
  model <- mlp(x = training_set, y = trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
  dependent.time.ann[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Prediction
  start.time <- Sys.time()
  prediction <- predict(model, newdata = test_set)
  dependent.time.predict[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Calculate the accuracy of the predictions 
  responselist <- matrix(nrow = length(prediction[,1]), ncol = 1, data = "Na")
  for(z in 1:nrow(prediction)) {
    responselist[z, ] <- toString(which(prediction[z, ] == max(prediction[z, ])) - 1)
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculate accuracy
  agreement <- responselist[,1] == test_label
  tt <- prop.table(table(agreement))
  dependent.accuracy[i] <- tt['TRUE']
}


#
#### Person dependent PCA ####
pca.dependent.accuracy <- table(1:length(pca.dependent_folds))
pca.dependent.time.pca <- table(1:length(pca.dependent_folds))
pca.dependent.time.ann <- table(1:length(pca.dependent_folds))
pca.dependent.time.predict <- table(1:length(pca.dependent_folds))

for (i in 1:length(pca.dependent_folds)){
  #
  # Devide into training and test
  training_label <- as.factor(dataset[-pca.dependent_folds[[i]], 1])
  levels <- levels(training_label)
  start.time <- Sys.time()
  temp.training_set <- prcomp((dataset[-pca.dependent_folds[[i]], -1]), retx = TRUE, center = TRUE, scale. = TRUE)
  training_set <- temp.training_set$x
  test_label <- dataset[pca.dependent_folds[[i]], 1]
  test_set <- dataset[pca.dependent_folds[[i]], -1]
  temp.test_set <- prcomp(test_set, retx = TRUE, center = TRUE, scale. = TRUE)
  test_set <- predict(temp.test_set, test_set)
  pca.dependent.time.pca[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Define network training set
  temp.trainingClass <- matrix(nrow = length(training_label), ncol = 10, data = 0)
  
  # Prepare nn test set
  for (z in 1:length(training_label)) {
    matchList <- match(levels, toString(training_label[z]))
    matchList[is.na(matchList)] <- 0
    temp.trainingClass[z,] <- matchList
  }
  trainingClass <- as.data.frame(temp.trainingClass)
  
  #
  # Train neural network model
  start.time <- Sys.time()
  model <- mlp(x = training_set, y = trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
  pca.dependent.time.ann[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Prediction
  start.time <- Sys.time()
  prediction <- predict(model, newdata = test_set)
  pca.dependent.time.predict[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Calculate the accuracy of the predictions 
  responselist <- matrix(nrow = length(prediction[,1]), ncol = 1, data = "Na")
  for(z in 1:nrow(prediction)) {
    responselist[z, ] <- toString(which(prediction[z, ] == max(prediction[z, ])) - 1)
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculate accuracy
  agreement <- responselist[,1] == test_label
  tt <- prop.table(table(agreement))
  pca.dependent.accuracy[i] <- tt['TRUE']
}


#
#### Person independent no PCA ####
independent.accuracy <- table(1:length(independent_folds))
independent.time.ann <- table(1:length(independent_folds))
independent.time.predict <- table(1:length(independent_folds))

for (i in 1:length(independent_folds)){
  #
  # Devide into training and test
  training_label <- as.factor(dataset.shuffled[-independent_folds[[i]], 1])
  levels <- levels(training_label)
  training_set <- dataset.shuffled[-independent_folds[[i]], -1]
  test_label <- dataset.shuffled[independent_folds[[i]], 1]
  test_set <- dataset.shuffled[independent_folds[[i]], -1]
  
  #
  # Define network training set
  temp.trainingClass <- matrix(nrow = length(training_label), ncol = 10, data = 0)
  
  # Prepare nn test set
  for (z in 1:length(training_label)) {
    matchList <- match(levels, toString(training_label[z]))
    matchList[is.na(matchList)] <- 0
    temp.trainingClass[z,] <- matchList
  }
  trainingClass <- as.data.frame(temp.trainingClass)
  
  #
  # Train neural network model
  start.time <- Sys.time()
  model <- mlp(x = training_set, y = trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
  independent.time.ann[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Prediction
  start.time <- Sys.time()
  prediction <- predict(model, newdata = test_set)
  independent.time.predict[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Calculate the accuracy of the predictions 
  responselist <- matrix(nrow = length(prediction[,1]), ncol = 1, data = "Na")
  for(z in 1:nrow(prediction)) {
    responselist[z, ] <- toString(which(prediction[z, ] == max(prediction[z, ])) - 1)
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculate accuracy
  agreement <- responselist[,1] == test_label
  tt <- prop.table(table(agreement))
  independent.accuracy[i] <- tt['TRUE']
}


#
#### Person independent PCA ####
pca.independent.accuracy <- table(1:length(pca.independent_folds))
pca.independent.time.pca <- table(1:length(pca.independent_folds))
pca.independent.time.ann <- table(1:length(pca.independent_folds))
pca.independent.time.predict <- table(1:length(pca.independent_folds))

for (i in 1:length(pca.independent_folds)){
  #
  # Devide into training and test
  training_label <- as.factor(dataset.shuffled[-pca.independent_folds[[i]], 1])
  levels <- levels(training_label)
  start.time <- Sys.time()
  temp.training_set <- prcomp((dataset.shuffled[-pca.independent_folds[[i]], -1]), retx = TRUE, center = TRUE, scale. = TRUE)
  training_set <- temp.training_set$x
  test_label <- dataset.shuffled[pca.independent_folds[[i]], 1]
  test_set <- dataset.shuffled[pca.independent_folds[[i]], -1]
  temp.test_set <- prcomp(test_set, retx = TRUE, center = TRUE, scale. = TRUE)
  test_set <- predict(temp.test_set, test_set)
  pca.independent.time.pca[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Define network training set
  temp.trainingClass <- matrix(nrow = length(training_label), ncol = 10, data = 0)
  
  # Prepare nn test set
  for (z in 1:length(training_label)) {
    matchList <- match(levels, toString(training_label[z]))
    matchList[is.na(matchList)] <- 0
    temp.trainingClass[z,] <- matchList
  }
  trainingClass <- as.data.frame(temp.trainingClass)
  
  #
  # Train neural network model
  start.time <- Sys.time()
  model <- mlp(x = training_set, y = trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
  pca.independent.time.ann[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Prediction
  start.time <- Sys.time()
  prediction <- predict(model, newdata = test_set)
  pca.dependent.time.predict[i] <- difftime(Sys.time(), start.time, units = "secs")
  
  #
  # Calculate the accuracy of the predictions 
  responselist <- matrix(nrow = length(prediction[,1]), ncol = 1, data = "Na")
  for(z in 1:nrow(prediction)) {
    responselist[z, ] <- toString(which(prediction[z, ] == max(prediction[z, ])) - 1)
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculate accuracy
  agreement <- responselist[,1] == test_label
  tt <- prop.table(table(agreement))
  pca.independent.accuracy[i] <- tt['TRUE']
}
