#### ANN FINAL ####

# Imports
require(caret)
require(RSNNS)
require(gmodels)
require(ggplot2)
require(class)
require(factoextra)
#source('C:/Users/Christian/Documents/GitHub/Machine_Learning/FINAL_PROJECT/load_dataset.R')
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/load_dataset.R')
# Import the function to plot neural networks from Github
# Not sure we are going to use it though. Takes a shit ton of time!
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#Dataset split is 50% training / 50% Test
# Define datasets - person dependent with and without pca

# Split into training and test
# Training lables must be as factors to be converted into levels
nn.person_dep.training_label <- as.factor(dataset[1:(nrow(dataset)/2), 1])
nn.person_dep.levels <- levels(nn.person_dep.training_label)
nn.person_dep.training_set <- dataset[1:(nrow(dataset)/2), -1]
nn.person_dep.test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset), 1]
nn.person_dep.test_set <- dataset[((nrow(dataset)/2)+1):nrow(dataset), -1]

nn.pca.person_dep.pca.time.start <- Sys.time()
nn.pca.person_dep.training_label <- as.factor(dataset[1:(nrow(dataset)/2), 1])
nn.pca.person_dep.levels <- levels(nn.pca.person_dep.training_label)
nn.pca.person_dep.temp.training_set <- dataset[1:(nrow(dataset)/2), -1]
nn.pca.person_dep.temp.pca_train <- prcomp(nn.pca.person_dep.temp.training_set, retx = TRUE, center = TRUE, scale = TRUE)
nn.pca.person_dep.training_set <- predict(nn.pca.person_dep.temp.pca_train, nn.pca.person_dep.temp.training_set)
nn.pca.person_dep.test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset), 1]
nn.pca.person_dep.temp.test_set <- dataset[((nrow(dataset)/2)+1):nrow(dataset), -1]
nn.pca.person_dep.temp.pca_test <- prcomp(nn.pca.person_dep.temp.test_set, retx = TRUE, center = TRUE, scale = TRUE)
nn.pca.person_dep.test_set <- predict(nn.pca.person_dep.temp.pca_test, nn.pca.person_dep.temp.test_set)
nn.pca.person_dep.pca.time.finish <- Sys.time() - nn.pca.person_dep.pca.time.start
# Cleanup
rm(nn.pca.person_dep.temp.training_set, nn.pca.person_dep.temp.pca_train, nn.pca.person_dep.temp.test_set, nn.pca.person_dep.temp.pca_test)

# Define datasets - person independet with and without pca
dataset.shuffled <- dataset[sample(nrow(dataset)),]

# Split into training and test
# Training lables must be as factors to be converted into levels
nn.person_indep.training_label <- as.factor(dataset.shuffled[1:(nrow(dataset.shuffled)/2), 1])
nn.person_indep.levels <- levels(nn.person_indep.training_label)
nn.person_indep.training_set <- dataset.shuffled[1:(nrow(dataset.shuffled)/2), -1]
nn.person_indep.test_label <- dataset.shuffled[((nrow(dataset.shuffled)/2)+1):nrow(dataset.shuffled), 1]
nn.person_indep.test_set <- dataset.shuffled[((nrow(dataset.shuffled)/2)+1):nrow(dataset.shuffled), -1]

nn.pca.person_indep.pca.time.start <- Sys.time()
nn.pca.person_indep.training_label <- as.factor(dataset.shuffled[1:(nrow(dataset.shuffled)/2), 1])
nn.pca.person_indep.levels <- levels(nn.pca.person_indep.training_label)
nn.pca.person_indep.temp.training_set <- dataset.shuffled[1:(nrow(dataset.shuffled)/2), -1]
nn.pca.person_indep.temp.pca_train <- prcomp(nn.pca.person_indep.temp.training_set, retx = TRUE, center = TRUE, scale = TRUE)
nn.pca.person_indep.training_set <- predict(nn.pca.person_indep.temp.pca_train, nn.pca.person_indep.temp.training_set)
nn.pca.person_indep.test_label <- dataset.shuffled[((nrow(dataset.shuffled)/2)+1):nrow(dataset.shuffled), 1]
nn.pca.person_indep.temp.test_set <- dataset.shuffled[((nrow(dataset.shuffled)/2)+1):nrow(dataset.shuffled), -1]
nn.pca.person_indep.temp.pca_test <- prcomp(nn.pca.person_indep.temp.test_set, retx = TRUE, center = TRUE, scale = TRUE)
nn.pca.person_indep.test_set <- predict(nn.pca.person_indep.temp.pca_test, nn.pca.person_indep.temp.test_set)
nn.pca.person_indep.pca.time.finish <- Sys.time() - nn.pca.person_indep.pca.time.start
# Cleanup
rm(nn.pca.person_indep.temp.training_set, nn.pca.person_indep.temp.pca_train, nn.pca.person_indep.temp.test_set, nn.pca.person_indep.temp.pca_test)

# Define nerual network training sets
#
# Person dependent, no PCA
nn.person_dep.temp.trainingClass <- matrix(nrow = length(nn.person_dep.training_label), ncol = 10, data = 0)

# Prepare nn test set
for (i in 1:length(nn.person_dep.training_label)) {
  matchList <- match(nn.person_dep.levels, toString(nn.person_dep.training_label[i]))
  matchList[is.na(matchList)] <- 0
  nn.person_dep.temp.trainingClass[i,] <- matchList
}
nn.person_dep.trainingClass <- as.data.frame(nn.person_dep.temp.trainingClass)

#
# Person dependent, PCA
nn.pca.person_dep.temp.trainingClass <- matrix(nrow = length(nn.pca.person_dep.training_label), ncol = 10, data = 0)

# Prepare nn test set
for (i in 1:length(nn.pca.person_dep.training_label)) {
  matchList <- match(nn.pca.person_dep.levels, toString(nn.pca.person_dep.training_label[i]))
  matchList[is.na(matchList)] <- 0
  nn.pca.person_dep.temp.trainingClass[i,] <- matchList
}
nn.pca.person_dep.trainingClass <- as.data.frame(nn.pca.person_dep.temp.trainingClass)

#
# Person independent, no PCA
nn.person_indep.temp.trainingClass <- matrix(nrow = length(nn.person_indep.training_label), ncol = 10, data = 0)

# Prepare nn test set
for (i in 1:length(nn.person_indep.training_label)) {
  matchList <- match(nn.person_indep.levels, toString(nn.person_indep.training_label[i]))
  matchList[is.na(matchList)] <- 0
  nn.person_indep.temp.trainingClass[i,] <- matchList
}
nn.person_indep.trainingClass <- as.data.frame(nn.person_indep.temp.trainingClass)

#
# Person independent, PCA
nn.pca.person_indep.temp.trainingClass <- matrix(nrow = length(nn.pca.person_indep.training_label), ncol = 10, data = 0)

# Prepare nn test set
for (i in 1:length(nn.pca.person_indep.training_label)) {
  matchList <- match(nn.pca.person_indep.levels, toString(nn.pca.person_indep.training_label[i]))
  matchList[is.na(matchList)] <- 0
  nn.pca.person_indep.temp.trainingClass[i,] <- matchList
}
nn.pca.person_indep.trainingClass <- as.data.frame(nn.pca.person_indep.temp.trainingClass)


# Define network size. This was the 'best' result from our previous exercises.
# Each entry represents a hidden layer, the value the number of neurons
networkSize = c(20) # ex c(#nodes, #nodes) = two layers
networkMaxEpochs = 600
networkLearningFunc = "Std_Backpropagation"
networkLearningFuncParam = c(0.045, 0)

#
# Train neural network model

# Person dependent, no PCA
nn.person_dep.time.start <- Sys.time()
nn.person_dep.model <- mlp(x = nn.person_dep.training_set, y = nn.person_dep.trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
nn.person_dep.time.finished <- Sys.time() - nn.person_dep.time.start

# Person dependent, PCA

nn.pca.person_dep.time.start <- Sys.time()
nn.pca.person_dep.model <- mlp(x = nn.pca.person_dep.training_set, y = nn.pca.person_dep.trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
nn.pca.person_dep.time.finished <- Sys.time() - nn.pca.person_dep.time.start

# Person indepdendet, no PCA

nn.person_indep.time.start <- Sys.time()
nn.person_indep.model <- mlp(x = nn.person_indep.training_set, y = nn.person_indep.trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
nn.person_indep.time.finished <- Sys.time() - nn.person_indep.time.start

# Person independent, PCA

nn.pca.person_indep.time.start <- Sys.time()
nn.pca.person_indep.model <- mlp(x = nn.pca.person_indep.training_set, y = nn.pca.person_indep.trainingClass, size = networkSize, maxit = networkMaxEpochs, learnFunc = networkLearningFunc, learnFuncParams = networkLearningFuncParam)
nn.pca.person_indep.time.finished <- Sys.time() - nn.pca.person_indep.time.start


# TODO: PlotIterateiveError for all, with different colour lines etc.
#plotIterativeError(nn.person_dep.model)
#predictions <- predict(model, newdata = test)


## Predictions and timing
# Person dependent, no PCA
nn.person_dep.prediction.time.start <- Sys.time()
nn.person_dep.prediction <- predict(nn.person_dep.model, newdata = nn.person_dep.test_set)
nn.person_dep.prediction.time.finish <-  Sys.time() - nn.person_dep.prediction.time.start

# Person dependent, PCA
nn.pca.person_dep.prediction.time.start <- Sys.time()
nn.pca.person_dep.prediction <- predict(nn.pca.person_dep.model, newdata = nn.pca.person_dep.test_set)
nn.pca.person_dep.prediction.time.finish <- Sys.time() - nn.pca.person_dep.prediction.time.start

# Person independent, no PCA
nn.person_indep.prediction.time.start <- Sys.time()
nn.person_indep.prediction <- predict(nn.person_indep.model, newdata = nn.person_indep.test_set)
nn.person_indep.prediction.time.finish <- Sys.time() - nn.person_indep.prediction.time.start

# Person independent, PCA
nn.pca.person_indep.prediction.time.start <- Sys.time()
nn.pca.person_indep.prediction <- predict(nn.pca.person_indep.model, newdata = nn.pca.person_indep.test_set)
nn.pca.person_indep.prediction.time.finish <- Sys.time() - nn.pca.person_indep.prediction.time.start

#
# Calculate the accuracy of the predictions 
#
# Person dependent, no PCA
nn.person_dep.responselist <- matrix(nrow = length(nn.person_dep.prediction[,1]), ncol = 1, data = "Na")
for(i in 1:nrow(nn.person_dep.prediction)) {
  nn.person_dep.responselist[i, ] <- toString(which(nn.person_dep.prediction[i, ] == max(nn.person_dep.prediction[i, ])) - 1)
}
nn.person_dep.responselist <- data.frame(nn.person_dep.responselist)
nn.person_dep.responselist[,1] <- as.factor(nn.person_dep.responselist[,1])
# Calculate accuracy
nn.person_dep.agreement <- nn.person_dep.responselist[,1] == nn.person_dep.test_label
nn.person_dep.table <- table(nn.person_dep.agreement)
print("Person Dependet, no PCA: ")
prop.table(nn.person_dep.table)

#
# Person dependent, PCA
nn.pca.person_dep.responselist <- matrix(nrow = length(nn.pca.person_dep.prediction[,1]), ncol = 1, data = "Na")
for(i in 1:nrow(nn.pca.person_dep.prediction)) {
  nn.pca.person_dep.responselist[i, ] <- toString(which(nn.pca.person_dep.prediction[i, ] == max(nn.pca.person_dep.prediction[i, ])) - 1)
}
nn.pca.person_dep.responselist <- data.frame(nn.pca.person_dep.responselist)
nn.pca.person_dep.responselist[,1] <- as.factor(nn.pca.person_dep.responselist[,1])
# Calculate accuracy
nn.pca.person_dep.agreement <- nn.pca.person_dep.responselist[,1] == nn.pca.person_dep.test_label
nn.pca.person_dep.table <- table(nn.pca.person_dep.agreement)
print("Person Dependet, PCA: ")
prop.table(nn.pca.person_dep.table)

#
# Person independent, no PCA
nn.person_indep.responselist <- matrix(nrow = length(nn.person_indep.prediction[,1]), ncol = 1, data = "Na")
for(i in 1:nrow(nn.person_dep.prediction)) {
  nn.person_indep.responselist[i, ] <- toString(which(nn.person_indep.prediction[i, ] == max(nn.person_indep.prediction[i, ])) - 1)
}
nn.person_indep.responselist <- data.frame(nn.person_indep.responselist)
nn.person_indep.responselist[,1] <- as.factor(nn.person_indep.responselist[,1])
# Calculate accuracy
nn.person_indep.agreement <- nn.person_indep.responselist[,1] == nn.person_indep.test_label
nn.person_indep.table <- table(nn.person_indep.agreement)
print("Person Independet, no PCA: ")
prop.table(nn.person_indep.table)

#
# Person independent, PCA
nn.pca.person_indep.responselist <- matrix(nrow = length(nn.pca.person_indep.prediction[,1]), ncol = 1, data = "Na")
for(i in 1:nrow(nn.pca.person_dep.prediction)) {
  nn.pca.person_indep.responselist[i, ] <- toString(which(nn.pca.person_indep.prediction[i, ] == max(nn.pca.person_indep.prediction[i, ])) - 1)
}
nn.pca.person_indep.responselist <- data.frame(nn.pca.person_indep.responselist)
nn.pca.person_indep.responselist[,1] <- as.factor(nn.pca.person_indep.responselist[,1])
# Calculate accuracy
nn.pca.person_indep.agreement <- nn.pca.person_indep.responselist[,1] == nn.pca.person_indep.test_label
nn.pca.person_indep.table <- table(nn.pca.person_indep.agreement)
print("Person Independet, PCA: ")
prop.table(nn.pca.person_indep.table)

#Decide if we want to plot the models.
#plot.nnet(model)
