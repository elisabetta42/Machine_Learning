#### ANN FINAL ####

# Imports
require(caret)
require(RSNNS)
require(gmodels)
require(ggplot2)
require(class)
require(factoextra)
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_5/load_dataset.R')
# Import the function to plot neural networks from Github
# Not sure we are going to use it though. Takes a shit ton of time!
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

# Define datasets, person dependent and person independent
nn.person_dep <- dataset
nn.pca.person_dep <- prcomp(nn.person_dep, retx = TRUE, center = TRUE, scale = TRUE)



nn.person_indep <- dataset[sample(nrow(dataset)),]
nn.pca.person_indep <- prcomp(nn.person_indep, retx = TRUE, center = TRUE, scale = TRUE)

# Define training and test sets. 
# Labels must be factors for nn to classify.

trainlabels <- as.factor(dataset[1:4000,1])
trainingset <- dataset[1:4000,-1]
trainlevels <- levels(trainlabels)
nntrainingClass <- matrix(nrow = length(trainlabels), ncol = 10, data = 0)

# Define test set
test <- dataset[4001:8000,-1]
testlabels <- dataset[4001:8000,1]

# Prepare nn test set.
for (i in 1:length(trainlabels)) {
  matchList <- match(trainlevels, toString(trainlabels[i]))
  matchList[is.na(matchList)] <- 0
  nntrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nntrainingClass)

# each entry represents a hidden layer, the value the number of neurons
size = c(20)#, 20)#, 20)

#measure time
time.start <- Sys.time()
#train model
model <- mlp(x = trainingset, y = trainingClass, size = size, maxit = 600, learnFunc = "Std_Backpropagation", learnFuncParams = c(0.045, 0))
finished.time <- Sys.time() - time.start
plotIterativeError(model)

predictions <- predict(model, newdata = test)

#Using the "predict" function we have recieved "predictions"
responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
for(i in 1:nrow(predictions)) {
  responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
}
responselist <- data.frame(responselist)
responselist[,1] <- as.factor(responselist[,1])
# Calculating the accuracy
agreement_rbf <- responselist[,1] == testlabels
table(agreement_rbf)
prop.table(table(agreement_rbf))

#plot.nnet(model)
