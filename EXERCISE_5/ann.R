require(caret)
require(RSNNS)
#what you can set numbers of tree and parameters
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_5/load_dataset.R')

# Define nn training set
trainlabels <- dataset[1:4000,1]
trainlevels <- levels(trainlabels)
nntrainingClass <- matrix(nrow = length(trainlevels), ncol = 10, data = 0)

for (i in 1:length(trainlevels)) {
  matchList <- match(trainlevels, toString(trainlabels[i]))
  matchList[is.na(matchList)] <- 0
  nntrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nntrainingClass)





test <- dataset[4001:8000,-1]
testlabels <- dataset[4001:8000,1]
set.seed(300)