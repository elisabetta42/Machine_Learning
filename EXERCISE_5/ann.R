require(caret)
require(RSNNS)
require(kernlab)
require(caret)
#what you can set numbers of tree and parameters
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_5/load_dataset.R')
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_5/plotConfusion')


####### ARTIFICIAL NEURAL NETWORK ########
# Define nn training set

trainlabels <- as.factor(dataset[1:4000,1])
trainingset <- dataset[1:4000,-1]
trainlevels <- levels(trainlabels)
nntrainingClass <- matrix(nrow = length(trainlabels), ncol = 10, data = 0)

#Define test set
test <- dataset[4001:8000,-1]
testlabels <- dataset[4001:8000,1]

#Prepare nn test set.
for (i in 1:length(trainlabels)) {
  matchList <- match(trainlevels, toString(trainlabels[i]))
  matchList[is.na(matchList)] <- 0
  nntrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nntrainingClass)

# each entry represents a hidden layer, the value the number of neurons
size = c(20, 20, 20)

model <- mlp(x = trainingset, y = trainingClass, size = size, maxit = 600, learnFunc = "Std_Backpropagation", learnFuncParams = c(0.04, 0))
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


########### SUPPORT VECTOR MACHINE ########
ciphers_train <- dataset[1:4000, -1]
ciphers_train_label <- as.factor(dataset[1:4000, 1])
ciphers_test <- dataset[4001:8000, -1]
ciphers_test_label <- as.factor(dataset[4001:8000, 1])

# create SVM model
svm_model <- ksvm(ciphers_train_label~ ., data = ciphers_train, kernel = "rbfdot", C = 1)

svm_prediction <- predict(svm_model, ciphers_test, type = "response")
svm_agreement <- svm_prediction == ciphers_test_label 

#print results
table(svm_agreement)
prop.table(table(svm_agreement))

draw_confusion_matrix(confusion)
