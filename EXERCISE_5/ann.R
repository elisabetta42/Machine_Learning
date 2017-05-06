require(caret)
require(RSNNS)
require(kernlab)
require(caret)
#what you can set numbers of tree and parameters
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_5/load_dataset.R')
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_5/plotConfusion')
#import the function from Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')



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
########### SUPPORT VECTOR MACHINE ########
ciphers_train <- dataset[1:4000, -1]
ciphers_train_label <- as.factor(dataset[1:4000, 1])
ciphers_test <- dataset[4001:8000, -1]
ciphers_test_label <- as.factor(dataset[4001:8000, 1])

#measure time
time.start2 <- Sys.time()
# create SVM model
svm_model <- ksvm(ciphers_train_label~ ., data = ciphers_train, kernel = "rbfdot", C = 1)
finished.time2 <- Sys.time() - time.start2

svm_prediction <- predict(svm_model, ciphers_test, type = "response")
svm_agreement <- svm_prediction == ciphers_test_label 

#print results
table(svm_agreement)
prop.table(table(svm_agreement))

confusion <- confusionMatrix(svm_prediction, ciphers_test_label)
svm_df <- as.data.frame(confusion$table)
svm_plot <- ggplot(svm_df, aes(x = Reference, y = Prediction)) 
svm_plot + geom_tile(aes(fill = Freq), colour = "white")

