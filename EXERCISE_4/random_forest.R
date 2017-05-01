require(randomForest)
require(caret)
require(mlbench)
#what you can set numbers of tree and parameters
#source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_4/load_dataset.R')

#Devide into training and test sets
train <-dataset[1:4000,-1]
trainlabels <- dataset[1:4000,1]
test <- dataset[4001:8000,-1]
testlabels <- dataset[4001:8000,1]
set.seed(300)

#for one run of random forest
#forest_model <- randomForest(train, as.factor(trainlabels), ntree = 500, mtry = sqrt(324))
#forest_prediction <- predict(forest_model, test, type="response")
#confusion<-confusionMatrix(forest_prediction, as.factor(testlabels))
#acc <- confusion$overall['Accuracy']

## RANDOM FOREST 10 FOLD CROSS VALIDATION ##
# define 10 folds
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# tuning parameters
mtry <- c(9, 18, 144, 324)
tunegrid <- expand.grid(.mtry = mtry)

# train the random forest model
rf_default <- train(train, as.factor(trainlabels), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
prediction <- predict(rf_default, newdata = test)
confusion<-confusionMatrix(prediction, as.factor(testlabels))
accuracy <- confusion$overall['Accuracy']
trellis.par.set(caretTheme())
plot(rf_default, main = "Random Forest Cross Validation")   