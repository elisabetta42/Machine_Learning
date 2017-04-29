require(randomForest)
require(caret)
#what you can set numbers of tree and parameters
source('C:/Users/Christian/Documents/GitHub/Machine_Learning/EXERCISE_4/load_dataset.R')

#LOAD ERROR: TEST AND TRAIN IS THE SAME 

#Devide into training and test sets
labels <- dataset[1:4000, 1]
train <-dataset[1:(nrow(dataset)/2),-1]
trainlabels <- dataset[1:(nrow(dataset)/2),1]
test <- dataset[(nrow(dataset)/2+1):(nrow(dataset)),-1]
testlabels <- dataset[(nrow(dataset)/2+1):(nrow(dataset)),1]
set.seed(300)

#for one run of random forest
forest_model <- randomForest(train, as.factor(trainlabels), ntree = 500, mtry = sqrt(324))
forest_prediction <- predict(forest_model, test, type="response")
confusion<-confusionMatrix(forest_prediction, as.factor(testlabels))
acc <- confusion$overall['Accuracy']

## RANDOM FOREST 10 FOLD CROSS VALIDATION ##
# define 10 folds
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# by default random forest will use sqrt(324) features pr. tree
# well tests with 18 features, half that. double that and so forth
variables <- c(9, seq(18, 324, length.out = 18))
grid_rf <- expand.grid(.mtry = variables)
m_rf <- train(as.factor(trainlabels) ~ ., data = train, method = "rf", metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf)
