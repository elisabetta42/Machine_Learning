# Requires
require(gmodels)
require(caret)
require(class)

## Exercise 1.3.4

#source('D:/Dropbox/SDU/8. Semester/Statistical Machine Learning/L1/loadImage.R')
#directory <- "D:/Documents/Statistical Machine Learning/2017/group"

source('C:/Users/Christian/Dropbox/SDU/8. Semester/Statistical Machine Learning/L1/loadImage.R')
source('C:/Users/Christian/Dropbox/SDU/8. Semester/Statistical Machine Learning/L1/multiplot.R')
directory <- "C:/Users/Christian/Documents/Statistical Machine Learning/2017/group"

dataset <- loadSinglePersonsData(100, 1, 1, directory)
dataset <- data.frame(dataset)

## Exercise 1.4.1

# set seed for repoduct and shuffle (seed takes randomly from list)
set.seed(423)
dataset_shuffle <- dataset[sample(nrow(dataset)),]

# define classification labels for dataset
classification_table <- dataset_shuffle[,2:length(dataset_shuffle[1,])]
training_label <- dataset_shuffle[1:2000,1]
test_label <- dataset_shuffle[2000:4000,1]

# divide into classification sets: 50/50 training testing
training_class <- classification_table[1:2000,]
test_class <- classification_table[2000:4000,]

# run model with k = 40, use timer;
start.time <- Sys.time();
test_pred <- knn(train = training_class, test = test_class, 
                 cl = training_label, k = 1);
time.taken <- Sys.time() - start.time;

# evaluate performance;
prop.chisq = FALSE;
# CrossTable(x = test_label, y = test_pred, prop.chisq =  FALSE);

# create confusion matrix;
confusion <- confusionMatrix(test_pred, test_label);
# get accuracy from matrix;
accuracy <- confusion$overall['Accuracy'];

#print results to conclude exercise 1.4.1
print(paste0("Acc for 5050: ", accuracy))
print(paste0("Time for 5050: ",time.taken))

training_label2 <- dataset_shuffle[1:3430,1];
training_class2 <- dataset_shuffle[1:3430,-1];
test_label2 <- dataset_shuffle[3430:4000,1];
test_class2 <- dataset_shuffle[3430:4000,-1];

start.time2 <- Sys.time();
test_pred2 <- knn(train = training_class2, test = test_class2,
                  cl = training_label2, k = 1);
time.taken2 <- Sys.time() - start.time2;

confusion2 <- confusionMatrix(test_pred2, test_label2);
accuracy2 <- confusion2$overall['Accuracy'];

print(paste0("Acc for 7030: ", accuracy2))
print(paste0("Time for 7030: ", time.taken2))



## Exercise 1.4.2

var_k <-c(1, 5, 10, 15, 20, 30, 40, 50, 100, 150)
var_k_acc <- list(1:length(var_k))
var_k_time <- list(1:length(var_k))

for (i in 1:length(var_k)){
  
  start.time <- Sys.time()
  # running knn with k = var_k(i)
  test_pred <- knn(train = training_class, test = test_class, 
                   cl = training_label, k = var_k[i])
  time.taken <- Sys.time() - start.time
  
  #create confusion matrix and add accuracy to list
  confusion <- confusionMatrix(test_pred, test_label)
  var_k_acc[i] <- confusion$overall['Accuracy']
  var_k_time[i] <- time.taken
}

# print the time and accuracy of the varying k
for (i in 1:length(var_k)){
  print(paste0("k: " , var_k[i]))
  print(paste0("Acc: " , var_k_acc[i]))
  print(paste0("Time: ", var_k_time[i]))
}

## Exercise 1.4.3

folds <- createFolds(dataset_shuffle$X1, k = 10)
cv_k_acc <- list(1:length(folds))
cv_k_time <- list(1:length(folds))
cv_overall_time <- Sys.time()
for (i in 1:length(folds)){
  # classification table
  cv_class_table <- dataset_shuffle[,2:length(dataset_shuffle[1,])]
  cv_training_label <- dataset_shuffle[-folds[[i]],1]
  cv_test_label <- dataset_shuffle[folds[[i]],1]
  
  # 50/50 classification/training test
  cv_training_class <- cv_class_table[-folds[[i]],]
  cv_test_class <- cv_class_table[folds[[i]],]
  
  # take time and run knn
  start.time <- Sys.time()
  cv_pred <- knn(train = cv_training_class, test = cv_test_class, 
                   cl = cv_training_label, k = 1)
  time.taken <- Sys.time() - start.time
  
  # calculate confusion matrix
  confusion <- confusionMatrix(cv_pred, cv_test_label)

  # get accuracy and time for i run
  cv_k_acc[i] <- confusion$overall['Accuracy']
  cv_k_time[i] <- time.taken
}
cv_overall_time_taken <- Sys.time() - cv_overall_time
#print results to conclude exercise 1.4.1
print("Results for 10 fold")
print(paste0("Mean acc: " , mean(unlist(cv_k_acc))))
print(paste0("Mean time: ", mean(unlist(cv_k_time))))
print(paste0("Total time: ", cv_overall_time_taken))

plot1 <- qplot(unlist(var_k), unlist(var_k_acc), geom = "line") + ggtitle("Accuracy for increasing k")
plot2 <- qplot(unlist(var_k), unlist(var_k_time), geom = "line")+ ggtitle("Classification time for increasing k")
multiplot(plot1, plot2)
#
# Exercise 1.4.4
#

smoothImage <- function(grayImg){
  kernel <- makeBrush(3, shape='Gaussian', step=TRUE, sigma=0.55)
  print(kernel)
  smoothed <- filter2(grayImg, kernel)
  return(smoothed)
}

dataset <- loadSinglePersonsData(100, 1, 1, directory)
dataset <- data.frame(dataset)
dataset_shuffle <- dataset[sample(nrow(dataset)),]

folds <- createFolds(dataset_shuffle$X1, k = 10)
sm_k_acc <- list(1:length(folds))
sm_k_time <- list(1:length(folds))
sm_overall_time <- Sys.time()
for (i in 1:length(folds)){
  # classification table
  sm_class_table <- dataset_shuffle[,2:length(dataset_shuffle[1,])]
  sm_training_label <- dataset_shuffle[-folds[[i]],1]
  sm_test_label <- dataset_shuffle[folds[[i]],1]
  
  # 50/50 classification/training test
  sm_training_class <- sm_class_table[-folds[[i]],]
  sm_test_class <- sm_class_table[folds[[i]],]
  
  # take time and run knn
  start.time <- Sys.time()
  sm_pred <- knn(train = sm_training_class, test = sm_test_class, 
                 cl = sm_training_label, k = 1)
  time.taken <- Sys.time() - start.time
  
  # calculate confusion matrix
  confusion <- confusionMatrix(cv_pred, cv_test_label)
  
  # get accuracy and time for i run
  sm_k_acc[i] <- confusion$overall['Accuracy']
  sm_k_time[i] <- time.taken
}
sm_overall_time_taken <- Sys.time() - cv_overall_time
#print results to conclude exercise 1.4.4
print("Results for 10 fold, different smoothing")
print(paste0("Mean acc: " , mean(unlist(sm_k_acc))))
print(paste0("Mean time: ", mean(unlist(sm_k_time))))
print(paste0("Total time: ", sm_overall_time_taken))

print(paste0("Acc difference: ", mean(unlist(cv_k_acc))-mean(unlist(sm_k_acc))))
print(paste0("First filter best? ",mean(unlist(cv_k_acc)) > mean(unlist(sm_k_acc)) ))
print(paste0("Time difference: ", cv_overall_time_taken-sm_overall_time_taken))
print(paste0("First filter fastest? ", cv_overall_time_taken < sm_overall_time_taken))

