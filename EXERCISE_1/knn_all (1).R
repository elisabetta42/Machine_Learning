# Requires
require(gmodels)
require(caret)
require(class)

#source('D:/Dropbox/SDU/8. Semester/Statistical Machine Learning/L1/loadImage.R')
source('C:/Users/Christian/Dropbox/SDU/8. Semester/Statistical Machine Learning/L1/loadImage.R')

# Example code for reading all images into a list, DPI 100
getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}
# Set a "list of list" for each group and member and run
#folder <- "D:/Documents/Statistical Machine Learning/2017/group"
folder <- "C:/Users/Christian/Documents/Statistical Machine Learning/2017/group"

dataList <- list(
  list(1,2,3), #0
  list(1,2,3),  #1
  list(1,2,3), #2
  list(1,2,3,4), #3
  list(1,2,3) #4
  #list(), #5
  #list(), #6
  #list(1,2), #7
  #list(1), #8 #omitted member 2 and 3, as member 2 did not do it correctly
  #list(), #9
  #list(1), #10
  #list(1,2), #11
  #list() #12 #omitted member 1, did not have corners.txt
  )
idList <- getAllData(dataList)

# You can now iterate trough the list
dataset <- idList[1]
dataset <- data.frame(dataset)

for(i in 2:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  dataset <- rbind(dataset, idTemp)
}
# you can combine the data frames using "id <- rbind(id, idTemp)"
set.seed(423)

## Running knn without shuffling

# define classification labels for dataset
classification_table <- dataset[,2:length(dataset[1,])]
training_label <- dataset[1:(nrow(dataset)/2),1]
test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset),1]

# divide into classification sets: 50/50 training testing
training_class <- classification_table[1:(nrow(dataset)/2),]
test_class <- classification_table[(nrow(dataset)/2+1):(nrow(dataset)),]

# run model with k = 40, use timer
start.time <- Sys.time()
test_pred <- knn(train = training_class, test = test_class, 
                 cl = training_label, k = 1)
time.taken <- Sys.time() - start.time

# create confusion matrix
confusion <- confusionMatrix(test_pred, test_label)
# get accuracy from matrix
accuracy <- confusion$overall['Accuracy']

print(paste0("Acc for no no shuffle: ", accuracy))
print(paste0("Time for no shuffle: ", time.taken))


## Doing the same with shuffled dataset

dataset_shuffle <- dataset[sample(nrow(dataset)),]

# define classification labels for dataset
sh_classification_table <- dataset_shuffle[,2:length(dataset_shuffle[1,])]
sh_training_label <- dataset_shuffle[1:(nrow(dataset_shuffle)/2),1]
sh_test_label <- dataset_shuffle[((nrow(dataset_shuffle)/2)+1):nrow(dataset_shuffle),1]

# divide into classification sets: 50/50 training testing
sh_training_class <- sh_classification_table[1:(nrow(dataset_shuffle)/2),]
sh_test_class <- sh_classification_table[(nrow(dataset_shuffle)/2+1):(nrow(dataset_shuffle)),]

# run model with k = 40, use timer
sh_start.time <- Sys.time()
sh_test_pred <- knn(train = sh_training_class, test = sh_test_class, 
                 cl = sh_training_label, k = 1)
sh_time.taken <- Sys.time() - sh_start.time

# create confusion matrix
sh_confusion <- confusionMatrix(sh_test_pred, sh_test_label)
# get accuracy from matrix
sh_accuracy <- sh_confusion$overall['Accuracy']

#print results to conclude exercise 1.4.1
print(paste0("Acc for shuffle: ", sh_accuracy))
print(paste0("Time taken for shuffle: ", sh_time.taken))




## exercise 1.4.6
# We hypothesise that splitting the dataset in half, without shuflling, for training and test, 
# the shuffle and split the training set in half, we reduce the training time without losing much accuracy.

# divide 50/50 into training test
hfs_training_set <- dataset[1:(nrow(dataset)/2),]
hfs_test_set <- dataset[((nrow(dataset)/2)+1):nrow(dataset),]
# shuffle testset
hfs_training_set <- hfs_training_set[sample(nrow(hfs_training_set)),]

# define labels and devide into classes
# only take half of training set
hfs_training_label <- hfs_training_set[1:(nrow(hfs_training_set)/2),1]
hfs_training_class <- hfs_training_set[1:(nrow(hfs_training_set)/2),-1]
hfs_test_label <- hfs_test_set[,1]
hfs_test_class <- hfs_test_set[,-1]

# run model with k = 1, use timer
hfs_start.time <- Sys.time()
hfs_test_pred <- knn(train = hfs_training_class, test = hfs_test_class, 
                 cl = hfs_training_label, k = 1)
hfs_time.taken <- Sys.time() - hfs_start.time

# create confusion matrix
hfs_confusion <- confusionMatrix(hfs_test_pred, hfs_test_label)
# get accuracy from matrix
hfs_accuracy <- hfs_confusion$overall['Accuracy']

print(paste0("Acc for half shuffle: ", hfs_accuracy))
print(paste0("Time for half shuffle: ", hfs_time.taken))


