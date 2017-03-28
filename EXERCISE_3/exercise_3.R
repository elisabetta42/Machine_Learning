#source('Desktop/ML/EXERCISE_3/loadImage.R')
#source('Desktop/ML/EXERCISE_3/load_dataset.R')
#the data for two person training are contained in dataset

#3.1.1 Try to improve the performance of two person training data ( person independent ). Perform Kmeans
#clustering of each cipher individually for the training set, in order to represent the training data
#as a number of cluster centroids. Now perform the training of the KNN using the centroids of these
#clusters. You can try with different cluster sizes and see the resulting performance.

k=c(1,10,20,50,100,200,300,350)#number of clusters
#run the code with the different numbers of clusters
for(cluster_num in k){
  
id<-dataset[1:4000,] #If “id” is the dataset for the first person, whom the training is performed on.
cipher_cluster <- c()
label_cluster <- c()
for( i in 0:9) {
  clusterData <- kmeans(id[ id[,1] == i, -1 ], cluster_num)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:cluster_num)*0 + i
}
train_lab <- factor(unlist(label_cluster))
train_dat <- cipher_cluster[[1]]
for( i in 2:10) {
  train_dat <- rbind(train_dat,cipher_cluster[[i]])
}
#try to do something meaningful for testing using the second person as pure test, using the original data
id<-dataset[4001:8000,] 
test_lab<-id[,1]
test_dat<-id[,-1]
#Then train_lab and train_dat can be used as input for KNN.
#perform knn
set.seed(423)

# run model with k = 1, use timer;
start.time <- Sys.time();
test_pred <- knn(train = train_dat, 
                 cl = train_lab,test=test_dat ,k = 1);
time.taken <- Sys.time() - start.time

# evaluate performance;
prop.chisq = FALSE;
# CrossTable(x = test_label, y = test_pred, prop.chisq =  FALSE);

# create confusion matrix;
confusion <- confusionMatrix(test_pred, test_lab);
# get accuracy from matrix;
accuracy <- confusion$overall['Accuracy'];

#print results to conclude exercise 1.4.1
print(paste0("Clusters Number: ", cluster_num))
print(paste0("Acc: ", accuracy))
print(paste0("Time: ",time.taken))
}



