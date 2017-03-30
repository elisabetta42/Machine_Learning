#Exercise 3.2
#source('Desktop/ML/EXERCISE_3/loadImage.R')
#source('Desktop/ML/EXERCISE_3/load_dataset.R') #loading dataset for two persons
#Ex. 3.2.1 Show a low level dendrogram containing 5 instances of each digit ( one or two person ).
#Let's start by creating a table with first five instances for every digits (one person)
person <- dataset[1:4000,]
instances=50 #number of raws for the new table, now 5 digits instance for one person
count=0;
index=0
cipher<-matrix(nrow = instances, ncol = length(person[1,]))
  
  for(i in 1:instances){ #5 instances * 10 digits = 50
    count=count+1
      if(count==5){
      index=index+395
      print(index)
        for(j in 1:5){
          for(col in 1:325){
            print(col)
          cipher[(i+j)-5,col]<-person[index+j,col]
          }
        }
     count=0
      }
  }

#cipher is the table containing the 5 instances for every index

require(graphics)
hc <- hclust(dist(cipher), "ave")
trainlabel=cipher[,1 ]
plot(hc,labels=trainlabel)

#Use K-Means clustering to compress each digit into 5 clusters and show a low level dendrogram
#of this ( two person ).
cluster_num=5 #use 5 clusters
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

hc <- hclust(dist(train_dat), "ave")
plot(hc,labels=train_lab)
