#Exercise 3.3
#source('Desktop/ML/EXERCISE_3/loadImage.R')
#source('Desktop/ML/EXERCISE_3/load_dataset.R')
#source('D:/Documents/GitHub/Machine_Learning/EXERCISE_3/loadImage.R')
#source('D:/Documents/GitHub/Machine_Learning/EXERCISE_3/load_dataset.R')

train<-dataset[1:(nrow(dataset)/2),-1]
test<-dataset[4001:nrow(dataset),-1]
train_lab<-dataset[1:(nrow(dataset)/2),1]
test_lab<-dataset[4001:nrow(dataset),1]
var_k <- c(1:13)
max<-0
plot(NULL,NULL,xlim = c(0,0.8), ylim = c(0,0.6), main="Precision / recall plot for knn \nwith k = 1-13 and l = 1-k", 
     xlab="Average precision", ylab="Average recall")
pred_test<- c() # matrix(nrow = length(var_k), ncol = length(var_k))
max_f1<-c()
for (i in 1:length(var_k)) {
  list_precision<- c() 
  list_recall<-c()
  list_f1 <- c()
  for (j in 1:i) {
    pred_test<- knn(train = train, test = test, 
                    cl = train_lab, k = var_k[i], l=j)
    matrix <- confusionMatrix(pred_test, test_lab)
    list_precision[j] <- (sum(diag(matrix$table)) / sum(matrix$table))
    list_recall[j] <-(sum(diag(matrix$table)) / 4000)
    list_f1[j]<-2*(( list_precision[j]*list_recall[j])/(list_precision[j]+list_recall[j]))
  }
  max_f1[i]<-max(list_f1)
  lines(list_precision,list_recall, pch = i, type="o")
  #print(list_recall)
  #print(max_f1)
} 

plot(var_k, max_f1, main="f1 score for each k", xlab="k values", ylab="f1 score",  type="o")
