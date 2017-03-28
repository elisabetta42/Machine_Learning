
#source('Desktop/ML/EXERCISE_2/final_ex_2.R') #download table with pca
require(gmodels)
require(ggplot2)

########################################################################
## Running knn for person dependent - Exercise 2.1 - subpoints 1.2 - 1.3
########################################################################
var_k <-c(1,10)
var_k_acc  <- list(1:(length(var_k)*length(p_d_max)*2))
var_k_time <- list(1:(length(var_k)*length(p_d_max)*2))

count=0
for(i in 1:length(p_d_max)){
  dataset <- pers_dep[,1:p_d_max[i]]  
  # define classification labels for dataset
  classification_table <- dataset[,2:length(dataset[1,])]
  training_label <- dataset[1:(nrow(dataset)/2),1]
  test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset),1]

  # divide into classification sets: 50/50 training testing
  training_class <- classification_table[1:(nrow(dataset)/2),]
  test_class <- classification_table[(nrow(dataset)/2+1):(nrow(dataset)),]

## Exercise 1.3
  for (j in 1:length(var_k)){
  count=count+1
  #print(length(dataset))
  start.time <- Sys.time()
  #running knn with k = var_k(i)
  test_pred <- knn(train = training_class, test = test_class, 
                cl = training_label, k = var_k[j])
  time.taken <- Sys.time() - start.time #difftime
  
  #create confusion matrix and add accuracy to list
  confusion <- confusionMatrix(test_pred, test_label)
  var_k_acc[count] <- confusion$overall['Accuracy']
  var_k_time[count] <- time.taken
  print(count)
  }  
}
#print the time and accuracy of the varying k

###################################################################################
#person independent
###################################################################################


for(i in 1:length(p_i_max)){
  dataset <- per_indep[,1:p_i_max[i]]  
  # define classification labels for dataset
  classification_table <- dataset[,2:length(dataset[1,])]
  training_label <- dataset[1:(nrow(dataset)/2),1]
  test_label <- dataset[((nrow(dataset)/2)+1):nrow(dataset),1]
  
  # divide into classification sets: 50/50 training testing
  training_class <- classification_table[1:(nrow(dataset)/2),]
  test_class <- classification_table[(nrow(dataset)/2+1):(nrow(dataset)),]
  
  ## Exercise 1.3
  for (j in 1:length(var_k)){
    count=count+1
    #print(length(dataset))
    start.time <- Sys.time()
    #running knn with k = var_k(i)
    test_pred <- knn(train = training_class, test = test_class, 
                     cl = training_label, k = var_k[j])
    time.taken <- Sys.time() - start.time #difftime
    
    #create confusion matrix and add accuracy to list
    confusion <- confusionMatrix(test_pred, test_label)
    var_k_acc[count] <- confusion$overall['Accuracy']
    var_k_time[count] <- time.taken
    print(count)
  }  
}
#print the time and accuracy of the varying k
for (i in 1:count){
  #print(paste0("k: " , var_k[i]))
  print(paste0("Acc: " , var_k_acc[i]))
  print(paste0("Time: ", var_k_time[i]))
}  

#plot accuracy/ks
#a<-1:length(var_k_acc)
#a <- a[seq(1, length(var_k_acc), length(var_k))]
#for (i in 1:(round(length(var_k_acc)/length(var_k))))
#{
#  print(i)
# plot1 <- qplot(unlist(var_k), unlist(var_k_acc[a[i]:a[i]+(length(var_k)-1)]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
# plot2 <- qplot(unlist(var_k), unlist(var_k_time[a[i]:a[i]+(length(var_k)-1)]), geom = "line",xlab="k values",ylab="accuracies")+ ggtitle("Classification time for increasing k")
# multiplot(plot1, plot2)
 
#}


