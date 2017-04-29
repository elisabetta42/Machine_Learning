#source('Desktop/ML/Machine_Learning/EXERCISE_4/exercise_4.R') #load the whole dataset
#source('Desktop/ML/Machine_Learning/EXERCISE_4/load_dataset.R') #load the whole dataset
require(C50)
require(rpart)
require(gmodels)
require(partykit)
require(rpart.plot) 
require(caret) 

require(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#single person
labels <- dataset[1:4000, 1]
train <-dataset[1:(nrow(dataset)/2),-1]
trainlabels <- dataset[1:(nrow(dataset)/2),1]

test <- dataset[(nrow(dataset)/2+1):(nrow(dataset)),-1]
testlabels <- dataset[(nrow(dataset)/2+1):(nrow(dataset)),1]
fit <- C5.0(train, as.factor(trainlabels))
summary(fit)
pred <- predict(fit, test)

PC1<-pca$x[, 1]
PC2<-pca$x[, 2]
PC3<-pca$x[, 3]
PC4<-pca$x[, 4]
PC5<-pca$x[, 5]
pca_plot_table<-cbind(labels, PC1, PC2, PC3, PC4, PC5)
confusionmat<-confusionMatrix(pred,testlabels)
accuracy<-confusionmat$overall["Accuracy"]
model<-rpart(labels ~ PC1 + PC2 + PC3 + PC4 + PC5, method="class")
rpart.plot(model)
#plot tree with 5 pca and compare it with information gain
#train using person independent and dependent and observe validation
#use rpart to create object
#and plot it using rpart

########################################################################
var_k <-c(1:10)
accuracies  <- list(1:length(var_k))
var_k_time <- list(1:length(var_k))
for (i in 1:length(var_k)){
#run it with person independent for all 16 persons - 50% train / 50% test - 10 cross-validation

person_independent<-dataset

p_i_train_set<-person_independent[1:(nrow(dataset)/2),-1]
p_i_train_labels<-person_independent[1:(nrow(dataset)/2),1]

p_i_test_set<-person_independent[((nrow(dataset)/2)+1):nrow(dataset),-1]
p_i_test_labels<-person_independent[((nrow(dataset)/2)+1):nrow(dataset),1]

start.time <- Sys.time()
p_i_fit <- C5.0(p_i_train_set, as.factor(p_i_train_labels))
time.taken <- Sys.time() - start.time #difftime
var_k_time[i] <- time.taken

p_i_pred <- predict(p_i_fit, p_i_test_set)
confusionmat<-confusionMatrix(p_i_pred,p_i_test_labels)
accuracies[i] <- confusionmat$overall['Accuracy']
}

plot1 <- qplot(unlist(var_k), unlist(accuracies), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for k number of experiment")
plot2 <- qplot(unlist(var_k), unlist(var_k_time), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for k number of experiment")
multiplot(plot1, plot2)

  
#run it with person dependent for all 16 persons 
person_dependent <- dataset[sample(nrow(dataset)),]
