# Principal Component Analysis (PCA) -
#EXERCISE 2.1
require(gmodels)
require(caret)
require(class)
require(factoextra)
source('Desktop/ML/EXERCISE_2/loadImage.R')
source('Desktop/ML/EXERCISE_2/load_dataset.R') #load the whole dataset
options(scipen=999)  #disable scientific notation 

#Perform a PCA on the data for the person dependent and the person
#independent data set. 

#STEP 1 performs pca on the whole dataset - except for the first line containing classifications
new_data <-dataset #used for person dependent
new_shuffle <- dataset[sample(nrow(dataset)),] #used for person independent

#perform pca - person dependent
n_d<-dataset[,2:length(dataset[1,])]
pd_pca <- prcomp(n_d, retx=TRUE, center=TRUE, scale=TRUE)
#use the predict method to translate the original data in the new coordinates system
labels<- dataset[,1]
new_data[,1]<-labels
new_data[,2:length(dataset[1,])] <- predict(pd_pca, n_d)
#new_data<-append (c(labels), new_data)
#new_data <- data.matrix(new_data)
pers_dep<-new_data


#perform pca - person independent
per_indep<-new_shuffle
n_d<-new_shuffle[,2:length(dataset[1,])]
p_i_pca <- prcomp(n_d, retx=TRUE, center=TRUE, scale=TRUE)
#use the predict method to translate the original data in the new coordinates system
labels<- new_shuffle[,1]
per_indep[,1]<-labels
per_indep[,2:length(dataset[1,])] <- predict(p_i_pca, n_d)
#new_data<-append (c(labels), new_data)
#per_indep <- data.matrix(per_indep)

#new_data contains the pca version of the data

#EXERCISE 1.1 - pca person dependent - pca person independent
## Running pca without shuffling - person dependent

## Running pca with shuffling - person independent
#pers_indep -- 

#remove useless stuff
remove(idTemp)
remove(n_d)
remove(new_data)
remove(new_shuffle)
remove(labels)
remove(idList)
remove(dataList)

# EXERCISE 1.1 pers_dep and pers_indep contains the needed parts
#show the eigenvalues - variance - and accumulated variance of pca 

#Person Dependent
#get parameters we need - eigenvalue, variance, cumulative variance - have also manual code
eig.val <- get_eigenvalue(pd_pca)

#plot the variance of the first 10 principal components 
parameter=1 #1 for eigen_value, 2 for variance, 3 for cumulative variance
title=c("eigenvalues","variances","cumulative variance")
images=c('Desktop/ML/EXERCISE_2/images/pd_eigenvalues.jpg','Desktop/ML/EXERCISE_2/images/pd_variances.jpg','Desktop/ML/EXERCISE_2/images/pd_cumulative_variance.jpg')
for(i in 1:3){
jpeg(file=images[i])
num_pca=10 #number of principal components to show (10-20) following the assignment specification
barplot(eig.val[1:num_pca, i], names.arg=1:num_pca, 
        main = title[i],
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), 
      eig.val[,i ], 
      type="b", pch=19, col = "red")
dev.off()
}

#person independent
p_i_eig.val <- get_eigenvalue(p_i_pca)

#plot the variance of the first 10 principal components 
parameter=1 #1 for eigen_value, 2 for variance, 3 for cumulative variance
title=c("eigenvalues","variances","cumulative variance")
images=c('Desktop/ML/EXERCISE_2/images/pi_eigenvalues.jpg','Desktop/ML/EXERCISE_2/images/pi_variances.jpg','Desktop/ML/EXERCISE_2/images/pi_cumulative_variance.jpg')
for(i in 1:3){
  jpeg(file=images[i])
  num_pca=10 #number of principal components to show (10-20) following the assignment specification
  barplot(p_i_eig.val[1:num_pca, i], names.arg=1:num_pca, 
          main = title[i],
          xlab = "Principal Components",
          ylab = "Percentage of variances",
          col ="steelblue")
  # Add connected line segments to the plot
  lines(x = 1:nrow(p_i_eig.val), 
        p_i_eig.val[,i ], 
        type="b", pch=19, col = "red")
  dev.off()

}

#EXERCISE 1.2 Show the performance of selecting enough principal components to
#represent 80%, 90%, 95%, 99% of the accumulated variance. For each
#test vary K.
#Person Dependent

p_d_max <- matrix(0, nrow=1, ncol = 4)
cv<-eig.val[,3] #save cumulative variance vector
perc<-c(80, 90, 95, 99) #percentage to represent
for(j in 1:length(p_d_max)){
  for(i in 2:length(cv)){
    if(cv[i]>=perc[j]){
      p_d_max[j]<-i
      break
    } 
  }
} 
#EXERCISE 1.2 Show the performance of selecting enough principal components to
#represent 80%, 90%, 95%, 99% of the accumulated variance. For each
#test vary K.
#Person Independent

p_i_max <- matrix(0, nrow=1, ncol = 4)
cv<-p_i_eig.val[,3] #save cumulative variance vector
perc<-c(80, 90, 95, 99) #percentage to represent
for(j in 1:length(p_i_max)){
  for(i in 2:length(cv)){
    if(cv[i]>=perc[j]){
      p_i_max[j]<-i
      break
    } 
  }
} 









