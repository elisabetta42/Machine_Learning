#perform pca - person independent
per_indep<-new_shuffle
n_d<-new_shuffle[,2:length(dataset[1,])]
p_i_pca <- prcomp(n_d, retx=TRUE, center=TRUE, scale=TRUE)
#use the predict method to translate the original data in the new coordinates system
labels<- new_shuffle[,1]
per_indep[,1]<-labels
per_indep[,2:length(dataset[1,])] <- predict(p_i_pca, n_d)


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
}v