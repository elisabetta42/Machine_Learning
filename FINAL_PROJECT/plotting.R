#### Plotting ####
#names
xlist <- c("Person Dependent", "PCA Person Dependent", "Person Independent", "PCA Person Independent")
#Accuracy
ylist.acc <- c(mean(dependent.accuracy), mean(pca.dependent.accuracy), mean(independent.accuracy), mean(pca.independent.accuracy)) 
ylist.acc.sd <- c(sd(dependent.accuracy), sd(pca.dependent.accuracy), sd(independent.accuracy), sd(pca.independent.accuracy)) 
#Time
ylist.time <- c(mean(dependent.time.ann), mean(pca.dependent.time.ann), mean(independent.time.ann), mean(pca.independent.time.ann))
ylist.time <- ylist.time/60
ylist.time.sd <- c(sd(dependent.time.ann/60), sd(pca.dependent.time.ann/60), sd(independent.time.ann/60), sd(pca.independent.time.ann/60))

#Accuracy as dataframe
ydf.acc <- data.frame(ClassificationTypes = xlist, Accuracy = ylist.acc, lower = (ylist.acc - ylist.acc.sd), upper = (ylist.acc + ylist.acc.sd))
ydf.time <- data.frame(ClassificationTypes = xlist, Time = ylist.time, lower = (ylist.time - ylist.time.sd), upper = (ylist.time + ylist.time.sd))

ggplot() + 
  geom_bar(data = ydf.acc, mapping = aes(x=ClassificationTypes, y=Accuracy), stat = "identity", fill = "grey") +
  geom_errorbar(data = ydf.acc, mapping=aes(x=ClassificationTypes, ymin=upper, ymax=lower), width = 0.2, size=1, color="blue") + 
  ggtitle("Mean Accuracy for Classification Types")

ggplot() + 
  geom_bar(data = ydf.time, mapping = aes(x=ClassificationTypes, y=Time), stat = "identity", fill = "grey") +
  geom_errorbar(data = ydf.time, mapping=aes(x=ClassificationTypes, ymin=upper, ymax=lower), width = 0.2, size=1, color="blue") + 
  ggtitle("Mean Time in minutes for Classification Types")
