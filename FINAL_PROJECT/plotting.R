#### Plotting ####

#### Crossvalidation ####
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


#### Single persons ####
x2list <- c("Single Person", "PCA Single Person")
y2list.acc <- c(mean(list.accuracy), mean(list.accuracy.pca))
y2list.acc.sd <- c(sd(list.accuracy), sd(list.accuracy.pca))

y2list.time <- c(mean(list.time.ann), mean(list.time.ann.pca))
y2list.time <- y2list.time
y2list.time.sd <- c(sd(list.time.ann), sd(list.time.ann.pca))

y2df.acc <- data.frame(ClassificationTypes = x2list, Accuracy = y2list.acc, lower = (y2list.acc - y2list.acc.sd), upper = (y2list.acc + y2list.acc.sd))
y2df.time <- data.frame(ClassificationTypes = x2list, Time = y2list.time, lower = (y2list.time - y2list.time.sd), upper = (y2list.time + y2list.time.sd))

ggplot() + 
  geom_bar(data = y2df.acc, mapping = aes(x=ClassificationTypes, y=Accuracy), stat = "identity", fill = "grey") +
  geom_errorbar(data = y2df.acc, mapping=aes(x=ClassificationTypes, ymin=upper, ymax=lower), width = 0.2, size=1, color="blue") + 
  ggtitle("Mean Accuracy for Single Persons")

ggplot() + 
  geom_bar(data = y2df.time, mapping = aes(x=ClassificationTypes, y=Time), stat = "identity", fill = "grey") +
  geom_errorbar(data = y2df.time, mapping=aes(x=ClassificationTypes, ymin=upper, ymax=lower), width = 0.2, size=1, color="blue") + 
  ggtitle("Mean Time in seconds for Single Persons")



