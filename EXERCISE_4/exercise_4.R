# Compute the optimal decision point for the first 5 PCAs and compute the information gain
#associated to it (plot 5 graphs, one for each component, and show the highest information gain).
#require(gmodels) #c5.o
#require(caret)
#require(class)
#require(C50)
#source('Desktop/ML/Machine_Learning/EXERCISE_4/load_dataset.R') #load the whole dataset
#Run pca and get first 5 best PCs
for (j in 1:5) {
pca_dataset <- dataset[1:4000, 2:length(dataset[1, ])]
labels <- dataset[1:4000, 1]
pca <- prcomp(pca_dataset,
              retx = TRUE,
              center = TRUE,
              scale = TRUE)

range_max <- max(pca$x[, j])
range_min <- min(pca$x[, j])
value_range <- range_max - range_min #have the range distance
#Take five first pca
steps = 20
cuts <- seq(min(pca$x[, j]), max(pca$x[, j]), length.out = steps)
inf_gain_table <- vector(mode="numeric", length=steps)
first_pca <-  pca$x[, j]

for (i in 1:length(cuts)) {
  t = cuts[i]
  below_range <- first_pca < t
  above_range <- first_pca >= t
  before_entropy <- table(labels) / length(labels)
  
  #calculate the after entropy for the elements below the threshold
  
  pro <- table(labels) / length(labels)
  before_entropy <- sum(-pro * log2(pro), na.rm =T)
  pro <-
    table(labels[below_range]) / length(labels[below_range]) #probability table for every digit
  after_entropy_below <- sum(-pro * log2(pro), na.rm =T)
  
  pro <-
    table(labels[above_range]) / length(labels[above_range]) #probability table for every digit
  after_entropy_above <- sum(-pro * log2(pro), na.rm =T)
  
  after_entropy =( (length(labels[below_range]) * after_entropy_below)  + 
                     (length(labels[above_range]) * after_entropy_above) ) / length(labels)
  
  information_gain =   before_entropy - after_entropy
  
  inf_gain_table[i] <- information_gain
}

plot(cuts,inf_gain_table)
lines(cuts,inf_gain_table)
}

