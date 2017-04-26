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
steps = 30
cuts <- seq(min(pca$x[, j]), max(pca$x[, j]), length.out = steps)
inf_gain_table <- vector(mode="numeric", length=length(cuts))
first_pca <-  pca$x[, j]

for (i in 1:length(cuts)) {
  t = cuts[i]
  below_range <- first_pca < t
  above_range <- first_pca >= t
  before_entropy <- table(labels) / length(labels)
  
  #calculate the after entropy for the elements below the threshold
  
  pro <- table(labels) / length(labels)
  before_entropy <- sum(-pro * log(pro))
  pro <-
    table(labels[below_range]) / length(labels[below_range]) #probability table for every digit
  after_entropy_below <- sum(-pro * log(pro))
  
  pro <-
    table(labels[above_range]) / length(labels[above_range]) #probability table for every digit
  after_entropy_above <- sum(-pro * log(pro))
  
  after_entropy = (length(below_range) * after_entropy_below) / length(labels) + (length(above_range) *
                                                                                    after_entropy_below) / length(labels)
  
  information_gain = after_entropy - before_entropy
  
  inf_gain_table[i] <- information_gain
}

plot(inf_gain_table)
}

