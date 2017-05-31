#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_persons_modified.R')
#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/createDatasets.R')
#source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/kmeans.R')

source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/load_persons_modified.R')
source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/createDatasets.R')
source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/kmeans.R')
source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/person_dependent.R')
#source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/single_person.R')


require(gmodels)
require(caret)
require(class)
require(factoextra)

# RUNNING STUFF 

#load dataset for all 33 persons
dataset<-loadYearData(100,2017)

# single person
single_person<-dataset #store dataset separately
single_person<-create_pca_dataset(single_person)
single_person<-single_person[,1:22] #PCA dataset
person_index<-seq(0, length(dataset[,1]),4000) #create a sequence to run one person per time
length<-length(person_index)-1
s_result<-vector('list')
for(j in 1:length){
  
  lower<-person_index[j]+1
  individual_person<-single_person[lower:person_index[j+1],] #dataset for one person
  individual_person<-individual_person[sample(nrow(individual_person)),] #shuffling
  
  # divide between train and test
  individual_person_test<-individual_person[1:(nrow(individual_person)/2),]
  individual_person_train<-individual_person[(nrow(individual_person)/2+1):(nrow(individual_person)),]
  
  #store result
  sp_result<-cross_val_pca(individual_person_train,individual_person_test,1)
  s_result[[j]]<-sp_result
}

#person dependent - Data from all persons with a training set that includes data from all persons
# PCA PARAMETERS
person_dependent<-dataset
#pca_parameters<-create_p_d_pca_object(dataset) #create object pca to run method

# CREATE PCA DATASET for person dependent - create the value table
person_dependent<-person_dependent[sample(nrow(person_dependent)),] #shuff
pca<-create_pca_dataset(person_dependent)

# TUNE PCA PARAMETERS WITH (80% - 90% - 95% - 99%)
#knn_cross_val<-knn_with_pca(pca[1:12000],pca_parameters) #tune knn parameters uncomment

# TUNE PARAMETERS FOR THE CLUSTER
#k=c(10,20,50,70,80,100,200,300,400)# ,90,100,200,300,400,500,600,700,800,900,1000)#number of clusters
k=c(450)
pers_dep<-pca[,1:22]

fold_index<-createsequence(0, length(dataset[,1]),round(length(dataset[,1])/100*10))
result<-vector('list')


for(c in 1:length(k)){
  pd_training<-pers_dep[-(fold_index[c]:fold_index[c+1]),]
  pd_training<-pd_training[order(pd_training[,1]),]
  amountofEachCipher=length(which(pd_training[,1]==0))
  pers_dep_test<-pers_dep[fold_index[c]:fold_index[c+1],]
  # create the dataset with a predefined number of clusters
  cluster_knn<-kMeansClusterPerformIncludeSpilt(pd_training,k[c],amountofEachCipher)
  pers_dep_test<-pers_dep_test[sample(nrow(pers_dep_test)),]
  
  labels=cluster_knn$labels
  cluster=cluster_knn$centers
  pca_cluster_table<-cbind(labels,cluster)
  
  pca_cluster_table<-pca_cluster_table[sample(nrow(pca_cluster_table)),]
  # tune the cluster parameters
 
  cluster_acc<-cross_val_pca(pca_cluster_table,pers_dep_test,10)
  result[[c]]<-cluster_acc
}

#person independent - Data from all persons where the training set does not include data from the
                      #persons in the test set

# CREATE PCA DATASET for person independent 
person_independent<-dataset
pi_pca<-create_pca_dataset(person_independent)

# TUNE PCA PARAMETERS WITH (80% - 90% - 95% - 99%)
#knn_cross_val<-knn_with_pca(pi_pca[1:12000],pca_parameters) #tune knn parameters with 3 persons

# TUNE PARAMETERS FOR THE CLUSTER
k=c(10,20,50,70,80,100,200,300,400) #,90,100,200,300,400,500,600,700,800,900,1000)#number of clusters
pers_indep<-pi_pca[,1:22] #fix this
pers_indep_test<-pers_indep

#arrange table per digits for clustering

pi_result<-vector('list')
higher<-nrow(dataset)-(3*4000)
fold_index<-createsequence(0,higher ,12000)

for(c in 1:length(k)){
  pi_test_dataset<-pers_indep_test[fold_index[c]:fold_index[c+1],]
  pi_training<- pers_indep_test[-(fold_index[c]:fold_index[c+1]),]
  pi_training<-pi_training[order(pi_training[,1]),]
  
  amountofEachCipher=length(which(pi_training[,1]==0))
  
  # create the dataset with a predefined number of clusters
  pi_cluster_knn<-kMeansClusterPerformIncludeSpilt(pi_training,k[c],amountofEachCipher)
  
  pi_labels=pi_cluster_knn$labels
  pi_cluster=pi_cluster_knn$centers
  
  pi_pca_cluster_table<-cbind(pi_labels,pi_cluster)
  pi_pca_cluster_table<-pi_pca_cluster_table[sample(nrow(pi_pca_cluster_table)),]
  
  # tune the cluster parameters
  pi_cluster_acc<-cross_val_pca(pi_pca_cluster_table,pi_test_dataset,10)
  pi_result[[c]]<-pi_cluster_acc
}





