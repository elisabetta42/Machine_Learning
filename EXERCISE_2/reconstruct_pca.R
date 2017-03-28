#Exercise 2.3: Reconstruction using PCA
require(factoextra)
#In this exercise I only take into account data of one person with 300 DPI for better precision when plotting
#pca eigenvalues

# 1. This task is about reconstructing data using PCA. First using these
#functions we can plot an image of a single cipher:

#load data for one person using 300 DPI
cipher_set <- loadSinglePersonsData(300, 3, 3, 'Desktop/ML/trunk/2017/group')
#Plot every cipher once and save them as jpeg
images=c(
         'Desktop/ML/EXERCISE_2/images/c_zero.jpg',
         'Desktop/ML/EXERCISE_2/images/c_one.jpg',
         'Desktop/ML/EXERCISE_2/images/c_two.jpg',
         'Desktop/ML/EXERCISE_2/images/c_three.jpg',
         'Desktop/ML/EXERCISE_2/images/c_four.jpg',
         'Desktop/ML/EXERCISE_2/images/c_five.jpg',
         'Desktop/ML/EXERCISE_2/images/c_six.jpg',
         'Desktop/ML/EXERCISE_2/images/c_seven.jpg',
         'Desktop/ML/EXERCISE_2/images/c_eigth.jpg',
         'Desktop/ML/EXERCISE_2/images/c_nine.jpg'
        
)

for (i in 1:length(index)){
  jpeg(file=images[i])
  id<-cipher_set[i+(399*i),]
  id<-matrix(id)
  id <- t(id)
  imageSize <- sqrt(ncol(id) - 1)
  imageM <- matrix( id[1,2:ncol(id)],nrow = imageSize,ncol
                    = imageSize,byrow = FALSE)
  rotate <- function(x) t(apply(x, 2, rev))
  imageM <- rotate(imageM) # rotate is a function to rotate the image
  imageM <- matrix( unlist(imageM), nrow=length(imageM[1,]) )
  image( imageM )
  dev.off()
}

#2 Try plotting the first 10 eigenvectors/loadingvectors as images. Can you
#describe what you see?
#PCA
#cipher_set <- loadSinglePersonsData(300, 3, 3, 'Desktop/ML/trunk/2017/group')
n_d<-cipher_set[,2:length(cipher_set[1,])]
pca_table<-prcomp(n_d, center=TRUE, scale=FALSE)

#
#Try plotting the first 10 eigenvectors/loadingvectors as images. Can you
#describe what you see?
#

eig_images=c(
  'Desktop/ML/EXERCISE_2/images/eig_one.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_two.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_three.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_four.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_five.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_six.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_seven.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_eigth.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_nine.jpg',
  'Desktop/ML/EXERCISE_2/images/eig_ten.jpg'
)

for(i in 1:10){
  id<-pca_table$rotation[,i]
  jpeg(file=eig_images[i])
  imageSize <- sqrt(length(id))
  imageM <- matrix( id,nrow = imageSize,ncol
                    = imageSize,byrow = FALSE)
  rotate <- function(x) t(apply(x, 2, rev))
  imageM <- rotate(imageM) # rotate is a function to rotate the image
  imageM <- matrix( unlist(imageM), nrow=length(imageM[1,]) )
  image( imageM )
  dev.off()
}

#
# Try plotting a reconstruction of the images you displayed in 3.1 using all
#PC’s. This can be done by multiplying the loadings with the scores and
#adding the removed centering.
#

pca_images=c(
  'Desktop/ML/EXERCISE_2/images/pca_zero.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_one.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_two.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_three.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_four.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_five.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_six.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_seven.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_eigth.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_nine.jpg'
  
)

for(z in 1:10){
  jpeg(file=pca_images[z])
trunc <- pca_table$x[z+399*z,] %*%#1:nrow(pca_table$rotation)
  t(pca_table$rotation) #nrow(pca_table$rotation)
trunc <- scale(trunc, center = -1 * pca_table$center, scale=FALSE)
id<-trunc
imageSize <- sqrt(length(id))
imageM <- matrix( id,nrow = imageSize,ncol
                  = imageSize,byrow = FALSE)
rotate <- function(x) t(apply(x, 2, rev))
imageM <- rotate(imageM) # rotate is a function to rotate the image
imageM <- matrix( unlist(imageM), nrow=length(imageM[1,]) )
eig.val <- get_eigenvalue(pca_table)
cumvar<-eig.val[,3]
print(z+399*z)
image( imageM )
dev.off();
}



#Now try re-recreating using 80% of variance, 90% and 95%.
#Using 80% variance

eig.val<-get_eigenvalue(pca_table)
cum_var<-eig.val[3]

index_variances<-c(17,29,43)

###########################################################################
#Reconstruct 80% of the cum variance
###########################################################################
pca_ciphers_80=c(
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_zero.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_one.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_two.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_three.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_four.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_five.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_six.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_seven.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_eigth.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_80_nine.jpg'
  
)

for(f in 1:10){
  jpeg(f=pca_ciphers_80[f])
cipherNumber<- f+399*f
trunc <- pca_table$x[cipherNumber,1:index_variances[1]] %*%#1:nrow(pca_table$rotation)
   t(pca_table$rotation[,1:index_variances[1]]) #nrow(pca_table$rotation)
  trunc <- scale(trunc, center = -1 * pca_table$center, scale=FALSE)
  id<-trunc
  imageSize <- sqrt(length(id))
  imageM <- matrix( id,nrow = imageSize,ncol
                    = imageSize,byrow = FALSE)
  rotate <- function(x) t(apply(x, 2, rev))
  imageM <- rotate(imageM) # rotate is a function to rotate the image
  imageM <- matrix( unlist(imageM), nrow=length(imageM[1,]) )
  image( imageM )
  dev.off()
}


###########################################################################
#Reconstruct 90% of the cum variance
###########################################################################
pca_ciphers_90=c(
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_zero.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_one.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_two.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_three.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_four.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_five.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_six.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_seven.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_eigth.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_90_nine.jpg'
  
)

for(f in 1:10){
  jpeg(f=pca_ciphers_90[f])
  cipherNumber<- f+399*f
  trunc <- pca_table$x[cipherNumber,1:index_variances[2]] %*%#1:nrow(pca_table$rotation)
    t(pca_table$rotation[,1:index_variances[2]]) #nrow(pca_table$rotation)
  trunc <- scale(trunc, center = -1 * pca_table$center, scale=FALSE)
  id<-trunc
  imageSize <- sqrt(length(id))
  imageM <- matrix( id,nrow = imageSize,ncol
                    = imageSize,byrow = FALSE)
  rotate <- function(x) t(apply(x, 2, rev))
  imageM <- rotate(imageM) # rotate is a function to rotate the image
  imageM <- matrix( unlist(imageM), nrow=length(imageM[1,]) )
  image( imageM )
  dev.off()
}

###########################################################################
#Reconstruct 95% of the cum variance
###########################################################################
pca_ciphers_95=c(
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_zero.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_one.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_two.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_three.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_four.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_five.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_six.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_seven.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_eigth.jpg',
  'Desktop/ML/EXERCISE_2/images/pca_ciphers_95_nine.jpg'
  
)

for(f in 1:10){
  jpeg(f=pca_ciphers_95[f])
  cipherNumber<- f+399*f
  trunc <- pca_table$x[cipherNumber,1:index_variances[3]] %*%#1:nrow(pca_table$rotation)
    t(pca_table$rotation[,1:index_variances[3]]) #nrow(pca_table$rotation)
  trunc <- scale(trunc, center = -1 * pca_table$center, scale=FALSE)
  id<-trunc
  imageSize <- sqrt(length(id))
  imageM <- matrix( id,nrow = imageSize,ncol
                    = imageSize,byrow = FALSE)
  rotate <- function(x) t(apply(x, 2, rev))
  imageM <- rotate(imageM) # rotate is a function to rotate the image
  imageM <- matrix( unlist(imageM), nrow=length(imageM[1,]) )
  image( imageM )
  dev.off()
}



#Lastly take two different ciphers and compare the 10 first scores and see
#if you can see a difference. 

first_cipher<-pca_table$x[801,1:10]
second_cipher<-pca_table$x[2000,1:10]

print(first_cipher)
print(second_cipher)
##Try also this were you take the mean for all
#400 of these ciphers and compare the first 10 scores.
first<-pca_table$x[800:1200,]
whole_first_cipher<-first[,1:10]

second<-pca_table$x[1600:2000,]
whole_second_cipher<-second[,1:10]


m_first_cipher<-first_cipher
m_second_cipher<-second_cipher

for(i in 1:10){
  m_first_cipher[i]<-mean(whole_first_cipher[,i])
  m_second_cipher[i]<-mean(whole_second_cipher[,i])
}

print(m_first_cipher)
print(m_second_cipher)


