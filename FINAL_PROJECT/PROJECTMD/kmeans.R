library('stats')

kMeansClusterPerform <- function(data,k)
{
  return( kmeans(data, k))
}

kMeansClusterPerformIncludeSpilt <- function(data,k, amountOfEachCipher)
{
  amountOfCiphers = amountOfEachCipher*10
  amountOfPerons = nrow(data) / amountOfCiphers
  
  Cipher0 = data[1:(amountOfEachCipher*amountOfPerons),2:ncol(data)]
  Cipher1 = data[(amountOfEachCipher*amountOfPerons+1):(amountOfEachCipher*amountOfPerons*2),2:ncol(data)]
  Cipher2 = data[(amountOfEachCipher*amountOfPerons*2+1):(amountOfEachCipher*amountOfPerons*3),2:ncol(data)]
  Cipher3 = data[(amountOfEachCipher*amountOfPerons*3+1):(amountOfEachCipher*amountOfPerons*4),2:ncol(data)]
  Cipher4 = data[(amountOfEachCipher*amountOfPerons*4+1):(amountOfEachCipher*amountOfPerons*5),2:ncol(data)]
  Cipher5 = data[(amountOfEachCipher*amountOfPerons*5+1):(amountOfEachCipher*amountOfPerons*6),2:ncol(data)]
  Cipher6 = data[(amountOfEachCipher*amountOfPerons*6+1):(amountOfEachCipher*amountOfPerons*7),2:ncol(data)]
  Cipher7 = data[(amountOfEachCipher*amountOfPerons*7+1):(amountOfEachCipher*amountOfPerons*8),2:ncol(data)]
  Cipher8 = data[(amountOfEachCipher*amountOfPerons*8+1):(amountOfEachCipher*amountOfPerons*9),2:ncol(data)]
  Cipher9 = data[(amountOfEachCipher*amountOfPerons*9+1):(amountOfEachCipher*amountOfPerons*10),2:ncol(data)]
  
  centers = kMeansClusterPerform(Cipher0,k)$centers
  centers = rbind(centers,kMeansClusterPerform(Cipher1,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher2,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher3,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher4,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher5,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher6,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher7,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher8,k)$centers)
  centers = rbind(centers,kMeansClusterPerform(Cipher9,k)$centers)
  
  labels = c()
  for(centroidIndex in 0:9)
  {
    for(cluster in 1:k)
    {
      labels = rbind(labels, centroidIndex)
    }
  }
  
  return(structure(list(centers = centers, labels = labels)))
}