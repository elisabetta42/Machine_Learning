#This script aim is to load the dataset that will then be used for PCA
# Requires

require(gmodels)
require(caret)
require(class)
#source('C:/Users/Christian/Documents/GitHub/Machine_Learning/FINAL_PROJECT/loadImage.R')
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/loadImage.R')
#source('Desktop/ML/Machine_Learning/EXERCISE_4/loadImage.R')
#folder <- "Desktop/ML/trunk/2017/group"
folder <- "C:/Users/Christian Arentsen/Documents/SML/2017/group"
#folder <- 'C:/Users/Christian/Documents/Statistical Machine Learning/2017/group'
#folder <- 'C:/Users/Christian/Documents/Statistical Machine Learning/preProcessed/2017/group'

getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}

dataList <- list(
  list(1,2)#,3)#, #0
  #list(1)#,2,3)#,  #1
  #list(1,2,3), #2
  #list(1,2,3,4), #3
  #list(1,2,3) #4
  #list(), #5
  #list(), #6
  #list(1,2), #7
  #list(1), #8 #omitted member 2 and 3, as member 2 did not do it correctly
  #list(), #9
  #list(1), #10
  #list(1,2), #11
  #list() #12 #omitted member 1, did not have corners.txt
)

