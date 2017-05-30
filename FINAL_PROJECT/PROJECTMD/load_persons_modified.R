library('lattice')
source('C:/Users/Christian Arentsen/Git/Machine_Learning/FINAL_PROJECT/PROJECTMD/loadImage.R')


loadPersonData <- function(dpi, year, groupNr, personNr) {
  year = paste(c("C:/Users/Christian Arentsen/Documents/SML/preProcessed/", year))
  groupNr = paste(c("/group", groupNr))
  personNr = as.character(personNr)
  
  imgData <- loadSinglePersonsData(dpi, groupNr, personNr, year)
  imgData = as.data.frame(imgData)
  
  return(imgData)
}

loadGroupData <- function(dpi, year, groupNr) {
  if(year == 2017) {
    if(groupNr == 0 | groupNr == 1 | groupNr == 2 | groupNr == 5  | groupNr==6) {
      #groups with 3 members
      imgData = loadPersonData(dpi, year, groupNr, 1)
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 2))
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 3))
      imgData = as.data.frame(imgData)
      return(imgData)
    } #else if(groupNr == {#groups with 2 members
      #imgData = loadPersonData(dpi, year, groupNr, 2)
      #imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 1))
      #imgData = as.data.frame(imgData)
      #return(imgData)
     else if(groupNr == 9 | groupNr == 10 | groupNr == 12) {
      #groups with 1 member
      imgData = loadPersonData(dpi, year, groupNr, 1)
      imgData = as.data.frame(imgData)
      return(imgData)
    } else if(groupNr == 7 | groupNr==8) {
      #groups with 2 members
      imgData = loadPersonData(dpi, year, groupNr, 1)
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 2))
      imgData = as.data.frame(imgData)
      return(imgData)
    } else if(groupNr == 3) {
      #groups with 4 members
      imgData = loadPersonData(dpi, year, groupNr, 1)
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 2))
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 3))
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 4))
      imgData = as.data.frame(imgData)
      return(imgData)
    } else if(groupNr == 4) {
      #groups with 4 members whose memberNr start from zero.
      imgData = loadPersonData(dpi, year, groupNr, 0)
      # Missing file for 200dpi
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 1))
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 2))
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 3))
      imgData = as.data.frame(imgData)
      return(imgData)
    } else if(groupNr == 11) {
      #groups with 3 members whose memberNr start from zero.
      imgData = loadPersonData(dpi, year, groupNr, 0)
      # -missing corners
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 1))
      imgData = rbind(imgData, loadPersonData(dpi, year, groupNr, 2))
      imgData = as.data.frame(imgData)
      return(imgData)
    }
  } else if(year == 2016) {
    return(NA)
  } else if(year == 2015) {
    return(NA)
  } else {
    return(NA)
  }
}

loadYearData <- function(dpi, year) {
  if(year == 2017) {
    imgData = loadGroupData(dpi, year, 0)
    imgData = rbind(imgData, loadGroupData(dpi, year, 1))
    imgData = rbind(imgData, loadGroupData(dpi, year, 2))
    imgData = rbind(imgData, loadGroupData(dpi, year, 3))
    imgData = rbind(imgData, loadGroupData(dpi, year, 4))
    imgData = rbind(imgData, loadGroupData(dpi, year, 5))
    imgData = rbind(imgData, loadGroupData(dpi, year, 6))
    imgData = rbind(imgData, loadGroupData(dpi, year, 7))
    imgData = rbind(imgData, loadGroupData(dpi, year, 8))
    imgData = rbind(imgData, loadGroupData(dpi, year, 9))
    imgData = rbind(imgData, loadGroupData(dpi, year, 10))
    imgData = rbind(imgData, loadGroupData(dpi, year, 11))
    imgData = rbind(imgData, loadGroupData(dpi, year, 12))
    imgData = as.data.frame(imgData)
    return(imgData)
  } else if (year == 2016) {
    return(NA)
  } else if (year == 2015) {
    return(NA)
  } else {
    return(NA)
  }
}

loadAll <- function(dpi) {
  imgData = loadYearData(dpi, 2017)
  #imgData = rbind(imgData, loadYearData(dpi, 2016))
  #imgData = rbind(imgData, loadYearData(dpi, 2015))
  
  return(imgData)
}
