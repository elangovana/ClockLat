source("./globals.R")
source("./InstallPackage.R")

pkgInstall("rworldmap")


library(sp)
library(rworldmap)

coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  return(indices) 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}




createContinents <- function(dataset){
  indices <-  coords2continent(dataset[, c(lon, lat)])
  
  print(head(indices$REGION))
  data<- cbind(dataset, indices$REGION)
  names(data)[names(data) == 'indices$REGION'] <- region
  
  
  return(data)
}


CleanInvalidData<- function(data){

  data[ which(data[, hour1] > 24 ), hour1 ] <- NA
  data[ which(data[, hour2] > 24 ), hour2 ] <- NA
  data[ which(data[, hour3] > 24 ), hour3 ] <- NA
   return(data)
}

createFeatureMaxMin <- function(trainData){
  trainData[, latestHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, clocklat.max)
  trainData[, earliestHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, clocklat.min)
  print(head(trainData))
  return(trainData) 
}

createFeatureAvgTotal <- function(trainData){
  trainData[, avgHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, clocklat.mean)
  trainData[, totalHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, clocklat.sum)
  print(head(trainData))
  return(trainData) 
}



createFriendsWeightedAvgLocation <- function(dataPosts, dataFriends, dataFriendsPostsLoc = NULL){
  
  if (is.null(dataFriendsPostsLoc )) {
    dataFriendsPostsLoc = dataPosts
  }
 
  #remove invalid coordinates
  dataFriendsPostsLoc <- dataFriendsPostsLoc[ which(!is.na(dataFriendsPostsLoc[,region])), ]


  completedSoFar = 0
  resLat <- numeric(nrow(dataPosts))
  resCLat <- numeric(nrow(dataPosts))
  resLon <- numeric(nrow(dataPosts))
  resCLon <- numeric(nrow(dataPosts))
  
  coldataFriends_Id = dataFriends[, id]
  coldataFriendsPostsLoc_id = dataFriendsPostsLoc[, id]
  totalRecords = nrow(dataPosts)
  for( i in 1:nrow(dataPosts)){    

    completedSoFar = completedSoFar + 1
    if (completedSoFar %% 1000 == 0){
      print(paste( (completedSoFar*100/totalRecords), "% completed or", completedSoFar , "records processed so far" , format(Sys.time(), "%a %b %d %X %Y")))
      
    }
   
    userId <- dataPosts[i,id]
    myEarliestHr <- dataPosts[i, earliestHr]
    myAvgHr <- dataPosts[i, avgHr]
    
    dataMyFriends <- dataFriends[which(coldataFriends_Id == userId), friendsId]
    
    dataMyFriendsLatLon <-  dataFriendsPostsLoc[ coldataFriendsPostsLoc_id %in% dataMyFriends , c(id, earliestHr, avgHr, lat, lon)]    
  
    if (length(dataMyFriendsLatLon[,id]) == 0 ){
      #when no friends lat lon available, use all available data     
      dataMyFriendsLatLon <- dataFriendsPostsLoc[  , c(id, earliestHr, avgHr, lat, lon)] 
    }

    resLon[i] <- clocklat.mean(dataMyFriendsLatLon[, lon]) 
    resLat[i] <- clocklat.mean(dataMyFriendsLatLon[, lat]) 
    
    tempColMyFriendsEarliestHr  = dataMyFriendsLatLon[, earliestHr]
    tempColMyFriendsAvgHr  = dataMyFriendsLatLon[, avgHr]
    
  
    #resLon[i] <- mean(dataMyFriendsLatLon[, lon]) 
    tmpColSumDistance <-  abs(myEarliestHr - tempColMyFriendsEarliestHr) 

    indexOfClosestFriend = which( tmpColSumDistance == clocklat.min(tmpColSumDistance) )[1]
    resCLon[i] <-dataMyFriendsLatLon[indexOfClosestFriend, lon ]
    resCLat[i] <- dataMyFriendsLatLon[indexOfClosestFriend, lat ]
  
   
   
  
  }
  
  dataPosts[, avgFriendsLat] <- resLat
  dataPosts[, avgFriendsLon] <- resLon
  dataPosts[, closestFriendsLat] <- resCLat
  dataPosts[, closestFriendsLon] <- resCLon
  return(dataPosts)
}

transformTrainFeatures <- function(dataPosts, dataFriends, outDir){
  data <- CleanInvalidData(dataPosts)
  data <- createFeatureMaxMin(data)  
  data <- createFeatureAvgTotal(data)
  data <- createContinents(data)
  data <- createFriendsWeightedAvgLocation(data, dataFriends)

  write.table(data,  file= file.path(outDir, "transformedData.csv"),  row.names = FALSE, sep=",", quote = FALSE)  
  return(data) 
}


transformTestFeatures <- function(dataPosts, dataFriends, dataFriendsLocPosts, outDir){
  data <- CleanInvalidData(dataPosts)
  data <- createFeatureMaxMin(data)  
  data <- createFeatureAvgTotal(data)
  
  data <- createFriendsWeightedAvgLocation(data, dataFriends, dataFriendsLocPosts)
  write.table(data,  file= file.path(outDir, "transformedTestData.csv"),  row.names = FALSE, sep=",", quote = FALSE)  
  return(data) 
}

