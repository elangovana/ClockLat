source("./globals.R")
source("./InstallPackage.R")


createFeatureMaxMin <- function(trainData){
  trainData[, latestHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, max)
  trainData[, earliestHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, min)
  print(head(trainData))
  return(trainData) 
}

createFeatureAvgTotal <- function(trainData){
  trainData[, avgHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, mean)
  trainData[, totalHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, sum)
  print(head(trainData))
  return(trainData) 
}



createFriendsWeightedAvgLocation <- function(dataPosts, dataFriends, dataFriendsPostsLoc = NULL){
  
  if (is.null(dataFriendsPostsLoc )) {
    dataFriendsPostsLoc = dataPosts
  }
  
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
    nofriends <- FALSE
    userId <- dataPosts[i,id]
    myEarliestHr <- dataPosts[i, earliestHr]
    myAvgHr <- dataPosts[i, avgHr]
    
    dataMyFriends <- dataFriends[which(coldataFriends_Id == userId), friendsId]
    
    dataMyFriendsLatLon <-  dataFriendsPostsLoc[ coldataFriendsPostsLoc_id %in% dataMyFriends , c(id, earliestHr, avgHr, lat, lon)]    
  
    if (length(dataMyFriendsLatLon[,id]) == 0 ){
      #when no friends lat lon available, use all available data     
      dataMyFriendsLatLon <- dataFriendsPostsLoc[  , c(id, earliestHr, avgHr, lat, lon)] 
    }

    resLon[i] <- mean(dataMyFriendsLatLon[, lon]) 
    resLat[i] <- mean(dataMyFriendsLatLon[, lat]) 
    
    tempColMyFriendsEarliestHr  = dataMyFriendsLatLon[, earliestHr]
    tempColMyFriendsAvgHr  = dataMyFriendsLatLon[, avgHr]
    
  
    #resLon[i] <- mean(dataMyFriendsLatLon[, lon]) 
    tmpColSumDistance <-  abs(myEarliestHr - tempColMyFriendsEarliestHr) + abs(myAvgHr - tempColMyFriendsAvgHr)

    indexOfClosestFriend = which( tmpColSumDistance == min(tmpColSumDistance) )[1]
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
  data <- createFeatureMaxMin(dataPosts)  
  data <- createFeatureAvgTotal(data)
  data <- createFriendsWeightedAvgLocation(data, dataFriends)
  write.table(data,  file= file.path(outDir, "transformedData.csv"),  row.names = FALSE, sep=",", quote = FALSE)  
  return(data) 
}


transformTestFeatures <- function(dataPosts, dataFriends, dataFriendsLocPosts, outDir){
  data <- createFeatureMaxMin(dataPosts)  
  data <- createFeatureAvgTotal(data)
  
  data <- createFriendsWeightedAvgLocation(data, dataFriends, dataFriendsLocPosts)
  write.table(data,  file= file.path(outDir, "transformedTestData.csv"),  row.names = FALSE, sep=",", quote = FALSE)  
  return(data) 
}

