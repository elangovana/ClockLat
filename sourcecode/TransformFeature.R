source("./globals.R")
source("./InstallPackage.R")

pkgInstall("rworldmap")
pkgInstall("plyr")
pkgInstall("igraph")

library(igraph)
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
  
  return(trainData) 
}



createFeatureAvgTotal <- function(trainData){
  trainData[, avgHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, clocklat.mean)
  trainData[, totalHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, clocklat.sum)
  
  return(trainData) 
}

getMyImmediateFriendsLatLon <- function(userId, friendsGraph, friendsLatLon, alreadySearchedEdgeIndex){
  
  colfriendsGraph_Id = friendsGraph[, id]
  colfriendsGraph_friendId = friendsGraph[, friendsId]
  colFriendsPostsLoc_id = friendsLatLon[, id]
  
  dataMyFriends <- friendsGraph[which(colfriendsGraph_Id == userId ), friendsId]
  
  #print(dataMyFriends)
  dataMyFriendsLatLon <-  friendsLatLon[ colFriendsPostsLoc_id %in% dataMyFriends, ]  
  
  return (dataMyFriendsLatLon)
}

findMyFriends1 <- function (userId, friendsGraph, friendsLatLon, minFriendsToFind = 10, maxDepth = 7, currentDepth=0,friendsCountSoFar = 0, alreadySearchedEdgeIndex = logical(nrow(friendsGraph))){
  #print(paste("------", "friends for user , depth, friends so far", userId, currentDepth, friendsCountSoFar))
  
  dataMyFriendsLatLon <- getMyImmediateFriendsLatLon(userId, friendsGraph, friendsLatLon, alreadySearchedEdgeIndex)
  friendsCountSoFar <- length(dataMyFriendsLatLon[, id]) + friendsCountSoFar 
  #print(dataMyFriendsLatLon)
  if ( friendsCountSoFar < minFriendsToFind){
    colfriendsGraph_Id = friendsGraph[, id]
    colfriendsGraph_friendId = friendsGraph[, friendsId]
    dataMyFriends <- friendsGraph[which(colfriendsGraph_Id == userId ), friendsId]
    #print("List Of friends")
    #print(dataMyFriends)
    for(i in dataMyFriends){
      friendsOfFriends <- getMyImmediateFriendsLatLon(i, friendsGraph, friendsLatLon, alreadySearchedEdgeIndex)
      dataMyFriendsLatLon <- rbind( dataMyFriendsLatLon, friendsOfFriends ) 
    }
    friendsCountSoFar = length(dataMyFriendsLatLon[,id])+ friendsCountSoFar
    #print(dataMyFriendsLatLon)
    currentDepth = currentDepth + 1
    
    if ( friendsCountSoFar < minFriendsToFind){
      if (currentDepth < maxDepth){
        #print(paste("Depth, fiendsCOunt", currentDepth, friendsCountSoFar))
        for(i in dataMyFriends){
          friendsOfFriends <- findMyFriends(i, friendsGraph, friendsLatLon, minFriendsToFind, maxDepth, currentDepth,friendsCountSoFar, alreadySearchedFriends)
          dataMyFriendsLatLon <- rbind( dataMyFriendsLatLon, friendsOfFriends ) 
          friendsCountSoFar = length(dataMyFriendsLatLon[,id]) + friendsCountSoFar
        }
      }
      
      
    }
  }
  return(dataMyFriendsLatLon)
}


findMyFriends <- function (userId, graphFriends, friendsLatLon, colFriendsPostsLoc_id, minFriends=10){  
  dataMyFriendsLatLon <- data.frame()
  
  tryCatch({
   v< - shortest.paths(graphFriends, v = as.character(userId) )
   if (is.null(v)) return (friendsLatLon)

   v<- t(v)  
   v<- v[order(v[, 1]), ]
   friends = names(v)

   friends <- friends[friends %in% colFriendsPostsLoc_id & friends != userId ][1:minFriends]

   dataMyFriendsLatLon <- friendsLatLon[ colFriendsPostsLoc_id %in% friends,]

   return(dataMyFriendsLatLon)
  },
  error = function(err) {   
    # error handler picks up where error was generated
    print(paste("MY_ERROR:  ", userId,err))
    return (friendsLatLon)
  })
  
}


calcMajorityFriendsLoc <- function(dataMyFriendsLatLon){
  tmpColFriendsLatLonFloored <- floor( dataMyFriendsLatLon[, c(lat, lon)])
  colnames( tmpColFriendsLatLonFloored) <- c(lat,lon)
  
  tmpCountOfFriendsFlooredLatLon = count(tmpColFriendsLatLonFloored, c(lat, lon))
  
  majorityFriendsFlooredLatLon = tmpCountOfFriendsFlooredLatLon[ which(tmpCountOfFriendsFlooredLatLon$freq == max(tmpCountOfFriendsFlooredLatLon$freq)), ]  
  majorityFriendsFlooredLatLon = majorityFriendsFlooredLatLon[1, ]
  
  majorityFriendsLatLon = dataMyFriendsLatLon[which(floor(dataMyFriendsLatLon[, lat]) %in% majorityFriendsFlooredLatLon[,lat] 
                                                    & floor(dataMyFriendsLatLon[, lon]) %in% majorityFriendsFlooredLatLon[,lon]),]
  
  return( c(clocklat.mean(majorityFriendsLatLon[, lat]), clocklat.mean( majorityFriendsLatLon[, lon])))
  
  
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
  resCfndDist <- numeric(nrow(dataPosts))
  resMLon <- numeric(nrow(dataPosts))
  resMLat <- numeric(nrow(dataPosts))
  resFriendsCount <- numeric(nrow(dataPosts))
  
  coldataFriends_Id = dataFriends[, id]
  coldataFriendsPostsLoc_id = dataFriendsPostsLoc[, id]
  totalRecords = nrow(dataPosts)
  
  
  graphFriends <- graph.data.frame(dataFriends)
  
  for( i in 1:nrow(dataPosts)){    
    
    completedSoFar = completedSoFar + 1
    if (completedSoFar %% 500 == 0){
      print(paste( (completedSoFar*100/totalRecords), "% completed or", completedSoFar , "records processed so far" , format(Sys.time(), "%a %b %d %X %Y")))
      
    }
    hasFriends = TRUE
    myLat = dataPosts[i,lat]
    myLon = dataPosts[i, lon]
    userId <- dataPosts[i,id]
    myEarliestHr <- dataPosts[i, earliestHr]
    myLatestHr <- dataPosts[i, latestHr]
    
    
    #dataMyFriendsLatLon <- findMyFriends(userId, graphFriends, dataFriendsPostsLoc, coldataFriendsPostsLoc_id) 
    dataMyFriends <- dataFriends[which(coldataFriends_Id == userId), friendsId]

    dataMyFriendsLatLon <-  dataFriendsPostsLoc[ coldataFriendsPostsLoc_id %in% dataMyFriends , c(id, earliestHr, latestHr, lat, lon)]    
    resFriendsCount[i] <- length(dataMyFriendsLatLon[,id])
    if (resFriendsCount[i]  == 0 ){
      #when no friends lat lon available, use all available data    
      dataMyFriendsLatLon <- dataFriendsPostsLoc[  , c(id, earliestHr, latestHr, lat, lon)] 
    }
    dataMyFriendsLatLon <- dataMyFriendsLatLon[ , c(id, earliestHr, latestHr, lat, lon)]  
    
    
    resLon[i] <- clocklat.median(dataMyFriendsLatLon[, lon]) 
    resLat[i] <- clocklat.median(dataMyFriendsLatLon[, lat]) 
    
    tempColMyFriendsEarliestHr  = dataMyFriendsLatLon[, earliestHr]
    tempColMyFriendsLatestHr  = dataMyFriendsLatLon[, latestHr]
    
    
    #resLon[i] <- mean(dataMyFriendsLatLon[, lon]) 
    
    
    tmpColSumDistance <-  abs(myEarliestHr - tempColMyFriendsEarliestHr) + abs(myLatestHr - tempColMyFriendsLatestHr) 
    resCfndDist[i] <- clocklat.min(tmpColSumDistance)
    indexOfClosestFriend = which( tmpColSumDistance == clocklat.min(tmpColSumDistance) )
    resCLon[i] <-clocklat.median (dataMyFriendsLatLon[indexOfClosestFriend, lon ])
    resCLat[i] <-clocklat.median ( dataMyFriendsLatLon[indexOfClosestFriend, lat ]    )
    
    #MajorityFriends
    result <- calcMajorityFriendsLoc(dataMyFriendsLatLon)
    
    resMLat[i] = result[1]
    resMLon[i] = result[2]     
    
  }
  
  dataPosts[, avgFriendsLat] <- resLat
  dataPosts[, avgFriendsLon] <- resLon
  dataPosts[, closestFriendsLat] <- resCLat
  dataPosts[, closestFriendsLon] <- resCLon
  dataPosts[, majorityFriendsLat] <- resMLat
  dataPosts[, majorityFriendsLon] <- resMLon
  dataPosts[, friendsCount] <- resFriendsCount
  dataPosts[, closestFriendFeatureDistance] <- resCfndDist
  return(dataPosts)
  
}

transformBasicFeatures <- function(dataPosts){
  datafl <- CleanInvalidData(dataPosts) 
  datafl <- createFeatureMaxMin(datafl)  
  datafl <- createFeatureAvgTotal(datafl)
  datafl <- createContinents(datafl)
  
  return(datafl)
}

transformTrainFeatures <- function(dataPosts, dataFriends, outDir, dataFriendsLoc = NULL){
  data <- transformBasicFeatures(dataPosts)
  data <- createFriendsWeightedAvgLocation(data, dataFriends, dataFriendsLoc)
 
  
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

