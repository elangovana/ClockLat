###Colnames
hour1 <- "Hour1"
id <- "Id"
hour2 <- "Hour2"
hour3 <- "Hour3"
lat <- "Lat"
lon <- "Lon"
posts <- "Posts"
region <- "Region"
predictedRegion <- "PredictedRegion"
earliestHr <- "EarliestHour"
latestHr <- "LatestHour"
avgHr <- "AverageHour"
totalHr <- "TotalHour"
avgFriendsLat <- "AvgFriendsLat"
avgFriendsLon <- "AvgFriendsLon"
closestFriendsLat <- "closestFreindsLat"
closestFriendsLon <- "closestFriendsLon"
friendsId <- "friendsId"
majorityFriendsLat <- "MajorityFreindsLat"
majorityFriendsLon <- "majorityFriendsLon"
friendsCount <- "friendsCount"
friendsDataBad <- "FriendsDataBad" 
closestFriendFeatureDistance <- "closestFriendFeatureDistance"
colInputTrainHeaders <-  c(id, hour1,hour2, hour3, lat, lon, posts)
colInputTestHeaders <-  c(id, hour1,hour2, hour3,  posts)
colInputFriends <- c(id, friendsId)

createDir <- function(path){ 
  if (!file.exists(path)){ 
    dir.create(path)  
  }
}

clocklat.mean <- function(x){
  mean(x, na.rm = TRUE)
}

clocklat.min <- function(x){
  min(x, na.rm = TRUE)
}


clocklat.max <- function(x){
  max(x, na.rm = TRUE)
}

clocklat.sum <- function(x){
  sum(x, na.rm = TRUE)
}

writePredicationAsCsv <- function(testData, predicted_lat, predicted_lon, outdir, filenamePrefix){
  createDir(outDir)
  
  data <- cbind(predicted_lat,predicted_lon)
  data <- cbind(testData, data)
  names(data)[names(data) == 'predicted_lat'] <- lat
  names(data)[names(data) == 'predicted_lon'] <- lon
  
  print(file.path(outDir, paste( c(filenamePrefix,"submission",'.csv'), collapse="")))
  
  write.csv(data[, c(id, lat,lon)], file= file.path(outDir, paste( c(filenamePrefix,"submission",'.csv'), collapse="")), row.names = FALSE,  quote = FALSE) 
  write.table(data,  file= file.path(outDir, paste( c(filenamePrefix,'.csv'), collapse="")),  row.names = FALSE, sep=",", quote = FALSE)  
  
  return(data)
}
