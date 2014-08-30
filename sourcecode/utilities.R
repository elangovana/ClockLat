source("./globals.R")


calcRMS <- function(actualData, predictedData){
  predictedData[ is.na(predictedData) ] <- 0
  return (sqrt(clocklat.mean((actualData-predictedData)^2)))
}

compareAcutalVsPredicted <- function(actualTestDataLatLon, predictedResults){
  rmsLat <- calcRMS(actualTestDataLatLon[, lat], predictedResults[,lat])
  rmsLon <- calcRMS(actualTestDataLatLon[, lon], predictedResults[,lon])
  print(paste("RMA Lat, RMA Lon, Total Avg LMS = " , rmsLat, rmsLon, (rmsLat +rmsLon)/2))
  rmsAvg <- (rmsLat + rmsLon)/2
  return(list(rmsLat, rmsLon, rmsAvg))
}

selectTestData <- function(trainDataPosts1, count){
  trainDataPosts <- trainDataPosts1[c(1:count), ]
  return(trainDataPosts)
}

sampleTrainData <- function(trainDataPosts1, selectedTrainDataPosts, count){
  trainDataPostsNotInSample <- trainDataPosts1[which( !(trainDataPosts1[,id] %in% selectedTrainDataPosts[,id]) ),]
  dim(trainDataPostsNotInSample)
  testDataPosts <- trainDataPostsNotInSample[sample(nrow(trainDataPostsNotInSample),count),  ]
  return(testDataPosts)
}

