source("./globals.R")

############################
##linear regression model##
#############################
calcLinearRegression<- function(postsData, testData, outDir){
  createDir(outDir)
  
  factors <- paste(c(hour1,hour2, hour3, posts),collapse="+")
  
  ## latitude
  formula <- as.formula(paste(paste(closestFriendLat,"~"),factors))
  print("Formula for lat used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lat <-predict(lm_model, newdata=testData)
 
  
  #longitude 
  formula <- as.formula(paste(paste(closestFriendLon,"~"),factors))
  print("Formula for longitude used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lon <-predict(lm_model, newdata=testData)
 
  
  #write predicted values to file
  writePredicationAsCsv(testData, predicted_lat, predicted_lon, outdir, "LinearRegression")
}

calcLinearRegressionOnEarliestAvgHour<- function(postsData, testData, outDir){
  createDir(outDir)
  
  factors <- paste(c(earliestHr, avgHr),collapse="+")
  
  ## latitude
  formula <- as.formula(paste(paste(lat,"~"),factors))
  print("Formula for lat used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lat <-predict(lm_model, newdata=testData)
  
  
  #longitude 
  formula <- as.formula(paste(paste(lon,"~"),factors))
  print("Formula for longitude used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lon <-predict(lm_model, newdata=testData)
  warnings()
  
  #write predicted values to file
  writePredicationAsCsv(testData, predicted_lat, predicted_lon, outdir, "LinearRegressionEarliestHr")
}

calcLinearRegressionOnFriendsList <- function(postsData,  testData, outDir){
  createDir(outDir)
 
  ## latitude
  predicted_lat <- calcLinearRegressionLatOnFriendsList(postsData,  testData, outDir)
  
  #longitude 
  predicted_lon <- calcLinearRegressionLonOnFriendsList(postsData,  testData, outDir)
  #write predicted values to file
  return(writePredicationAsCsv(testData, predicted_lat, predicted_lon, outdir, "LinearRegressionFriendsHr"))
  
}

calcLinearRegressionLatOnFriendsList<- function(postsData,  testData, outDir, fileName="latFittedModelFriend.pdf"){
  factors <- paste(c(closestFriendsLat),collapse="+")
  formula <- as.formula(paste(paste(lat,"~"),factors))
  print("Formula for lat used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lat <-predict(lm_model, newdata=testData)
  plotFittedModel(lm_model, fileName, outDir)

  return(predicted_lat)
}

calcLinearRegressionLonOnFriendsList<- function(postsData,  testData, outDir, filename="lonFittedModelFriend.pdf"){
  factors <- paste(c(closestFriendsLon, earliestHr),collapse="+")
  formula <- as.formula(paste(paste(lon,"~"),factors))
  print("Formula for longitude used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lon <-predict(lm_model, newdata=testData)
  plotFittedModel(lm_model, filename, outDir)

  return(predicted_lon)
}


calcLinearRegressionOnFriendsListWithZonalModel <- function(postsData,  testData, outDir){
  #splitDataBased On Zones
  min = "min"
  max = "max"
  latZoneList = data.frame(c(NA, 0), c(0,NA))
  colnames(latZoneList) <- c(min, max)
  print(latZoneList)
  predicted_lat <- data.frame()
  for( i in 1:nrow(latZoneList)){ 
    minLat = latZoneList[i, min]
    maxLat = latZoneList[i, max]
    print(paste("min max", minLat, maxLat))
    splitPostData <- postsData[ (is.na(minLat) | (postsData[, lat]>=minLat & postsData[, majorityFriendsLat]>=minLat) ) &  (is.na(maxLat) | (postsData[, lat]<maxLat & postsData[, majorityFriendsLat]<maxLat)) ,]
    print(dim(splitPostData))

    splitTestData <- testData[ (is.na(minLat) | testData[,majorityFriendsLat]>=minLat) & (is.na(maxLat) | testData[, majorityFriendsLat]<maxLat), ]

    prediction <- calcLinearRegressionLatOnFriendsList(splitPostData, splitTestData, outDir, paste("latFitted", minLat, maxLat, ".pdf"))

    print(dim(predicted_lat))
    print(str(prediction))
    prediction <- data.frame(prediction, row.names = names(prediction))
    predicted_lat <- rbind (predicted_lat, prediction)
    print(dim(predicted_lat))
  }
  
  lonZoneList = data.frame(c(NA,0), c(0,  NA))
  colnames(lonZoneList) <- c(min, max)
  print(lonZoneList)
  predicted_lon <- data.frame()
  for( i in 1:nrow(lonZoneList)){ 
    
    minLon = lonZoneList[i, min]
    maxLon = lonZoneList[i, max]
    print(paste("min max", minLon, maxLon))
    splitPostData <- postsData[ ( is.na(minLon) | (postsData[, lon]>=minLon & postsData[, majorityFriendsLon]>=minLon )) & (is.na(maxLon) | (postsData[, lon]<maxLon & postsData[, majorityFriendsLon]<maxLon)),]
    print(dim(splitPostData))

    splitTestData <- testData[  (is.na(minLon) | testData[,majorityFriendsLon]>=minLon) &   (is.na(maxLon) | testData[, majorityFriendsLon]<maxLon), ]

    prediction <- calcLinearRegressionLatOnFriendsList(splitPostData, splitTestData, outDir, paste("lonFitted", minLon, maxLon, ".pdf"))
    prediction <- data.frame(prediction, row.names = names(prediction))
    print(dim(predicted_lon))
    print(str(prediction))
    predicted_lon <- rbind (predicted_lon, prediction)
    print(dim(predicted_lon))    
  }
  
  print("completed pridction")
  print(dim( predicted_lon))
  print(dim( predicted_lat))
  print(dim(testData))
  colnames(predicted_lon) <- c("predicted_lon")
  colnames(predicted_lat) <- c("predicted_lat")
  writePredicationAsCsv(testData, predicted_lat, predicted_lon, outdir, "LinearRegressionFriendsZonalHr")
}