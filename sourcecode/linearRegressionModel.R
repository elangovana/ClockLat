source("./globals.R")

############################
##linear regression model##
#############################
calcLinearRegression<- function(postsData, testData, outDir){
  createDir(outDir)
  
  factors <- paste(c(hour1,hour2, hour3, posts),collapse="+")
  
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
  factors <- paste(c(majorityFriendsLat),collapse="+")
  formula <- as.formula(paste(paste(lat,"~"),factors))
  print("Formula for lat used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lat <-predict(lm_model, newdata=testData)
  plotFittedModel(lm_model, "latFittedModelFriend.pdf", outDir)
  print(warnings())
  
  #longitude 
  factors <- paste(c(majorityFriendsLon),collapse="+")
  formula <- as.formula(paste(paste(lon,"~"),factors))
  print("Formula for longitude used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lon <-predict(lm_model, newdata=testData)
  plotFittedModel(lm_model, "lonFittedModelFriend.pdf", outDir)
  print(warnings())
  #write predicted values to file
  writePredicationAsCsv(testData, predicted_lat, predicted_lon, outdir, "LinearRegressionFriendsHr")
  
}