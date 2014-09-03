source("./globals.R")
source("./InstallPackage.R")

calcKnn <- function(postsData, testData, outDir){
  pkgInstall("caret")
  library(caret)
  
  
  
  ## latitude
  factors <- paste(c(closestFriendsLat),collapse="+")
  formula <- as.formula(paste(paste(lat,"~"),factors))

  # fit model
  fit <- knnreg(formula, postsData, k=5)
  # summarize the fit
  print(summary(fit))
  # make predictions
  prediction <- predict(fit, testData)

  prediction_lat <- apply(prediction, 1, which.max)
  prediction_lat <- names(prediction[1,])[prediction_lat]
  prediction_lat <- as.numeric(prediction_lat)
  prediction_lat <- data.frame(prediction_lat)
  colnames(prediction_lat) <- c("predicted_lat")

  
  # fit model
  factors <- paste(c(earliestHr, latestHr, closestFriendsLon),collapse="+")
  formula <- as.formula(paste(paste(lon,"~"),factors))
  fit <- knnreg(formula, postsData, k=5)
  # summarize the fit
  print(summary(fit))
  # make predictions
  prediction <- predict(fit, testData, type =  c("prob", "class"))
  print((prediction))
  prediction_lon <- apply(prediction, 1, which.max)
  prediction_lon <- names(prediction[1,])[prediction_lon]
  prediction_lon <- as.numeric(prediction_lon)
  prediction_lon <- data.frame(prediction_lon)
  colnames(prediction_lon) <- c("predicted_lon")


  #write predicted values to file
  writePredicationAsCsv(testData, prediction_lat, prediction_lon, outdir, "LinearRegressionKnn")
}
