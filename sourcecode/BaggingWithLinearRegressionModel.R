source("./globals.R")

pkgInstall("ipred")


calcBaggedRegression <- function(postsData, testData, outDir){
  library("ipred")
  library("rpart")

  
  ns <- floor (2*length(postsData[,id])/3)
  print(ns)
  ## latitude
  factors <- paste(c( avgFriendsLat, majorityFriendsLat, friend1Lat, friend2Lat, friend3Lat, posts),collapse="+")
  formula <- as.formula(paste(paste(lat,"~"),factors))
  
  mod <- bagging(formula, nbagg=50, data=postsData, coob=TRUE, ns = ns)
  print(summary(mod))   
  predicted_lat <- predict(mod, newdata=testData, aggregation="weighted") 

  
  ## lon
  newcol<- postsData$latestHr + postsData$earliestHr
  factors <- paste(c( avgFriendsLon, majorityFriendsLon, friend1Lon, friend2Lon, friend3Lon, posts, earliestHr),collapse="+")
  formula <- as.formula(paste(paste(lon,"~"),factors))
  
  mod <- bagging(formula, nbagg=50, data=postsData, coob=TRUE, ns=ns)
  print(summary(mod))  
  predicted_lon <- predict(mod, newdata=testData, aggregation="weighted") 

  
  return(writePredicationAsCsv(testData, predicted_lat, predicted_lon, outdir, "LinearRegressionBagging"))
  
}

