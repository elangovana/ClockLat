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
  data <- cbind(predicted_lat,predicted_lon)
  data <- cbind(testData, data)
  colnames(data) <- colHeaders
 
  write.csv(data[, c(id, lat,lon)], file.path(outDir, file='submissionPredictedLinearRegressionTestData.csv'), row.names = FALSE,  quote = FALSE) 
  write.table(data, file.path(outDir,'predictedLinearRegressionTestData.csv'),  row.names = FALSE, sep=",", quote = FALSE)  
}