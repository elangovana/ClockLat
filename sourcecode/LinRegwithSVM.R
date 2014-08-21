source("./globals.R")

##########Linear Regression with SVM Data########
#################################################
#################################################

calcLinearRegwithSVMData<- function(postsData, testData, origtestData, outDir){
  createDir(outDir)
  print(head(postsData))
  print(head(testData))
  newfactors<-paste(c(region,hour1),collapse="+")
  
  ## latitude
  formula <- as.formula(paste(paste(lat,"~"),newfactors))
  print("Formula for lat used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lat <-predict(lm_model, newdata=testData)
  
  
  #Longitude
  
  formula <- as.formula(paste(paste(lon,"~"),newfactors))
  print("Formula for longitude used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  predicted_lon <-predict(lm_model, newdata=testData)
  
  #write predicted values to file
  writePredicationAsCsv(origtestData, predicted_lat, predicted_lon, outdir, "SVMLinearReg")
  
}
