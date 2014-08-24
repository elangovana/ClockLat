source("./InstallPackage.R")
pkgInstall("fpc")

linearRegwithFriendsList <- function(postsData, friendsData, testData, outDir){
  
 
  idvalues<-testData$Id
  
  #print(idvalues)
  finalTable<-data.frame()
  
  for(i in idvalues)
  {
    data<-data.frame()
    print(i)
    friendsData.temp<-subset(friendsData, friendsData$Id==i)
    
    rowdata<-subset(testData,testData$Id==i)
    
    for(j in 1:nrow(friendsData.temp))
    {
      data<-rbind(data,subset(postsData,postsData$Id==friendsData.temp[j,2]))
      data<-na.omit(data)
      
    }
    df2<-calcLinearRegressionOnEarliestHour(data, rowdata, outDir)
    finalTable<-rbind(finalTable,df2)
    
  }
  writePredicationAsCsv(finalTable, predicted_lat, predicted_lon, outdir, "LinearRegrWithFriendsData")
  return(finalTable)
}
calcLinearRegressionOnEarliestHour<- function(postsData, testData, outDir){
  createDir(outDir)
  
  factors <- paste(c(earliestHr, avgHr),collapse="+")
  
  ## latitude
  formula <- as.formula(paste(paste(lat,"~"),factors))
  #print("Formula for lat used: ")
  #print(formula)
  lm_model <-lm(formula, data= postsData)
  #print(summary(lm_model))
  predicted_lat <-predict(lm_model, newdata=testData)
  
  
  #longitude 
  formula <- as.formula(paste(paste(lon,"~"),factors))
  #print("Formula for longitude used: ")
  #print(formula)
  lm_model <-lm(formula, data= postsData)
  #print(summary(lm_model))
  predicted_lon <-predict(lm_model, newdata=testData)
  
  testData<-cbind(testData,predicted_lat,predicted_lon)
  return(testData)
  #write predicted values to file
  #writePredicationAsCsv(testData, predicted_lat, predicted_lon, outdir, "LinearRegressionEarliestHr")
}
