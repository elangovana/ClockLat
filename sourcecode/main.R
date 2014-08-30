source("./globals.R")
source("./utilities.r")
source("./plotModel.R")
source("./linearRegressionModel.R")
source("./SVMModel.R")
source("./LinRegwithSVM.R")
source("./TransformFeature.R")
source("./BaggingWithLinearRegressionModel.r")



##########################
## Main #################
##########################
options(echo=FALSE)
#options( warn = 2 )
trainRunOnly = FALSE
##Options for train run only
TRAINSIZE = 100
TESTSIZE = 1
RUNS = 1
##
#ModelFunctions
calcFunctions <- list("LinearRegressionOnFriendsList"=calcLinearRegressionOnFriendsList, "BaggedRegression"=calcBaggedRegression)
##
args<-commandArgs(trailingOnly = TRUE)

##default data set when no args provided
fileTrainDataPosts = "./../data/posts-train.txt"
fileTrainDataFriends = "./../data/graph.txt"
fileTestDataPosts = "./../data/posts-test-x.txt"
outDir = "./../output"

## parse args for data set
if (length(args) == 4) {
  print(args)
  fileTrainDataPosts <- args[0]
  fileTrainDataFriends <- args[1]
  fileTestDataPosts <- args[2]
  outDir <- args[3]
  ##TODO: include others
} else {
  print("Usage:  main.r  trainPostsFile TrainFriendsFile TestPostsFile outputDir \n")
  cat("Using Default dataset without parameters ", fileTrainDataPosts, fileTrainDataFriends, fileTestDataPosts, outDir)
}


## load data
fullTrainDataPosts <- read.csv(file = fileTrainDataPosts, header = TRUE)
fullTestDataPosts <-  read.csv(file = fileTestDataPosts, header = TRUE)
fullTrainDataFriends <- read.table(file = fileTrainDataFriends, col.names=colInputFriends)



actualTestDataLatLon <- NULL
if (!trainRunOnly){
  RUNS = 1
}
resError = data.frame()

for(i in 1:RUNS){
  
  trainDataPosts <- fullTrainDataPosts
  testDataPosts <- fullTestDataPosts
  trainDataFriends <- fullTrainDataFriends
  
  if (trainRunOnly){
    
    sampledtestDataPosts =  selectTestData(fullTrainDataPosts, TESTSIZE)
    sampledtrainDataPosts = sampleTrainData(fullTrainDataPosts, sampledtestDataPosts, TRAINSIZE)
    
    trainDataPosts = sampledtrainDataPosts
    testDataPosts = sampledtestDataPosts[, colInputTestHeaders]
    actualTestDataLatLon = sampledtestDataPosts[, c(id, lat, lon)]
    
  }
  
  
  colnames(trainDataPosts) <- colInputTrainHeaders
  colnames(testDataPosts) <- colInputTestHeaders
  #plot Input data
  plotModel(trainDataPosts, outDir)
  
  #transform features
  transformedTrainData <- transformTrainFeatures(trainDataPosts,trainDataFriends, outDir)
  transformedTestData <- transformTestFeatures(testDataPosts, trainDataFriends, transformedTrainData, outDir)
  plotTransformedModel(transformedTrainData, outDir)
  
  #predictions
  for(f in 1:length(calcFunctions)){
    print("In here")
    calcFunction = calcFunctions[[f]]
    nameOfFunct <- names(calcFunctions)[[f]]
    predictedResults <- calcFunction(transformedTrainData, transformedTestData, outDir)
    if (trainRunOnly) {
      #compare results    
      print("In result")
      rmsResults <- compareAcutalVsPredicted(actualTestDataLatLon, predictedResults)
      resError[i, paste(nameOfFunct,"Errlat")] <- rmsResults[1]
      resError[i, paste(nameOfFunct,"ErrLon")] <- rmsResults[2]
      resError[i, paste(nameOfFunct,"ErrAvg")] <- rmsResults[3]
    }
  }
  
}
if (trainRunOnly) {
  write.csv(resError, file= file.path(outDir, paste( c("rmsError.csv"), collapse="")),  quote = FALSE)
}





