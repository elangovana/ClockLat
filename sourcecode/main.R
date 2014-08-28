source("./globals.R")
source("./plotModel.R")
source("./linearRegressionModel.R")
source("./SVMModel.R")
source("./LinRegwithSVM.R")
source("./TransformFeature.R")

calcRMS <- function(actualData, predictedData){
  n <- length(actualData)

  return( sqrt( sum(( actualData - predictedData)^2) /n))
}


##########################
## Main #################
##########################
options(echo=FALSE)
options( warn = 2 )
trainRunOnly = TRUE
args<-commandArgs(trailingOnly = TRUE)

##default data set when no args provided
fileTrainDataPosts = "./../data/posts-train.txt"
fileTrainDataFriends = "./../data/graph.txt"
fileTestDataPosts = "./../data/posts-test-x.txt"
outDir = "./../output"

## parse args for data set
if (length(args) == 2) {
  print(args)
  fileTrainDataPosts <- args[0]
  fileTrainDataFriends <- args[1]
  ##TODO: include others
} else {
  cat("Using Default dataset", fileTrainDataPosts)
  print("Usage: 
        main.r TestPostsFile trainPostsFile TrainFriendsFile outputDir \n
        where 
            TestPostsFile is the test dataset.
            Tests
        ")
}
 

## load data
trainDataPosts <- read.csv(file = fileTrainDataPosts, header = TRUE)
testDataPosts <-  read.csv(file = fileTestDataPosts, header = TRUE)

trainDataFriends <- read.table(file = fileTrainDataFriends, col.names=colInputFriends)
actualTestDataLatLon <- NULL

if (trainRunOnly){
  sampleTrainData <- function(trainDataPosts1){
    trainDataPosts <- trainDataPosts1[sample(nrow(trainDataPosts1), 45000), ]
    return(trainDataPosts)
  }
  
  sampleTestData <- function(trainDataPosts1, selectedTrainDataPosts){
    trainDataPostsNotInSample <- trainDataPosts1[which( !(trainDataPosts1[,id] %in% selectedTrainDataPosts[,id]) ),]
    dim(trainDataPostsNotInSample)
    testDataPosts <- trainDataPostsNotInSample[sample(nrow(trainDataPostsNotInSample),1000),  ]
    return(testDataPosts)
  }
  sampledtrainDataPosts =  sampleTrainData(trainDataPosts)
  sampledtestDataPosts = sampleTestData(trainDataPosts, sampledtrainDataPosts)

  trainDataPosts = sampledtrainDataPosts
  testDataPosts = sampledtestDataPosts[, colInputTestHeaders]
  actualTestDataLatLon = sampledtestDataPosts[, c(id, lat, lon)]
  print("---")
  print(dim(trainDataPosts))
  print(dim(testDataPosts))
}


colnames(trainDataPosts) <- colInputTrainHeaders
colnames(testDataPosts) <- colInputTestHeaders

head(trainDataPosts)
head(testDataPosts)

plotModel(trainDataPosts, outDir)
transformedTrainData <- transformTrainFeatures(trainDataPosts,trainDataFriends, outDir)
transformedTestData <- transformTestFeatures(testDataPosts, trainDataFriends, transformedTrainData, outDir)
plotTransformedModel(transformedTrainData, outDir)


#calcLinearRegressionOnFriendsListWithZonalModel(transformedTrainData, transformedTestData, outDir)
 predictedResults <- calcLinearRegressionOnFriendsList(transformedTrainData, transformedTestData, outDir)

#labeledTestDataPosts<-calcSVM(transformedTrainData, transformedTestData, outDir)

if (trainRunOnly) {
  rmsLat <- calcRMS(actualTestDataLatLon[, lat], predictedResults[,lat])
  rmsLon <- calcRMS(actualTestDataLatLon[, lon], predictedResults[,lon])
  print(paste("RMA Lat, RMA Lon, Total Avg LMS = " , rmsLat, rmsLon, (rmsLat +rmsLon)/2))
}



