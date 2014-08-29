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
trainRunOnly = TRUE
##Options for train run only
TRAINSIZE = 10000
TESTSIZE = 10
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
trainDataPosts <- read.csv(file = fileTrainDataPosts, header = TRUE)
testDataPosts <-  read.csv(file = fileTestDataPosts, header = TRUE)

trainDataFriends <- read.table(file = fileTrainDataFriends, col.names=colInputFriends)
actualTestDataLatLon <- NULL

if (trainRunOnly){
  
  sampledtestDataPosts =  selectTestData(trainDataPosts, TESTSIZE)
  sampledtrainDataPosts = sampleTrainData(trainDataPosts, sampledtestDataPosts, TRAINSIZE)

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
predictedResultsRegression <- calcLinearRegressionOnFriendsList(transformedTrainData, transformedTestData, outDir)
predictedResultsBagging <- calcBaggedRegression(transformedTrainData, transformedTestData, outDir)


if (trainRunOnly) {
  #compare results

  compareAcutalVsPredicted(actualTestDataLatLon, predictedResultsRegression)
  compareAcutalVsPredicted(actualTestDataLatLon, predictedResultsBagging)
}



