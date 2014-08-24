source("./globals.R")
source("./plotModel.R")
source("./linearRegressionModel.R")
source("./MutualInfoBasedModel.R")
source("./SVMModel.R")
source("./LinRegwithSVM.R")
source("./Transformlabel.R")
source("./TransformFeature.R")
source("./linRegWithFriends.R")
##########################
## Main #################
##########################
options(echo=FALSE)
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
trainDataFriends <- read.table(file = fileTrainDataFriends, col.names=c("Id", "FrdsId"))

colnames(trainDataPosts) <- colInputTrainHeaders
colnames(testDataPosts) <- colInputTestHeaders

head(trainDataPosts)
head(testDataPosts)

plotModel(trainDataPosts, outDir)
transformedTrainData <- transformFeatures(trainDataPosts, outDir)
transformedTestData <- transformFeatures(testDataPosts, outDir)
plotTransformedModel(transformedTrainData, outDir)

labeledTrainDataPosts <- labelContinents(transformedTrainData,  outDir)


calcLinearRegression(transformedTrainData, transformedTestData, outDir)
calcLinearRegressionOnEarliestHour(transformedTrainData, transformedTestData, outDir)
#calcCustomMutualInformation(trainDataPosts, testDataPosts, outDir)
#labeledTestDataPosts<-calcSVM(labeledTrainDataPosts, transformedTestData, outDir)
linearRegwithFriendsList(labeledTrainDataPosts,trainDataFriends,transformedTestData,outDir)

