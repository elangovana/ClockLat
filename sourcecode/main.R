source("./globals.R")
source("./plotModel.R")
source("./linearRegressionModel.R")


##########################
## Main #################
##########################
options(echo=FALSE)
args<-commandArgs(trailingOnly = TRUE)

##default data set when no args provided
fileTrainDataPosts = "./../data/posts-train.txt"
fileTrainDataFriends = "./../data/graph.txt"
fileTestDataFriends = "./../data/posts-test-x.txt"
outDir = "./../output/"

## parse args for data set
if (length(args) == 2) {
  print(args)
  fileTrainDataPosts <- args[0]
  fileTrainDataFriends <- args[1]
  ##TODO: include others
} else {
  cat("Using Default dataset", fileTrainDataPosts)
}
 

## load data
trainDataPosts <- read.csv(file = fileTrainDataPosts, header = TRUE)
colnames(trainDataPosts) <- c(id, hour1,hour2, hour3, lat, lon, posts)
head(trainDataPosts)


plotModel(trainDataPosts, outDir)
calcLinearRegression(trainDataPosts)
