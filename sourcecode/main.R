source("./globals.R")
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

## parse args for data set
if (length(args) == 2) {
  print(args)
  fileTrainDataPosts <- args[0]
  fileTrainDataFriends <- args[1]
} else {
  cat("Using Default dataset", fileTrainDataPosts)
}
 

## load data
trainDataPosts <- read.csv(file = fileTrainDataPosts, header = TRUE)
colnames(trainDataPosts) <- c(id, hour1,hour2, hour3, lat, lon, posts)
head(trainDataPosts)

calcLinearRegression(trainDataPosts)
