
options(echo=TRUE)
args<-commandArgs(trailingOnly = TRUE)

##default data set when no args provided
fileTrainDataPosts = "./../data/posts-train.txt"
filetrainDataFriends = "./../data/graph.txt"

## parse args for data set
if (length(args) == 2) {
  print(args)
  fileTrainDataPosts <- args[0]
  filetrainDataFriends <- args[1]
}

## load data
trainDataPosts <- read.csv(file = fileTrainDataPosts, header = TRUE)
colnames(trainDataPosts) <- c("Id", "Hour1","Hour2" ,  "Hour3"  , "Lat", "Lon", "Posts")

head(trainDataPosts)


