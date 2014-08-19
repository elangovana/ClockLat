
options(echo=FALSE)
args<-commandArgs(trailingOnly = TRUE)

##default data set when no args provided
fileTrainDataPosts = "./../data/posts-train.txt"
filetrainDataFriends = "./../data/graph.txt"

## parse args for data set
if (length(args) == 2) {
  print(args)
  fileTrainDataPosts <- args[0]
  filetrainDataFriends <- args[1]
} else {
  cat("Using Default dataset", fileTrainDataPosts)
}
 
###Colnames
hour1 <- "Hour1"
id <- "Id"
hour2 <- "Hour2"
hour3 <- "Hour3"
lat <- "Lat"
lon <- "Lon"
posts <- "Posts"


## load data
trainDataPosts <- read.csv(file = fileTrainDataPosts, header = TRUE)
colnames(trainDataPosts) <- c(id, hour1,hour2, hour3, lat, lon, posts)
head(trainDataPosts)

##linear regression model
formula <- as.formula(paste(paste(lat,"~"),paste(c(hour1,hour2, hour3, posts),collapse="+")))
print("Formula used: ")
formula

lm_model <-lm(formula, data= trainDataPosts)
summary(lm_model)
