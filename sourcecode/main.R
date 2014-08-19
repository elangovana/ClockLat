###Colnames
hour1 <- "Hour1"
id <- "Id"
hour2 <- "Hour2"
hour3 <- "Hour3"
lat <- "Lat"
lon <- "Lon"
posts <- "Posts"


############################
##linear regression model##
#############################
calcLinearRegression<- function(postsData){

  factors <- paste(c(hour1,hour2, hour3, posts),collapse="+")
  
  ## latitude
  formula <- as.formula(paste(paste(lat,"~"),factors))
  print("Formula for lat used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  
  #longitude
  
  formula <- as.formula(paste(paste(lon,"~"),factors))
  print("Formula for longitude used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  lm_model
  
  
}

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
