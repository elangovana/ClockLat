source("./globals.R")
source("./InstallPackage.R")


createFeatureMaxMin <- function(trainData){
  trainData[, latestHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, max)
  trainData[, earliestHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, min)
  print(head(trainData))
  return(trainData) 
}

createFeatureAvgTotal <- function(trainData){
  trainData[, avgHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, mean)
  trainData[, totalHr] <- apply(trainData[, c(hour1, hour2, hour3)], 1, sum)
  print(head(trainData))
  return(trainData) 
}


transformFeatures <- function(data, outDir){
  data <- createFeatureMaxMin(data)  
  data <- createFeatureAvgTotal(data)
  write.table(data,  file= file.path(outDir, "transformedData.csv"),  row.names = FALSE, sep=",", quote = FALSE)

  return(data) 
}