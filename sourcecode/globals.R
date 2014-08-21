###Colnames
hour1 <- "Hour1"
id <- "Id"
hour2 <- "Hour2"
hour3 <- "Hour3"
lat <- "Lat"
lon <- "Lon"
posts <- "Posts"
region <- "Region"
latestHr <- "EarliestHour"
earliestHr <- "LatestHour"
colInputTrainHeaders <-  c(id, hour1,hour2, hour3, lat, lon, posts)
colInputTestHeaders <-  c(id, hour1,hour2, hour3,  posts)

createDir <- function(path){ 
  if (!file.exists(path)){ 
    dir.create(path)  
  }
}

writePredicationAsCsv <- function(testData, predicted_lat, predicted_lon, outdir, filenamePrefix){
  createDir(outDir)
  
  data <- cbind(predicted_lat,predicted_lon)
  data <- cbind(testData, data)
  names(data)[names(data) == 'predicted_lat'] <- lat
  names(data)[names(data) == 'predicted_lon'] <- lon
  
  print(file.path(outDir, paste( c(filenamePrefix,"submission",'.csv'), collapse="")))
  
  write.csv(data[, c(id, lat,lon)], file= file.path(outDir, paste( c(filenamePrefix,"submission",'.csv'), collapse="")), row.names = FALSE,  quote = FALSE) 
  write.table(data,  file= file.path(outDir, paste( c(filenamePrefix,'.csv'), collapse="")),  row.names = FALSE, sep=",", quote = FALSE)  
}
