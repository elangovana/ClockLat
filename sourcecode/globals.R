###Colnames
hour1 <- "Hour1"
id <- "Id"
hour2 <- "Hour2"
hour3 <- "Hour3"
lat <- "Lat"
lon <- "Lon"
posts <- "Posts"

colHeaders <-  c(id, hour1,hour2, hour3, lat, lon, posts)

createDir <- function(path){ 
  if (!file.exists(path)){ 
    dir.create(path)  
  }
}
