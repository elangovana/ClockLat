source("./globals.R")



plotModel <- function(dataset, outDir){
  createDir(outDir)
  
  pdf(file.path(outDir,'plots.pdf'))
  
  plot(dataset[,hour1], dataset[,lat], main="Hour1 vs Latitude", xlab="hour1", ylab="latitude", pch=20)
  plot(dataset[,hour1], dataset[,lon], main="Hour1 vs longitude", xlab="hour1", ylab="longitude", pch=20)
  
  plot(dataset[,hour2], dataset[,lat], main="Hour2 Vs latitude", xlab="hour2", ylab="latitude", pch=20)
  plot(dataset[,hour2], dataset[,lon], main="Hour2 Vs longitude", xlab="hour2", ylab="longitude", pch=20)
  
  plot(dataset[,hour3], dataset[,lat], main="Hour3 Vs latitude", xlab="hour3", ylab="latitude", pch=20)
  plot(dataset[,hour3], dataset[,lon], main="Hour3 Vs longitude", xlab="hour3", ylab="longitude", pch=20)
  
  plot(dataset[,posts], dataset[,lat], main="posts vs latitude", xlab="posts", ylab="latitude", pch=20)
  plot(dataset[,posts], dataset[,lon], main="posts vs Longitude", xlab="posts", ylab="longitude", pch=20)
  
  plot(dataset[,hour1], dataset[,hour2], main="Hour1 vs Hour2", xlab="hour1", ylab="hour2", pch=20)  
  plot(dataset[,hour2], dataset[,hour3], main="Hour2 vs Hour3", xlab="hour2", ylab="hour3", pch=20)  
  plot(dataset[,hour1], dataset[,hour3], main="Hour1 vs Hour3", xlab="hour1", ylab="hour3", pch=20)
  
  plot(dataset[,lon], dataset[,lat], main="lattitude vs longitude", xlab="lon", ylab="lat", pch=20)
  
  
  dev.off()
}