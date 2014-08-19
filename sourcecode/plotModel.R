source("./globals.R")

createDir <- function(path){ 
  if (!file.exists(path)){ 
    dir.create(path)  
  }
}


plotModel <- function(dataset, outDir){
  createDir(outDir)
  pdf(file.path(outDir,'plots.pdf'))
  plot(dataset[,hour1], dataset[,lat], main="Scatterplot", xlab="hour1", ylab="latitude", pch=20)
  plot(dataset[,hour1], dataset[,lon], main="Scatterplot", xlab="hour1", ylab="longitude", pch=20)
  
  plot(dataset[,hour2], dataset[,lat], main="Scatterplot", xlab="hour2", ylab="latitude", pch=20)
  plot(dataset[,hour3], dataset[,lat], main="Scatterplot", xlab="hour3", ylab="latitude", pch=20)
  plot(dataset[,posts], dataset[,lat], main="Scatterplot", xlab="posts", ylab="latitude", pch=20)
  dev.off()
}