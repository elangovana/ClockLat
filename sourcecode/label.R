source("./globals.R")
source("./InstallPackage.R")

pkgInstall("rworldmap")


library(sp)
library(rworldmap)

coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
 

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  

  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  return(indices) 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

plotMappedModel <- function(dataset, outDir){
  createDir(outDir)
  
  pdf(file.path(outDir,'plotregions.pdf'))
  
  plot(dataset[,hour1], dataset[,region], main="Hour1 vs region", xlab="hour1", ylab="region")
  plot(dataset[,region], dataset[,hour1], main="Hour1 vs region", xlab="region", ylab="hour1")
  
  plot(dataset[,posts], dataset[,region], main="posts vs region", xlab="posts", ylab="region")
  plot(dataset[,region], dataset[,posts], main="posts vs region", xlab="region", ylab="posts")
  
  plot(dataset[,region], dataset[,hour2], main="Hour2 Vs region", xlab="hour2", ylab="region")
 
  
  
  dev.off()
}


labelContinents <- function(trainData, testData, outDir){
 indices <-  coords2continent(trainData[, c(lon, lat)])
 
 print(head(indices$REGION))
 data<- cbind(trainData, indices$REGION)
 colnames(data)[8] = region

 plotMappedModel(data, outDir)
 write.table(data,  file= file.path(outDir, "labelledcontinents.csv"),  row.names = FALSE, sep=",", quote = FALSE)
 return(data)
}