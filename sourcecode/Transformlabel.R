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




labelContinents <- function(dataset, outDir){
 indices <-  coords2continent(dataset[, c(lon, lat)])
 
 print(head(indices$REGION))
 data<- cbind(dataset, indices$REGION)
 names(data)[names(data) == 'indices$REGION'] <- region
 
 write.table(data,  file= file.path(outDir, "labelledcontinents.csv"),  row.names = FALSE, sep=",", quote = FALSE)
 return(data)
}