source("./globals.R")
source("./InstallPackage.R")

pkgInstall("ggplot2")

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

plotModel <- function(dataset, outDir){
  createDir(outDir)
  library(ggplot2)
  pdf(file.path(outDir,'plots.pdf'), onefile = TRUE)
  
  colorGradient <- colorRampPalette(c("lightgreen","blue"))(24)
  
  print(ggplot(data = dataset, aes_string(x = lon, y = lat, col=hour1)) +
          geom_point() +
          scale_colour_gradientn(colours = colorGradient) +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Hour1")
        
  )
  
  print(ggplot(data = dataset, aes_string(x = lon, y = lat, col=hour2)) +
          geom_point() +
          scale_colour_gradientn(colours = colorGradient) +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Hour2")
        
  )
  
  print(ggplot(data = dataset, aes_string(x = lon, y = lat, col=hour3)) +
          geom_point() +
          scale_colour_gradientn(colours = colorGradient) +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Hour3")
        
  )
  
  
  
  dev.off()
  print("Complete")
}


plotTransformedModel <- function(dataset, outDir){
  createDir(outDir)
  
  
  
  library(ggplot2)
  pdf(file.path(outDir,'plotregions.pdf'), onefile = TRUE)
  
  colorGradient <- colorRampPalette(c("green","red", "blue"))(24)
  
  plothr1 <- ggplot(data = dataset, aes_string(x = lon, y = lat, col=hour1)) +
    geom_point() +
    scale_colour_gradientn(colours = colorGradient) +
    theme_bw() +
    theme(legend.position = "top") +
    ggtitle("Hour1")
  
  plothr2 <- ggplot(data = dataset, aes_string(x = lon, y = lat, col=hour2)) +
    geom_point() +
    scale_colour_gradientn(colours = colorGradient) +
    theme_bw() +
    theme(legend.position = "top") +
    ggtitle("Hour2")
  
  plotehr<- ggplot(data = dataset, aes_string(x = lon, y = lat, col=earliestHr)) +
    geom_point() +
    scale_colour_gradientn(colours = colorGradient) +
    theme_bw() +
    theme(legend.position = "top") +
    ggtitle("Earliest Hour")
  
  plotlhr <- ggplot(data = dataset, aes_string(x = lon, y = lat, col=latestHr)) +
    geom_point() +
    scale_colour_gradientn(colours = colorGradient) +
    theme_bw() +
    theme(legend.position = "top") +
    ggtitle("Latest Hour")
  
  print(multiplot(plothr1,plothr2, plotehr, plotlhr, cols=2))
  
  
  print(ggplot(data = dataset, aes_string(x = lon, y = lat, col=totalHr)) +
          geom_point() +
          scale_colour_gradientn(colours = colorGradient) +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Total hour"))
  
  print(ggplot(data = dataset, aes_string(x = lon, y = lat, col=avgHr)) +
          geom_point() +
          scale_colour_gradientn(colours = colorGradient) +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Average HR"))
  
  print(plotehr)
  
  print(plotlhr)
  
  print(ggplot(data = dataset, aes_string(x = closestFriendsLat, y = lat)) +
          geom_point() +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Lat : Closest friend vs me"))
  
  print(ggplot(data = dataset, aes_string(x = closestFriendsLon, y = lon)) +
          geom_point() +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Lon : Closest friend vs me"))
  
  print(ggplot(data = dataset, aes_string(x = avgFriendsLat, y = lat)) +
          geom_point() +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Lat : Avg friends lat vs me"))
  
  print(ggplot(data = dataset, aes_string(x = avgFriendsLon, y = lon)) +
          geom_point() +
          theme_bw() +
          theme(legend.position = "top") +
          ggtitle("Lon : Avg friends lon vs me"))
  
  dev.off()
}

plotFittedModel <- function(lmfit, fileName, outDir){
  createDir(outDir)
  
  
  

  pdf(file.path(outDir,fileName), onefile = TRUE)
  plot(lmfit)
  dev.off()
}