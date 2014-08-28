source("./globals.R")
source("./InstallPackage.R")

pkgInstall("e1071")

#######SVM###########
####################
####################

calcSVM<-function(postsData, testData, outDir){
  createDir(outDir)
  
  factors <- paste(c(earliestHr, latestHr),collapse="+")
  
  #Region
  formula <- as.formula(paste(paste(region,"~"),factors))
  print("Formula for region used: ")
  print(formula)
  svm_model<-svm(formula, data= postsData)
  print(summary(svm_model))
  predicted_reg <-predict(svm_model, newdata=testData)
  
  print(testData)
  print(predicted_reg)
  print(dim(predicted_reg))
 print(typeof(predicted_reg))
  print(length(predicted_reg))
 print(names(predicted_reg))
 
 predicted_reg <- data.frame(row.names = names(predicted_reg), predicted_reg)
 print(predicted_reg)
 print(dim(predicted_reg))
  data <- merge( testData, predicted_reg, by.x=0, by.y=0)

  
  print(head(data))
  names(data)[names(data) == 'predicted_reg'] <- predictedRegion

    
  write.table(data,  file= file.path(outDir, "labelledcontinentsforTest.csv"),  row.names = FALSE, sep=",", quote = FALSE)
  return(data)
}