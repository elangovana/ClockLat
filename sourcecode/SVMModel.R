source("./globals.R")

pkgInstall("e1071")

#######SVM###########
####################
####################

calcSVM<-function(postsData, testData, outDir){
  createDir(outDir)
  
  factors <- paste(c(hour1),collapse="+")
  
  #Region
  formula <- as.formula(paste(paste(region,"~"),factors))
  print("Formula for region used: ")
  print(formula)
  svm_model<-svm(formula, data= postsData)
  print(summary(svm_model))
  predicted_reg <-predict(svm_model, newdata=testData)
  
  data <- cbind(testData, predicted_reg)
  colnames(data) <- c(id, hour1,hour2, hour3,posts,region)
  
    
  write.table(data,  file= file.path(outDir, "labelledcontinentsforTest.csv"),  row.names = FALSE, sep=",", quote = FALSE)
  return(data)
}