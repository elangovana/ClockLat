
source("./globals.R")
source("./InstallPackage.R")

pkgInstall("nnet")




calcMutinomialLogisiticRegression <- function(postsLabelledData, testData, outDir){
  library(nnet)
  factors <- paste(c(hour1,hour2, hour3, posts),collapse="+")
  

  formula <- as.formula(paste(paste(region,"~"),factors))
  print("Formula for classsification used: ")
  print(formula)
  
  mod <- multinom(formula, postsLabelledData)
  probs <- predict(mod, testData, "probs")
  
  print(head(probs))
  cum.probs <- t(apply(probs,1,cumsum))
  
  # Draw random values
  vals <- runif(nrow(testData))
  
  # Join cumulative probabilities and random draws
  tmp <- cbind(cum.probs,vals)
  
  print(head(cbind(testData, tmp)))
  # For each row, get choice index.
  k <- ncol(probs)
  print(k)
  ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
  
  predicated_region <- cbind(testData,y=ids)
  print(head(predicated_region))
}
