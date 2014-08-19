source("./globals.R")

############################
##linear regression model##
#############################
calcLinearRegression<- function(postsData){
  
  factors <- paste(c(hour1,hour2, hour3, posts),collapse="+")
  
  ## latitude
  formula <- as.formula(paste(paste(lat,"~"),factors))
  print("Formula for lat used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  
  #longitude 
  formula <- as.formula(paste(paste(lon,"~"),factors))
  print("Formula for longitude used: ")
  print(formula)
  lm_model <-lm(formula, data= postsData)
  print(summary(lm_model))
  lm_model
  
  
}