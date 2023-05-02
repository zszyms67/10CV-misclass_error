## required packages for state transition diagram 
library(nnet)
library(randomForest)
library(neuralnet)

###########
## @Description           a simple function to obtain 10-CV misclassification error for Logistic regression, random forest, and neural network 
## @k                     matrix or dataframe of predictor and single response variable  
## @model                 specified prediction model to fit to data 
##                        "log" , "rforest", "nnet" 
## @return                returns 10-CV misclassification error 
############  


cv <- function(data, model) {
  
  set.seed(123)
  m <- nrow(data)
  j <- sample(1:m, 105, replace = FALSE)
  train <- data[j,]
  test <-  data[-j,]
  cv_error <- c()
  
  for (i in 1:10) {
    
    if (model == "log") {
      
      fit <- glm(Species ~ ., data = train, family = "binomial")
      pred <- predict(fit, newdata = test, type = "response") 
      pred <- ifelse(pred > 0.5, "versicolor", "setosa")
      cv_error[i]<- sum(test$Species != pred)/nrow(test)
      
    } else if (model == "rforest") {
      
      fit <- randomForest(Species ~ ., data = train)
      pred <- predict(fit, newdata = test, type = "class")
      cv_error[i]<- sum(test$Species != pred)/nrow(test)
      
    } else if (model == "nnet") {
      
      fit <- nnet(Species ~ ., data = train, size = 1, maxit = 10000)
      pred <- predict(fit, newdata = test, type = "class")
      cv_error[i]<- sum(test$Species != pred)/nrow(test)
      
    }
  }
  return(mean(cv_error))
}
