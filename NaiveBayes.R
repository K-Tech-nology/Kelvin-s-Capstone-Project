library(e1071)
library(caTools)
library(caret)
library(dplyr)
library(tidyr)

str(iris)

#splitting dataset
train_test_split <- function(x,train=TRUE,size=0.7){
  total_row = nrow(x)
  row_train = total_row*size
  range = 1:row_train
  if (train == TRUE){
    return (iris[range,])
  } else {
    return (iris[-range,])
  }
}

train <- train_test_split(iris,train=TRUE)
test <- train_test_split(iris,train=FALSE)

#train and test model
train_model <- naiveBayes(Species~.,data=train)
train_model

pred_test <- predict(train_model, newdata = test)

#model evaluation
confusion_matrix <- table(test$Species,pred_test)
confusion_matrix

confusionMatrix(confusion_matrix)