library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart.plot)
library(rplot)

df <- read.csv("winequality-red.csv")
sapply(df, function(x){sum(is.na(x))})

count(df, vars = quality) #checking unique values for target variable

#first method to change into categorical value
df_new <- df %>%
  mutate(quality = factor(quality, levels = c(3,4,5,6,7,8), labels = c("Low","Low","Medium","Medium","High","High")))

#second method to change into categorical value
df$quality_c <- df$quality
df$quality_c <- ifelse(df$quality_c <= 4, "Low", ifelse(df$quality_c > 4 & df$quality_c <= 6, "Medium", "High") )
df_new <- df %>%
  select(-quality)

#creating train and test dataset
train_test_f <- function(data, size=0.8, train=TRUE){
  n_row = nrow(data)
  total_row = size*n_row
  sample <- 1:total_row
  if (train == TRUE){
    return (data[sample,])
  } else {
    return (data[-sample,])
  }
}

train <- train_test_f(df_new, 0.8, train=TRUE)
test <- train_test_f(df_new,0.8,train=FALSE)

prop.table(table(train$quality_c))

#train the model using rpart and rpart.plot
model <- rpart(quality_c~., data=train, method='class')
rpart.plot(model, extra = 106)

predict_test <- predict(model, test, type='class')
table_mat <- table(test$quality_c, predict_test)
table_mat

accuracy <- sum(diag(table_mat))/sum(table_mat)
accuracy