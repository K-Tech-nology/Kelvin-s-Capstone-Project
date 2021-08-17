#import library and load dataset
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(mice)
library(lattice)
library(reshape2)
library(DataExplorer)

#Data Wrangling
df <- read.csv("UCI_Credit_Card.csv")

sapply(df, function(x){sum(is.na(x))})

count(df, vars=SEX)
count(df, vars=EDUCATION)
count(df, vars=MARRIAGE)

df[df$EDUCATION==0,4] <- 4
df[df$EDUCATION==5,4] <- 4
df[df$EDUCATION==6,4] <- 4
df$MARRIAGE[df$MARRIAGE==0] <- 3

df[,1:25] <- sapply(df[,1:25], as.numeric)
str(df)
summary(df)
colnames(df)[colnames(df)=="default.payment.next.month"] <- "default_payment"

plot_correlation(df)
plot_histogram(df)

df_sc <- df %>%
  select(-one_of("ID",'AGE','BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'))

#Pre-processing
colnames(df_sc)
df_sc[,1:17] <- scale(df_sc[,1:17])
head(df_sc)

#create training and test data
df2 <- sort(sample(nrow(df_sc),nrow(df_sc)*.7))

train <- df_sc[df2,]
test <- df_sc[-df2,]

dim(train)
dim(test)

#model development
log.model <- glm(default_payment ~., data = train, family=binomial(link = "logit"))
summary(log.model)

#Prediction
log.predictions <- predict(log.model,test, type="response")
head(log.predictions,10)

log.predictions.rd <- ifelse(log.predictions > 0.5, 1, 0)
head(log.predictions.rd, 10)

accuracy <- table(log.predictions.rd, test[,17])
sum(diag(accuracy))/sum(accuracy)
