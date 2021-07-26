#load necessary library
library(tidyverse) #for data manipulation and visualization
library(lubridate) #for datetime data
library(stringr) #for string data
library(forecast) #for time series prediction

#load the data
df <- read.csv("supermarket_sales.csv")

#check for any missing values
sapply(df, function(x) sum(is.na(x)))

#creating new column name sales
df$sales <- df$Quantity * df$Unit.price

#select required cols for analysis and prediction
df_sales_normal<- df %>%
    select("Date","Customer.type","sales") %>%
    filter(df$Customer.type == "Normal") %>%
    group_by(Date,Customer.type) %>%
    summarise(Total_Sales = sum(sales))

#adding new column for %Y-%m
df_sales_normal$Month <- mdy(df_sales_normal$Date) %>%
    format(format="%Y-%m-%d")

#tidying up our data frame
df_sales_normal <- subset(df_sales_normal, select = -c(Date))
df_sales_normal <- df_sales_normal[,c("Month","Total_Sales")]
df_sales_normal$Month <- as.Date(df_sales_normal$Month)

#Analyze and Visualize our data
library(ggplot2)
ggplot(df_sales_normal, aes(x=Month,y=Total_Sales)) + 
geom_line(col = 'hotpink') +
ylim(0,6000) + xlab("Time") + ylab("Total Sales") +
scale_x_date(date_labels = "%Y %m", date_breaks = "1 month") +
theme_bw() + theme(legend.title = element_blank(),
                   axis.text.x = element_text(angle = 45, vjust = 0.5))

#check the correlation between time and sales
par(mfrow=c(1,2))
acf(as.ts(df_sales_normal$Total_Sales), main="Total_Sales")
pacf(as.ts(df_sales_normal$Total_Sales), main="Total_Sales")

#split the data set for training and testing
train_index <- 30
n_total <- nrow(df_sales_normal)
df_train <- df_sales_normal[1:(train_index),]
df_test <- df_sales_normal[(train_index+1):n_total,]
predicted <- numeric(n_total-train_index)

#train the data
for (i in 1:(n_total-train_index)) {
    df_train_1 <- df_train[1:(train_index-1+i),]
    arima_model <- auto.arima(as.ts(df_train_1$Total_Sales))
    pred <- forecast(arima_model,1)
    predicted <- pred$mean
}

#contain It in tibble df
df_pred <- tibble(obs = c(df_train$Total_Sales, df_test$Total_Sales),
                  predicted = c(df_train$Total_Sales, predicted),
                  time = df_sales_normal$Month)

#Plot the result
ggplot(gather(df_pred,obs_pred,value,-time) %>%
       mutate(obs_pred = factor(obs_pred, levels = c("predicted","obs"))),
       aes(x=time, y=value, col=obs_pred,linetype=obs_pred)) +
geom_line() + xlab("") + ylab("") +
scale_color_manual(values=c("black","hotpink")) +
scale_linetype_manual(values=c(2,1)) +
scale_x_date(date_labels = "%y %b", date_breaks = "1 month") +
theme_bw() + theme(legend.title = element_blank(),
                   axis.text.x = element_text(angle=45, vjust = 0.5))
