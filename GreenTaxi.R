#Import Library and Load data
library(dplyr)
library(tidyr)
library(ggplot2)

df <- list.files(path="/Users/kelvinprawtama/Desktop/GreenTaxiNYC", pattern = "*.csv", full.names = TRUE) %>% lapply(read.csv) %>% bind_rows
dim(df)

#add new column with date only
df$pu_date <- df$lpep_pickup_datetime
df$pu_date <- as.POSIXct(df$pu_date, format = "%Y-%m-%d %H:%M:%S")
df$pu_date <- format(df$pu_date, format = "%Y-%m-%d")

#selecting columns for analysis 
df_new <- df %>%
  select("pu_date", "VendorID", "RatecodeID",
         "payment_type", "trip_type", "trip_distance", 
         "total_amount") 

#check for any missing values and remove if any
sapply(df_new, function(x) sum(is.na(x)))

df_nonNA <- df_new[rowSums(is.na(df_new))==0,]
sapply(df_nonNA, function(x) sum(is.na(x)))

#Getting data from 2020
df_nonNA <- df_nonNA[df_nonNA$pu_date >= '2020-01-01',]

#Analyze and Visualize
#Calculating Current Market Share 
market_share <- df_nonNA %>%
  select("VendorID","total_amount") 
  
market_share[market_share$VendorID == 1, 1] <- "Creative Mobile Technologies"
market_share[market_share$VendorID == 2, 1] <- "Verifone"

market_share <- market_share %>%
  group_by(VendorID) %>% summarize(Total = sum(total_amount))

market_share$Percentage <- market_share$Total/sum(market_share$Total)

#plot the result for market share
labels <- paste(round(100*market_share$Percentage, 2), "%", sep="")
ggplot(market_share, aes(x="", y=round(Percentage*100,0),fill=VendorID)) +
  geom_col() +
  geom_text(aes(label = labels),position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Vendor Name",
       x = NULL,
       y = NULL,
       title = "2020 NYC Green Taxi Market Share") +
  coord_polar(theta = "y")


#Ratecode and payment for understanding which rate and payment method has the highest preferences from demand
rate_share <- df_nonNA %>%
  select("RatecodeID","payment_type","total_amount") 

rate_share <- rate_share[rate_share$RatecodeID != 99,]

rate_share <- as.Character(rate_share$RatecodeID)
rate_share[rate_share$RatecodeID == "1",1] <- "Standard Rate"
rate_share[rate_share$RatecodeID == "2",1] <- "JFK"
rate_share[rate_share$RatecodeID == "3",1] <- "Newark"
rate_share[rate_share$RatecodeID == "4",1] <- "Nassau or Westchester"
rate_share[rate_share$RatecodeID == "5",1] <- "Negotiated Fare"
rate_share[rate_share$RatecodeID == "6",1] <- "Group Ride"

rate_share[rate_share$payment_type == "1",2] <- "Credit Card"
rate_share[rate_share$payment_type == "2",2] <- "Cash"
rate_share[rate_share$payment_type == "3",2] <- "No Charge"
rate_share[rate_share$payment_type == "4",2] <- "Dispute"
rate_share[rate_share$payment_type == "5",2] <- "Unknown"
  
rate_share <- rate_share %>%
  group_by(RatecodeID,payment_type) %>%
  summarise(Total = sum(total_amount))

rate_share$Percentage <- round((rate_share$Total/sum(rate_share$Total))*100, 2)

#plot rate_share result
ggplot(rate_share,aes(x=RatecodeID,y=Percentage,fill=payment_type)) +
 geom_bar(stat="identity") +
 labs(fill = "Payment Type",
      x = "Rate Type",
      y = "Percentage",
      title = "2020 NYC Green Taxi Rate and Payment Type")

#Calculate trip type percentage based on revenue
trip_share <- df_nonNA %>%
  select("trip_type","total_amount")

trip_share[trip_share$trip_type == 1,1] <- "Street-Hail"
trip_share[trip_share$trip_type == 2,1] <- "Dispatch"

trip_share <- trip_share %>%
  group_by(trip_type) %>%
  summarise(Total = sum(total_amount))

trip_share$Percentage <- round((trip_share$Total/sum(trip_share$Total))*100,0)

#Plot trip_share result
label_2 <- paste(trip_share$Percentage,"%",sep="")
ggplot(trip_share,aes(x="",y=Percentage,fill=trip_type)) +
  geom_col() +
  geom_text(aes(label = label_2), position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Trip Type",
       x = NULL,
       y = NULL,
       title = "2020 NYC Green Taxi Trip Share") +
  coord_polar(theta="y")

#Time Series Green Taxi Revenue Changes
time_series <- df_nonNA %>%
  select("pu_date","total_amount")

time_series$Month <- as.POSIXct(time_series$pu_date,format="%Y-%m-%d")
time_series$Month <- as.Date(cut(time_series$Month, breaks="month"))

time_series <- time_series[,c("pu_date","Month","total_amount")]

time_series_date <- time_series %>%
  select("pu_date","total_amount") %>%
  group_by(pu_date) %>%
  summarise(Total=sum(total_amount))

time_series_date$Month <- as.POSIXct(time_series_date$pu_date,format="%Y-%m-%d")
time_series_date$Month <- as.Date(cut(time_series_date$Month, breaks="month"))

time_series_date <- time_series_date[time_series_date$Month < "2021-01-01",]

#Plot Month over Month Revenue
library(scales)
ggplot(time_series_date, aes(x=Month,y=Total), fill=Total) +
 stat_summary(fun = sum,
              geom = "line") +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month"
  ) +
  labs(fill=NULL,
       title="2020 NYC Green Taxi MoM Revenue")

#Showing daily changes in each month
library(lubridate)
time_series_month <- time_series %>%
  mutate(
    Month=month(pu_date))

time_series_month <- time_series_month[time_series_month$pu_date < "2021-01-01",]
time_series_month <- time_series_month %>%
  group_by(pu_date, Month) %>%
  summarise(Total=sum(total_amount))

time_series_month$Day <- day(time_series_month$pu_date) 

ggplot(time_series_month,aes(x=Day,y=Total)) +
  geom_line(group=1) +
  facet_wrap(~ Month, ncol = 3) +
  labs(x=NULL,
       title="2020 NYC Green Taxi Revenue per Month")

#Time Series Modeling
#Split training and test data set
training <- time_series_date[,c("pu_date","Total")]
training$pu_date <- as.POSIXct(training$pu_date,format="%Y-%m-%d")
training$pu_date <- as.Date(cut(training$pu_date,breaks = "month"))

training <- training %>%
  group_by(pu_date) %>%
  summarise(Total=sum(Total))

library(forecast)
auto_arima <- auto.arima(training[,2])
arima_forecast <- forecast(auto_arima,h=10)
plot(arima_forecast)

require(tseries)
time_s <- ts(training[,2], start = c(2020,1), end = c(2020,12), frequency = 12)
fit <- arima(time_s, order = c(0,1,0))
pred <- predict(fit, n.ahead = 10)
ts.plot(time_s, pred$pred, lty = c(1,3), col=c(5,2)) 