#load library
library(dplyr)
library(tidyverse)
library(ggplot2)

data <- read.csv("Shipper_stockmovement.csv")

data$date <- as.POSIXct(data$date, format="%m/%d/%Y")
str(data)

colnames(data)

first <- data %>%
  select("date","Inbound.Outbound","warehouse","location","tenant","qty","total.price") %>%
  group_by(date,Inbound.Outbound,warehouse,location,tenant) %>%
  summarise(Total_Qty = sum(qty),
            Total_Price = sum(total.price))

first$date <- as.Date(first$date, format = "%Y-%m-%d")
first <- rename(first, "Inbound_Outbound" = "Inbound.Outbound")

library(scales)
ggplot(first, aes(x=date, y=Total_Qty, colour=Inbound_Outbound)) +
  geom_line() +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month"
  ) + facet_wrap(~ location,ncol=2) +
  labs(
    title = "Warehouse Location Month on Month Stock Movement",
    x = "Date",
    y = "Total Quantity"
  )