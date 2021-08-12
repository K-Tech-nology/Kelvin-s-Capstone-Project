#import library and load data
library(dplyr)
library(tidyverse)
library(pivottabler)

df <- read.csv("Shipper_stockmovement.csv")

str(df)
#creating function for reporting after data has been extracted and modified
pvt_sm <- function(x) {
  pvt <- PivotTable$new()
  pvt$addData(x)
  pvt$addColumnDataGroups("date")
  pvt$addRowDataGroups("warehouse",addTotal=FALSE)
  pvt$addRowDataGroups("location",addTotal=FALSE)
  pvt$addRowDataGroups("tenant",addTotal=FALSE)
  pvt$addRowDataGroups("Inbound.Outbound",addTotal=FALSE)
  pvt$defineCalculation(calculationName="Total",
                        summariseExpression="sum(qty)")
  pvt$renderPivot()
  
}

pvt_sm(df)
