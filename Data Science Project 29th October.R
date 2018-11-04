setwd("C:/Users/SHASHIKANT/Desktop/R")

data<-read.csv("Satisfaction Survey.csv", stringsAsFactors = FALSE)

str(data)

dataLowSatisfaction<-data[data$Satisfaction<4,]

install.packages("dplyr")
library(dplyr)

dataGroupedByAirlineName<-group_by(dataLowSatisafaction,Airline.Name)

dataSummarised<-summarise(dataGroupedByAirlineName ,LowSatisfiedEntries=n())

View(dataSummarised)

dataGroupedByAirlineNameOriginalData<-group_by(data, Airline.Name)

dataSummarised2<-summarise(dataGroupedByAirlineNameOriginalData, Total.Entries=n())

View(dataSummarised2)

dataCompare<-merge(dataSummarised,dataSummarised2)

dataCompare$Relative<-(dataCompare$LowSatisfiedEntries/dataCompare$Total.Entries)*100

View(dataCompare)

## We have chosen Cheapset as relatively it has more low satisfied customers

dataCleaned<-data[(trimws(data$Airline.Name,which="right")=="Cheapseats Airlines Inc."),]

rownames(dataCleaned)<-NULL

dataCleaned<-subset(dataCleaned, select = -c(Airline.Name))

# d<-data.frame(lapply(dataCleaned, trimws), stringsAsFactors = FALSE)

# str(d)

# grepl("^\\s*$", dataCleaned)

apply(dataCleaned,2, function(x) any(is.na(x)))

# Now we come to know that only three columns contain NA values
# Those are 1. Departure.Delay.in.Minutes 2. Arrival.Delay.in.Minutes 3.Flight.time.in.minutes
# Rest columns are free from NA

# d<-dataCleaned

dataCleaned[is.na(dataCleaned)]<-9999 # Dirty value

apply(dataCleaned,2, function(x) any(is.na(x)))

# No column contains NA
