options(scipen = 100)

setwd("C:/Users/SHASHIKANT/Desktop/R")

data<-read.csv("Satisfaction Survey.csv", stringsAsFactors = FALSE)

str(data)

dataLowSatisfaction<-data[data$Satisfaction<4,]

install.packages("dplyr")
library(dplyr)

dataGroupedByAirlineName<-group_by(dataLowSatisfaction,Airline.Name)

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

dataCleaned<-subset(dataCleaned, select = -c(Airline.Code))


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

str(dataCleaned)

# converting the attributes which are date to the respective format

dataCleaned$Flight.date<-as.Date(dataCleaned$Flight.date,"%m/%d/%y")

str(dataCleaned)

# Now we will look how following factors affect the customer rating

# modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = dataCleaned)
# summary(modelOfAirlineStatus)
# 
# modelOfAge<-lm(formula = Satisfaction~Age, data = dataCleaned)
# summary(modelOfAge)
# 
# modelOfAge<-lm(formula = Satisfaction~Age, data = dataCleaned)
# summary(modelOfAge)

modelOfInfluencingFactors<-lm(formula = Satisfaction~Arrival.Delay.greater.5.Mins+Flight.Distance+Flight.time.in.minutes+Flight.cancelled+Arrival.Delay.in.Minutes+Departure.Delay.in.Minutes+Scheduled.Departure.Hour+Destination.State+Destination.City+Origin.State+Orgin.City+Flight.date+Day.of.Month+Class+Eating.and.Drinking.at.Airport+Shopping.Amount.at.Airport+No..of.other.Loyalty.Cards+Type.of.Travel+X..of.Flight.with.other.Airlines+No.of.Flights.p.a.+Year.of.First.Flight+Price.Sensitivity+Gender+Age+Airline.Status, data = dataCleaned)
summary(modelOfInfluencingFactors)

install.packages("sqldf")
library(sqldf)

name_column <- colnames(dataCleaned)
real_names <- gsub("\\.", "_", name_column)
colnames(dataCleaned) <- real_names

store<-sqldf("Select Age,Shopping_Amount_at_Airport FROM dataCleaned GROUP BY Age;")
str(store)

install.packages("ggplot2")
library(ggplot2)

ggplot(store, aes(x = store$Age, y = store$Shopping_Amount_at_Airport)) + 
  geom_bar(stat = "identity", color = "Black", fill = "Blue") + 
  ggtitle("Average Amount spent at Airport by different age groups") + 
  scale_x_continuous(name="Age") + 
  scale_y_continuous(name = "Average Amount spent") 

plot(x=store$Age,y=store$Shopping_Amount_at_Airport)

