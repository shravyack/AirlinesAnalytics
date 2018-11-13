options(scipen = 100)

# setwd("C:/Users/SHASHIKANT/Desktop/R")

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

# modelOfInfluencingFactors<-lm(formula = Satisfaction ~ Arrival_Delay_greater_5_Mins+Flight_Distance+Flight_time_in_minutes+Flight_cancelled+Arrival_Delay_in_Minutes+Departure_Delay_in_Minutes+Scheduled_Departure_Hour+Destination_State+Destination_City+Origin_State+Orgin_City+Flight_date+Day_of_Month+Class+Eating_and_Drinking_at_Airport+Shopping_Amount_at_Airport+No__of_other_Loyalty_Cards+Type_of_Travel+X__of_Flight_with_other_Airlines+No_of_Flights_p_a_+Year_of_First_Flight+Price_Sensitivity+Gender+Age+Airline_Status, data = dataCleaned)
# summary(modelOfInfluencingFactors)

install.packages("sqldf")
library(sqldf)

# Replacing .(dots) by _(underscore) for easy analysis 

name_column <- colnames(dataCleaned)
real_names <- gsub("\\.", "_", name_column)
colnames(dataCleaned) <- real_names


# Checking the effect of avg shop amt spend on different age groups
AgeVsShop <-sqldf("Select Age,count(age),avg(Shopping_Amount_at_Airport) AS Shopping_Amount_at_Airport FROM dataCleaned GROUP BY Age;")
str(AgeVsShop)

# Checking the effect of avg shop amt spend on different age groups

AvgVsE_D <-sqldf("Select Age,count(age), avg(Eating_and_Drinking_at_Airport) AS Eating_and_Drinking_at_Airport FROM dataCleaned GROUP BY Age;")
str(AvgVsE_D)

install.packages("ggplot2")
library(ggplot2)

# Plotting line graph of Age VS Shopping Amount

ggplot(AgeVsShop, aes(x = AgeVsShop$Age, y = AgeVsShop$Shopping_Amount_at_Airport)) + 
  geom_line(stat = "identity", color = "Black") + 
  ggtitle("Age vs Shopping Amount Spent") + 
  scale_x_continuous(name="Age") + 
  scale_y_continuous(name = "Average Amount spent")

# Plotting line graph of Age VS Eating and Drinking Amount

ggplot(AvgVsE_D, aes(x = AvgVsE_D$Age , y = AvgVsE_D$Eating_and_Drinking_at_Airport )) + 
  geom_line(stat = "identity", color = "Blue") + 
  ggtitle("Age vs Eating & Drinking amount") + 
  scale_x_continuous(name="Age") + 
  scale_y_continuous(name = "Average Amount spent")

# 
# ggplot(dataCleaned, aes(x = dataCleaned$Age, y = dataCleaned$Shopping_Amount_at_Airport)) +
#   geom_point(size = 3) +
#   ggtitle("Average Amount spent in Shopping at Airport by different age groups") + 
#   scale_x_continuous(name="Age") + 
#   scale_y_continuous(name = "Average Amount spent")

# plot(x=store$Age,y=store$Shopping_Amount_at_Airport)

ggplot(store, aes(x = store1$Age, y = store1$Eating_and_Drinking_at_Airport)) + 
  geom_bar(stat = "identity", color = "Black", fill = "Yellow") + 
  ggtitle("Average Amount spent in Eating & Driking at Airport by different age groups") + 
  scale_x_continuous(name="Age") + 
  scale_y_continuous(name = "Average Amount spent")
