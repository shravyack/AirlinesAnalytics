options(scipen = 100)

#setwd("D:/Vidisha_Assignments/IST 687 - Introduction to Data Science")
setwd("C:/Users/SHASHIKANT/Desktop/R")

data<-read.csv("Satisfaction Survey.csv", stringsAsFactors = FALSE)

str(data)

dataLowSatisfaction<-data[data$Satisfaction<4,]

install.packages("dplyr")
library(dplyr)

dataGroupedByAirlineName<-group_by(dataLowSatisfaction,Airline.Name)

dataSummarised<-summarise(dataGroupedByAirlineName ,LowSatisfiedEntries=n())

#View(dataSummarised)

dataGroupedByAirlineNameOriginalData<-group_by(data, Airline.Name)

dataSummarised2<-summarise(dataGroupedByAirlineNameOriginalData, Total.Entries=n())

#View(dataSummarised2)

dataCompare<-merge(dataSummarised,dataSummarised2)

dataCompare$Relative<-(dataCompare$LowSatisfiedEntries/dataCompare$Total.Entries)*100

#View(dataCompare)

# Pie Chart for dataCompare

slicesR<-dataCompare$Relative
slicesT<-dataCompare$Total.Entries
names<-dataCompare$Airline.Name
namesR<-paste(names,slicesR)
namesR<-paste(namesR,"%", sep = "")
pie(slicesR,labels = namesR, color=rainbow(length(namesR)) ,main="Pie Chart for % of low satisfied customers")
namesT<-paste(names,slicesT)
pie(slicesT,labels = namesT, color=rainbow(length(namesT)) ,main="Pie Chart for Total Airline entries")

## We have chosen Cheapset as it has relatively more low satisfied customers

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

# Visualizing the Satisfaction column

hist(as.numeric(dataCleaned$Satisfaction))

# Checking the effect of avg shop amt spend on different age groups
AgeVsShop <-sqldf("Select Age,count(age),avg(Shopping_Amount_at_Airport) AS Shopping_Amount_at_Airport FROM dataCleaned GROUP BY Age;")
#str(AgeVsShop)

# Checking the effect of avg shop amt spend on different age groups

AvgVsE_D <-sqldf("Select Age,count(age), avg(Eating_and_Drinking_at_Airport) AS Eating_and_Drinking_at_Airport FROM dataCleaned GROUP BY Age;")
#str(AvgVsE_D)

# Performing descriptive statistics

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

# Plot of Airline Status Vs Satisfaction
statusVsSatisfaction <- table(dataCleaned$Satisfaction, dataCleaned$Airline_Status)
statusVsSatisfaction_df <- data.frame(statusVsSatisfaction)
colnames(statusVsSatisfaction_df) <- c("Satisfaction", "Status", "Frequency")
statusVsSatisfaction_plot <- ggplot(statusVsSatisfaction_df, aes(y = jitter(statusVsSatisfaction_df$Frequency,20), x = statusVsSatisfaction_df$Status))
statusVsSatisfaction_plot <- statusVsSatisfaction_plot + geom_point(aes(color = statusVsSatisfaction_df$Satisfaction, size = 1)) + guides(color = guide_legend(title = "Satisfaction Rate"))
statusVsSatisfaction_plot <- statusVsSatisfaction_plot + scale_x_discrete("Airline Status") + scale_y_continuous("Frequency of each Status") + ggtitle("Plot of Airline Status Vs Satisfaction")
statusVsSatisfaction_plot

# Plot of Price Sensitivity Vs Satisfaction
satisfactionVsSensitivity <- table(dataCleaned$Satisfaction, dataCleaned$Price_Sensitivity)
satisfactionVsSensitivity_df <- data.frame(satisfactionVsSensitivity)
colnames(satisfactionVsSensitivity_df) <- c("Satisfaction", "Sensitivity", "Frequency")
satisfactionVsSensitivity_plot <- ggplot(satisfactionVsSensitivity_df, aes(x = Sensitivity, y = Frequency, group = Satisfaction))
satisfactionVsSensitivity_plot <- satisfactionVsSensitivity_plot + geom_line(aes(color = satisfactionVsSensitivity_df$Satisfaction)) + guides(color = guide_legend(title = "Satisfaction Rate"))
satisfactionVsSensitivity_plot <- satisfactionVsSensitivity_plot + scale_x_discrete("Price Sensitivity (Values: 0 - 4)") + scale_y_continuous("Frequency") + ggtitle("Plot of Satisfaction Vs Price Sensitivity")
satisfactionVsSensitivity_plot

# Plot of Price Sensitivity Vs Airline status
statusVsSensitivity <- table(dataCleaned$Airline_Status, dataCleaned$Price_Sensitivity)
statusVsSensitivity_df <- data.frame(statusVsSensitivity)
colnames(statusVsSensitivity_df) <- c("Status", "Sensitivity", "Frequency")
statusVsSensitivity_plot <- ggplot(statusVsSensitivity_df, aes(x = Sensitivity, y = Frequency, group = Status))
statusVsSensitivity_plot <- statusVsSensitivity_plot + geom_line(aes(color = statusVsSensitivity_df$Status)) + guides(color = guide_legend(title = "Status"))
statusVsSensitivity_plot <- statusVsSensitivity_plot + scale_x_discrete("Price Sensitivity (Values: 0 - 4)") + scale_y_continuous("Frequency") + ggtitle("Plot of Airline status Vs Price Sensitivity")
statusVsSensitivity_plot

# Plot of Number of flights per annum Vs Satisfaction
noofflightsVsSatisfaction <- table(dataCleaned$Satisfaction, dataCleaned$No_of_Flights_p_a_)
noofflightsVsSatisfaction_df <- data.frame(noofflightsVsSatisfaction)
colnames(noofflightsVsSatisfaction_df) <- c("Satisfaction", "NumberOfFlights", "Frequency")
noofflightsVsSatisfaction_df$Satisfaction <- as.factor(noofflightsVsSatisfaction_df$Satisfaction)
noofflightsVsSatisfaction_df$NumberOfFlights <- as.numeric(noofflightsVsSatisfaction_df$NumberOfFlights)
noofflightsVsSatisfaction_df <- noofflightsVsSatisfaction_df[noofflightsVsSatisfaction_df$Frequency > 0,]
noofflightsVsSatisfaction_plot <- ggplot(noofflightsVsSatisfaction_df, aes(x = NumberOfFlights, y = jitter(Frequency, 30)))
noofflightsVsSatisfaction_plot <- noofflightsVsSatisfaction_plot + geom_point(aes(color = noofflightsVsSatisfaction_df$Satisfaction, size = 0)) + guides(color = guide_legend(title = "Satisfaction Rate")) + scale_color_brewer(palette = "Spectral")
noofflightsVsSatisfaction_plot <- noofflightsVsSatisfaction_plot + scale_x_continuous("Number of flights") + scale_y_continuous("Frequency") + ggtitle("Plot of Satisfaction Vs Number of flights per annum")
noofflightsVsSatisfaction_plot

# Plot of Type of travel Vs Satisfaction
traveltypeVsSatisfaction <- table(dataCleaned$Satisfaction, dataCleaned$Type_of_Travel)
traveltypeVsSatisfaction_df <- data.frame(traveltypeVsSatisfaction)
noofflightsVsSatisfaction_df$Satisfaction <- as.numeric(noofflightsVsSatisfaction_df$Satisfaction)
colnames(traveltypeVsSatisfaction_df) <- c("Satisfaction", "Type", "Frequency")
traveltypeVsSatisfaction_plot <- ggplot(traveltypeVsSatisfaction_df, aes(x = traveltypeVsSatisfaction_df$Type, y = jitter(Frequency, 30)))
traveltypeVsSatisfaction_plot <- traveltypeVsSatisfaction_plot + geom_bar(stat = "identity", aes(fill = traveltypeVsSatisfaction_df$Satisfaction)) + scale_fill_brewer(palette = "Blues") + guides(fill = guide_legend(title = "Satisfaction Rate"))
traveltypeVsSatisfaction_plot <- traveltypeVsSatisfaction_plot + scale_x_discrete("Type of travel") + scale_y_continuous("Frequency") + ggtitle("Plot of Satisfaction Vs type of travel")
traveltypeVsSatisfaction_plot

# Number of people in different classes
class_df <- data.frame(table(dataCleaned$Class))
colnames(class_df) <- c("Class", "Frequency")
class_plot <- ggplot(class_df, aes(x = class_df$Class, y = class_df$Frequency)) + geom_histogram(stat = "identity", aes(fill = "red")) + ggtitle("People in different classes") + scale_x_discrete("Class") + scale_y_continuous("Frequency")
class_plot

# Departure delay affecting customer satisfaction
depdelay_df <- data.frame(table(dataCleaned$Satisfaction, dataCleaned$Departure_Delay_in_Minutes))
colnames(depdelay_df) <- c("Satisfaction", "DepartureDelay", "Frequency")
depdelay_df$Satisfaction <- as.numeric(depdelay_df$Satisfaction)
depdelay_df$DepartureDelay <- as.numeric(depdelay_df$DepartureDelay)
depdelay_func <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec >= q[2]] <- "High"
  vBuckets[vec < q[1]] <- "Low"
  return(vBuckets)
}
str(depdelay_df)
depdelay_df$Range <- depdelay_func(depdelay_df$DepartureDelay)
depdelay_plot <- ggplot(depdelay_df, aes(x = Range, y = Frequency)) + geom_bar(stat = "identity", aes(fill = depdelay_df$Satisfaction)) + scale_fill_brewer(palette = "OrRd")
depdelay_plot

# Plotting age and type of travel
  
dataPlot<-dataCleaned

#dummy<-dataPlot$Age

head(ageTravel_df)
ggplot(data=)

q<-quantile(dataPlot$Age, c(0.25, 0.5, 0.75))

vec<-dataPlot$Age

#summary(dataPlot$Age)

vBuckets <- replicate(length(vec), "Average")
vBuckets[vec <= q[1]] <- "15 to 33"
vBuckets[vec > q[1] & vec<=q[2]] <- "33 to 45"
vBuckets[vec> q[2] & vec<=q[3]] <- "46 to 59"
vBuckets[vec>q[3]] <- "60 to 85"

dataPlot$Age<-as.factor(vBuckets)

str(dataPlot$Age)

ageTravel<-table(dataPlot$Age,dataPlot$Type_of_Travel)
ageTravel_df<-data.frame(ageTravel)
colnames(ageTravel_df)<-c("Age","Type_of_Travel","Frequency")
View(ageTravel_df)

ggplot(ageTravel_df,aes(x=Age,y=Frequency))+
  geom_bar(stat = "identity",aes(fill = ageTravel_df$Type_of_Travel))+
  scale_fill_brewer(palette = "Spectral")

# Plotting age vs Price Sensitivity

agePrice<-table(dataPlot$Age,dataPlot$Price_Sensitivity)
agePrice_df<-data.frame(agePrice)
colnames(agePrice_df)<-c("Age","Price_Sensitivity","Frequency")

ggplot(agePrice_df,aes(x=Age,y=Frequency))+
  geom_bar(stat = "identity",aes(fill = agePrice_df$Price_Sensitivity))+
  scale_fill_brewer(palette = "Spectral")

# Plotting age vs class

ageClass<-table(dataPlot$Age,dataPlot$Class)
ageClass_df<-data.frame(ageClass)
colnames(ageClass_df)<-c("Age","Class","Frequency")

ggplot(ageClass_df,aes(x=Age,y=Frequency))+
  geom_bar(stat = "identity",aes(fill = ageClass_df$Class))+
  scale_fill_brewer(palette = "Spectral") +
  guides(fill=guide_legend(title="Class"))

# Plotting age vs Shopping amount

AgeShop<-table(dataPlot$Age,dataPlot$Shopping_Amount_at_Airport)
AgeShop_df<-data.frame(AgeShop)
colnames(AgeShop_df)<-c("Age","Shop_Amount","Frequency")

ggplot(AgeShop_df,aes(x=Age,y=Frequency))+
  geom_point(aes(color = AgeShop_df$Age))+
  scale_color_brewer(palette = "Spectral") +
  guides(color=guide_legend(title="Age"))

summary(AgeShop_df$Shop_Amount)

AgeShop_df$Shop_Amount<-as.numeric(AgeShop_df$Shop_Amount)

AgeShop_dfT<-AgeShop_df[AgeShop_df$Shop_Amount>0,]
summary(AgeShop_dfT$Shop_Amount)

summary(AgeShop_df$Shop_Amount)

# Sochna padega for above part

# Frequency of flight - network plot

#finding unique pairs of Origin State and Destination State
uniqueStatePairs <- unique(dataCleaned[,c('Origin_State','Destination_State')])
head(uniqueStatePairs,2)

library(sqldf)

#ordering origin state's count of outbounding flights 
orderedState<-sqldf("Select Distinct Count(Origin_State), Origin_State FROM uniqueStatePairs 
                    GROUP BY Origin_State ORDER BY Count(Origin_State) DESC")
head(orderedState,3)

# We got that the states of Illinois(36), Maryland(32) and Nevada(32) has most outbounds

#functions to add geocodes for destination states

# Latitude
addLat<- function(x){
  latitude <- geocode(source = 'dsk', x)
  return(latitude$lat)
}
# Longitude
addLon<- function(x){
  longitude <- geocode(source = 'dsk', x)
  return(longitude$lon)
}

library(ggmap)

# Starting with Illinois

#Geocode for Illinois
LatW <-addLat("Illinois")
LonW <-addLon("Illinois")

# adding the lat and long of all the states to the above df

ConWithIllinois<-sqldf("select AVG(Satisfaction) AS Sat, Destination_State FROM dataCleaned WHERE Origin_State='Illinois' GROUP BY Destination_State")
ConWithIllinois

ConWithIllinois$lat<-addLat(ConWithIllinois$Destination_State)
ConWithIllinois$lon<-addLon(ConWithIllinois$Destination_State)

# Creating a dummy states df to simulate simple US map

USAState<-data.frame(state.name,state.region,state.center)

USAState$stateName<-tolower(USAState$state.name)
#USAState<-USAState[,-1]

ConWithIllinois$stateName<-tolower(ConWithIllinois$Destination_State)
#ConWithIllinois<-ConWithIllinois[,-2]

us<-map_data("state")

# Creating a simple map

map_simple<-ggplot(USAState,aes(map_id=stateName))+
  geom_map(map=us,fill="light blue",color="black")+
  expand_limits(x=us$long,y=us$lat)+
  ggtitle("Map of USA")
map_simple

# Adding outbounds on the map

outboundMap<-map_simple+
  geom_curve(aes(x=LonW,y=LatW,xend=ConWithIllinois$lon ,yend=ConWithIllinois$lat, color=Sat), data=ConWithIllinois)
outboundMap

# Can be repeated same for other two states

# Creating a color coded (heat map) of origin states

stateSat<-sqldf("Select avg(Satisfaction) as sat,Origin_State FROM dataCleaned GROUP BY Origin_State")
stateSat

stateSatDes<-sqldf("Select avg(Satisfaction) as sat,Destination_State FROM dataCleaned GROUP BY Destination_State")
stateSatDes

stateSat$stateName<-tolower(stateSat$Origin_State)
#stateSat<-stateSat[,-2]

stateSatDes$stateName<-tolower(stateSatDes$Destination_State)

USAStateA<-data.frame(state.name,state.center,state.area)
USAStateA$stateName<-tolower(USAStateA$state.name)
#USAStateA[,-1]

library(ggplot2)

merged_df<-merge(USAStateA,stateSat,all.x=TRUE)
merged_df

merged_dfDes<-merge(USAStateA,stateSatDes,all.x=TRUE)
merged_dfDes

ggplot(merged_df , aes(map_id=stateName)) +
  geom_map(map=us,aes(fill=sat), color="black") +
  expand_limits(x=us$long,y=us$lat) +
  coord_map() +
  ggtitle("Color coded, Origin state based map of USA")+
  geom_text(aes(x=merged_df$x,y=merged_df$y,label=merged_df$stateName))

ggplot(merged_dfDes , aes(map_id=stateName )) +
  geom_map(map=us,aes(fill=sat), color="black") +
  expand_limits(x=us$long,y=us$lat) +
  coord_map() +
  ggtitle("Color coded, Destination state based map of USA")+
  geom_text(aes(x=merged_df$x,y=merged_df$y,label=merged_df$stateName))

# Plotting Gender vs Type of Travel

GenderTypeOfTravel <-table(dataPlot$Gender, dataPlot$Type_of_Travel)
GenderTypeOfTravel_df<-data.frame(GenderTypeOfTravel)
colnames(GenderTypeOfTravel_df)<-c("Gender","Type_of_Travel","Frequency")

ggplot(GenderTypeOfTravel_df,aes(x= Gender,y=Frequency))+
  geom_bar(stat = "identity",aes(fill = GenderTypeOfTravel_df$Type_of_Travel))+
  scale_fill_brewer(palette = "Spectral") +
  guides(fill=guide_legend(title="Type of Travel"))+
  ggtitle("Gender vs Type of Travel")

# Plotting Gender vs Class

GenderClass<-table(dataPlot$Gender,dataPlot$Class)
GenderClass_df<-data.frame(GenderClass)
colnames(GenderClass_df)<-c("Gender","Class","Frequency")

ggplot(GenderClass_df ,aes(x= Gender,y=Frequency))+
  geom_bar(stat = "identity",aes(fill = GenderClass_df$Class ))+
  scale_fill_brewer(palette = "Spectral") +
  guides(fill=guide_legend(title="Class"))+
  ggtitle("Gender vs Class")

# Plotting Gender vs Eating & Drinking amount

dataPlot$Eating_and_Drinking_at_Airport<-map(as.numeric(dataPlot$Eating_and_Drinking_at_Airport))
GenderEatDrink<-table(dataPlot$Gender,dataPlot$Eating_and_Drinking_at_Airport)
GenderEatDrink_df<-data.frame(GenderEatDrink)
colnames(GenderEatDrink_df)<-c("Gender","Eating_and_Drinking","Frequency")

ggplot(GenderEatDrink_df ,aes(x= Gender,y=Frequency))+
  geom_bar(stat = "identity",aes(fill = GenderEatDrink_df$Eating_and_Drinking ))+
  scale_fill_brewer(palette = "Spectral") +
  guides(fill=guide_legend(title="Eating and Drinking"))+
  ggtitle("Gender vs Eating&Drinking")

dataCleanedLow<-dataCleaned[dataCleaned$Satisfaction<3,]

summary(dataCleaned$X__of_Flight_with_other_Airlines)

summary(dataCleanedLow$X__of_Flight_with_other_Airlines)

# Plotting Arrival Delay in Minutes vs Destination State

summary(dataCleaned$Arrival_Delay_in_Minutes)

dataPlot<-dataCleaned

dataPlot<-dataPlot[!(dataPlot$Arrival_Delay_in_Minutes=="9999"),]

summary(dataPlot$Arrival_Delay_in_Minutes)

#hist(dataPlot$Arrival_Delay_in_Minutes)

stateArrival<-sqldf("Select sum(Arrival_Delay_in_Minutes) AS sum,Destination_State FROM dataPlot GROUP BY Destination_State" )

stateArrival$stateName<-tolower(stateArrival$Destination_State)
#stateArrival<-stateArrival[,-2]

#USAStateA<-USAStateA[,-1]

merged_dfA<-merge(USAStateA,stateArrival,all.x=TRUE)

ggplot(merged_dfA , aes(map_id=stateName)) +
  geom_map(map=us,aes(fill=sum ), color="black") +
  expand_limits(x=us$long,y=us$lat) +
  coord_map() +
  ggtitle("Color coded, Destination state based on Arrival delay")+
  geom_text(aes(x=merged_dfA$x,y=merged_dfA$y,label=merged_dfA$stateName))

# Plotting  Origin State and Departure Delay in Minutes

dataPlot<-dataCleaned

summary(dataPlot$Departure_Delay_in_Minutes)

dataPlot<-dataPlot[!(dataPlot$Departure_Delay_in_Minutes =="9999"),]

summary(dataPlot$Departure_Delay_in_Minutes)

stateDep<-sqldf("Select sum(Departure_Delay_in_Minutes) AS sum,Origin_State FROM dataPlot GROUP BY Origin_State" )

stateDep$stateName<-tolower(stateDep$Origin_State)

merged_dfD<-merge(USAStateA,stateDep,all.x=TRUE)

ggplot(merged_dfD , aes(map_id=stateName)) +
  geom_map(map=us,aes(fill=sum ), color="black") +
  expand_limits(x=us$long,y=us$lat) +
  coord_map() +
  ggtitle("Color coded, Origin state based on Departure delay")+
  geom_text(aes(x=merged_dfD$x,y=merged_dfD$y,label=merged_dfD$stateName))

# LM for subset

dataCleanedFemale<-dataCleaned[]

dataCleanedFemale<-dataCleaned[(trimws(dataCleaned$Gender,which="right")=="Female"),]

rownames(dataCleanedFemale)<-NULL

dataCleanedFemale<-subset(dataCleanedFemale, select = -c(Gender))

modelFemale<-lm(formula=Satisfaction~.,data=dataCleanedFemale)
summary(modelFemale)

str(dataCleaned)

# Using Association Rules Mining

Survey_data<-dataCleaned

str(Survey_data)

map<-function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

# Before applying check whether the col to be converted are numeric

str(Survey_data)

# We see only Satisfaction column needs to be converted

Survey_data$Satisfaction<-as.numeric(Survey_data$Satisfaction)

# Rechecking it

str(Survey_data)

# Now we can convert each column into their respective mapped columns

Survey_data$Satisfaction<-map(Survey_data$Satisfaction)
Survey_data$Age<-map(Survey_data$Age)
Survey_data$Price_Sensitivity<-map(Survey_data$Price_Sensitivity)
Survey_data$Year_of_First_Flight<-map(Survey_data$Year_of_First_Flight)
Survey_data$No_of_Flights_p_a_<-map(Survey_data$No_of_Flights_p_a_)
Survey_data$X__of_Flight_with_other_Airlines<-map(Survey_data$X__of_Flight_with_other_Airlines)
Survey_data$No__of_other_Loyalty_Cards<-map(Survey_data$No__of_other_Loyalty_Cards)
Survey_data$Shopping_Amount_at_Airport<-map(Survey_data$Shopping_Amount_at_Airport)
Survey_data$Eating_and_Drinking_at_Airport<-map(Survey_data$Eating_and_Drinking_at_Airport)
Survey_data$Day_of_Month<-map(Survey_data$Day_of_Month)
# Flight date can't be mapped hence not taken into consideration
Survey_data$Scheduled_Departure_Hour<-map(Survey_data$Scheduled_Departure_Hour)
Survey_data$Departure_Delay_in_Minutes<-map(Survey_data$Departure_Delay_in_Minutes)
Survey_data$Flight_time_in_minutes<-map(Survey_data$Flight_time_in_minutes)
Survey_data$Flight_Distance<-map(Survey_data$Flight_Distance)
Survey_data$Arrival_Delay_in_Minutes<-map(Survey_data$Arrival_Delay_in_Minutes)

# So now all the columns are categorical

#head(Survey_data)

#str(Survey_data)

# The columns aren't factors, so converting them to factors

dummy<-Survey_data

dummy[1:14]<-lapply(dummy[1:14],as.factor)
dummy[20:26]<-lapply(dummy[20:26],as.factor)

#str(dummy)

#Survey_data<-lapply(Survey_data, as.factor)

#str(Survey_data)

install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)

#surveyDataX<-as(Survey_data,"transactions")

dummy[15:21]<-dummy[20:26]
dummy<-dummy[-c(22:26)]

str(dummy)

dummyX<-as(dummy, "transactions")

#inspect(surveyDataX[1:20,])

inspect(dummyX[1:10,])

surveyDataX<-dummyX

# For customers having low satisfaction

rulesLow<-apriori(surveyDataX ,parameter = list(support=0.15,confidence=0.5,minlen=4,maxtime=0,maxlen=5),appearance = list(rhs=c("Satisfaction=Low")))
# We got 130 rules

inspect(rulesLow) # seeing the results affecting the satisfaction as low

plot(rulesLow) # Ploting the results

# Now, we will choose good rules based on the lift value, we can see there are few points above 1.8, so we will use that as delimiter

goodrulesLow <- rulesLow[quality(rulesLow)$lift > 1.9]

inspect(goodrulesLow) # These are 6 good rules

# [1] {Airline_Status=Blue, Type_of_Travel=Personal Travel, No__of_other_Loyalty_Cards=Low}

# [2] {Airline_Status=Blue, Gender=Female, Type_of_Travel=Personal Travel} 

# [3] {Airline_Status=Blue, Type_of_Travel=Personal Travel, Class=Eco}                      

# [4] {Airline_Status=Blue, Type_of_Travel=Personal Travel, Destination_City=No}            

# [5] {Airline_Status=Blue, Gender=Female, Type_of_Travel=Personal Travel, Destination_City=No}            

# [6] {Airline_Status=Blue, Type_of_Travel=Personal Travel, Class=Eco, Destination_City=No} 

# Now for customers having high satisfaction

rulesHigh<-apriori(surveyDataX ,parameter = list(support=0.002,confidence=0.5,minlen=4,maxtime=0,maxlen=5),appearance = list(rhs=c("Satisfaction=High")))
# 154 rules

inspect(rulesHigh) 

plot(rulesHigh)

goodrulesHigh<-rulesHigh[quality(rulesHigh)$lift>6]

rules_by_liftGood<-sort(goodrulesHigh, by="lift")

inspect(goodrulesHigh)

inspect(rules_by_liftGood)

# [1] {Airline_Status=Platinum, Price_Sensitivity=Low, Year_of_First_Flight=High, No__of_other_Loyalty_Cards=High}

# [2] {Airline_Status=Platinum, Year_of_First_Flight=High, Type_of_Travel=Business travel, Scheduled_Departure_Hour=Low}

# [3] {Airline_Status=Platinum, Gender=Male, Year_of_First_Flight=High, Day_of_Month=Low}       

# [4] {Airline_Status=Platinum, Year_of_First_Flight=High, Type_of_Travel=Business travel, Day_of_Month=Low} 

# goodrulesHighOrder<-rulesHigh[quality(rulesHigh)$lift]

rules_by_lift <- sort(rulesHigh, by = "lift")
inspect(rules_by_lift)

# Rules for subset of data

# Airline Status
# Age
# Gender
# Type of Travel
# Price Sensitivity
# Nos of Flights pa
# Class
# Year of first flight

Survey_dataSub<-dummy

str(Survey_dataSub)

head(Survey_dataSub,2)

Survey_dataSub<-Survey_dataSub[,-c(8,10:12,14:21)]

head(Survey_dataSub)

surveyDataSubX<-as(Survey_dataSub,"transactions")

rulesLow<-apriori(surveyDataX ,parameter = list(support=0.07,confidence=0.5,minlen=4,maxtime=0,maxlen=5),appearance = list(rhs=c("Satisfaction=Low")))
# Got 132 rules

inspect(rulesLow)

plot(rulesLow)

goodrulesLow <- rulesLow[quality(rulesLow)$lift > 1.983]

inspect(goodrulesLow)

rulesHigh<-apriori(surveyDataX ,parameter = list(support=0.0025,confidence=0.5,minlen=4,maxtime=0,maxlen=5),appearance = list(rhs=c("Satisfaction=High")))
# Got 71 rules

inspect(rulesHigh)

plot(rulesHigh)

goodrulesHigh <- rulesHigh[quality(rulesHigh)$lift > 5.8]

inspect(goodrulesHigh)

# kSVM

Survey_dataSVM<-dataCleaned

dummy<-ifelse(as.numeric(Survey_dataSVM$Satisfaction) > 3, "Happy", "Not Happy") 

Survey_dataSVM$HappyCust <-dummy # Creating a new column and insrting the above generated value.

dim(Survey_dataSVM)

randIndex<-sample(1:dim(Survey_dataSVM)[1]) # Creating a dataframe of random indices

cutPoint2_3<-floor(2*dim(Survey_dataSVM)[1]/3) # Creating a breakpoint of 2/3rd and 1/3rd part

trainData<-Survey_dataSVM[randIndex[1:cutPoint2_3],] # Creating traindata with 2/3rd

testData <-Survey_dataSVM[randIndex[(cutPoint2_3+1):dim(Survey_dataSVM)[1]],] # Creating testdata with 1/3rd 

dim(trainData) # Checking the dimension

dim(testData) # Checking the dimension

install.packages("kernlab") # installing the package: kernlab
library(kernlab) # Including the library: kernlab

svmOutput<-ksvm(HappyCust~Airline_Status+Age+Price_Sensitivity+No_of_Flights_p_a_+Class, data = trainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmOutput

svmPred <- predict(svmOutput, testData, type = "votes") # Making a Prediction variable based on number of votes

str(svmPred)
head(svmPred[2,])

compTable<-data.frame(testData$HappyCust,svmPred[2,]) # Creating a composite table based on HappyCustomer and svmPrediction
colnames(compTable)<-c("Testing Data","Prediction Data")


conMatrix<-table(compTable) # Creating a confusion matrix
conMatrix # Displaying the result onto console

errorSum<-conMatrix[1,2]+conMatrix[2,1] # Creating a dataframe containing sum of errors
errorRate<-errorSum/sum(conMatrix)*100 # Creating percentage of error rate
errorRate

# Drawing Confusion Matrix

fourfoldplot(conMatrix, color = c("Red", "Green"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

# Airline_Status+Price_Sensitivity+Class has error rate 37.37048
# Airline_Status+Class has error rate 37.07
# Airline_Status+Age has error rate 31.19
# Type of travel and age has error rate 22.93
# Age and Gender has error rate 32.0053
# Type of travel and class 24.91
# Type of travel, class and airline status has error rate 24.68
# Type of travel, age and class has error rate 23.73
# Type of travel and airline status has error rate 24.83
# Airline_Status+Age+Price_Sensitivity+No_of_Flights_p_a_+Class has error rate of 29.97

# Using the normal svm instead of ksvm

# install.packages("MASS")
# library(MASS)
# 
# install.packages("class")
# library(class)

install.packages("e1071")
library(e1071)

Survey_dataSVMN<-Survey_dataSVM

head(Survey_dataSVMN,2)

#Survey_dataSVM<-Survey_dataSVM[,-1]

#head(Survey_dataSVM,2)

svmOutputN<-svm(HappyCust~Airline_Status+Age+Price_Sensitivity+No_of_Flights_p_a_+Class,data=trainData,type="C")
svmOutputN
summary(svmOutputN)

pred<-predict(svmOutputN,testData)
str(pred)
table(pred,testData$HappyCust)

conMatrix<-table(pred,testData$HappyCust)

errorSum<-conMatrix[1,2]+conMatrix[2,1] # Creating a dataframe containing sum of errors
errorRate<-errorSum/sum(conMatrix)*100 # Creating percentage of error rate
errorRate

# Airline_Status+Age+Price_Sensitivity+No_of_Flights_p_a_+Class has error rate of 29.97

# LM

m1 <- lm(formula = Satisfaction ~ ., data = dataCleaned)
summary(m1)

# Significant columns

# Airline_Status
# Age
# Price_Sensitivity
# No_of_Flights_p_a_
# Class

m2 <- lm(formula = Satisfaction ~ Type_of_Travel, data = dataCleaned)
summary(m2)

# Mileage and Personal

m3<-lm(formula=Satisfaction~Age, data=dataCleaned)
summary(m3)
plot(dataCleaned$Satisfaction,dataCleaned$Age)
abline(m3)

# Nothing significant

m4<-lm(formula=Satisfaction~Price_Sensitivity,dataCleaned)
summary(m4)
plot(dataCleaned$Satisfaction,dataCleaned$Price_Sensitivity)
abline(m4)

# a line is drawn but didn't get much value

m5<-lm(formula = Satisfaction~No_of_Flights_p_a_,dataCleaned)
summary(m5)
plot(dataCleaned$Satisfaction,dataCleaned$No_of_Flights_p_a_)
abline(m5)

# Nothing significant

m6<-lm(formula = Satisfaction~Class,dataCleaned)
summary(m6)

# Class Eco and ClassEco Plus

# Interpretation:5

library(sqldf)

origStateCount<-sqldf("SELECT Origin_State, count(Origin_State) AS count FROM dataCleaned GROUP BY Origin_State")
totalCount<-sum(origStateCount$count)
origStateCount$prctFly<-(origStateCount$count/totalCount)*100
head(origStateCount)

datasam <- dataCleaned

mergeSam<-merge(datasam,origStateCount,by="Origin_State")
head(mergeSam)

mergeSamLow<-mergeSam[mergeSam$Satisfaction<4,]

origStateCountLow<-sqldf("SELECT Origin_State, count(Origin_State) AS Lowcount FROM mergeSamLow GROUP BY Origin_State")
origStateCountLow

origStateCountMerge<-merge(origStateCount,origStateCountLow,by="Origin_State")
colnames(origStateCountMerge)<-c("Origin_State","TotalStateCount","prctFly","TotalStateCountLow")
origStateCountMerge$prctLowCount<-(origStateCountMerge$TotalStateCountLow/origStateCountMerge$TotalStateCount)*100
#origStateCountMerge<-origStateCountMerge[order(-origStateCountMerge$prctLowCount),]
head(origStateCountMerge)
dummy<-data.frame(origStateCountMerge$Origin_State,origStateCountMerge$prctLowCount)
colnames(dummy)<-c("Origin_State","prctLowCount")
mergeSamFinal<-merge(mergeSam,dummy,by="Origin_State")

dataSam<-mergeSamFinal

# Creating a color coded (heat map) of origin states

combinedSam<-merge(dummy,origStateCount,by="Origin_State")
#combinedSam<-combinedSam[,-3]

#barplot(as.matrix(combinedSam[2:3],beside=TRUE))

combinedSam<-combinedSam[order(-combinedSam$prctFly),]
finalsub<-combinedSam[1:5,]
        
