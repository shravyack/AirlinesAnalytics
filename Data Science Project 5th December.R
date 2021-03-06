options(scipen = 100)

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
#str(AgeVsShop)

# Checking the effect of avg shop amt spend on different age groups

AvgVsE_D <-sqldf("Select Age,count(age), avg(Eating_and_Drinking_at_Airport) AS Eating_and_Drinking_at_Airport FROM dataCleaned GROUP BY Age;")
#str(AvgVsE_D)

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

# ggplot(store, aes(x = store1$Age, y = store1$Eating_and_Drinking_at_Airport)) + 
#   geom_bar(stat = "identity", color = "Black", fill = "Yellow") + 
#   ggtitle("Average Amount spent in Eating & Driking at Airport by different age groups") + 
#   scale_x_continuous(name="Age") + 
#   scale_y_continuous(name = "Average Amount spent")


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

dummyX<-as(dummy, "transactions")

#inspect(surveyDataX[1:20,])

inspect(dummyX[1:10,])

surveyDataX<-dummyX

# For customers having low satisfaction

rulesLow<-apriori(surveyDataX ,parameter = list(support=0.15,confidence=0.5,minlen=4,maxtime=0,maxlen=5),appearance = list(rhs=c("Satisfaction=Low")))
# We got 41 rules

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
# Clas
# Year of first flight

Survey_dataSub<-Survey_data

head(Survey_dataSub,2)

Survey_dataSub<-Survey_dataSub[,-c(8,10:12,14:26)]

str(Survey_dataSub)

Survey_dataSub<-lapply(Survey_dataSub,as.factor)

str(Survey_dataSub)



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
conMatrix<-table(compTable) # Creating a confusion matrix
conMatrix # Displaying the result onto console

errorSum<-conMatrix[1,2]+conMatrix[2,1] # Creating a dataframe containing sum of errors
errorRate<-errorSum/sum(conMatrix)*100 # Creating percentage of error rate
errorRate

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

# Sochna padega


