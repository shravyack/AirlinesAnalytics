
#Part A: Data Load #
df <- read.csv("B:/Syracuse/Data Science/Project details/Satisfaction Survey.csv", header = TRUE, stringsAsFactors = FALSE)

View(df)

#Part B: Selection of Particular Airline

#Part C: Data Cleaning #
dataCleaned<-df[(trimws(df$Airline.Name,which="right")=="Cheapseats Airlines Inc."),]

rownames(dataCleaned)<-NULL

dataCleaned<-subset(dataCleaned, select = -c(Airline.Name))

dataCleaned<-subset(dataCleaned, select = -c(Airline.Code))

#To find out which columns have NA values
apply(dataCleaned,2, function(x) any(is.na(x)))

# Now we come to know that only three columns contain NA values
# Those are 1. Departure.Delay.in.Minutes 2. Arrival.Delay.in.Minutes 3.Flight.time.in.minutes
# Rest columns are free from NA

dataCleaned[is.na(dataCleaned)]<-9999 # Dirty value

apply(dataCleaned,2, function(x) any(is.na(x)))

# No column contains NA

str(dataCleaned)

# converting the attributes which are date to the respective format

dataCleaned$Flight.date<-as.Date(dataCleaned$Flight.date,"%m/%d/%y")

str(dataCleaned)

#Pending task: Clean col names
colnames(dataCleaned)

# Now we will look how following factors affect the customer rating

# modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = dataCleaned)
# summary(modelOfAirlineStatus)
# 
# modelOfAge<-lm(formula = Satisfaction~Age, data = dataCleaned)
# summary(modelOfAge)
# 
# modelOfAge<-lm(formula = Satisfaction~Age, data = dataCleaned)
# summary(modelOfAge)
View(dataCleaned)
allattributemodel<-lm(formula = Satisfaction~., data = dataCleaned)
summary(allattributemodel)

#From above results, following attributes are more statistically significant for customer satisfaction
#Adjusted R square value: 0.4506
#3 Stars:
#Airline.StatusGold, Airline.StatusPlatinum, Airline.StatusSilver, Age, GenderMale, Price.Sensitivity,
#No.of.Flights.p.a.,Type.of.TravelMileage tickets, Type.of.TravelPersonal Travel,ClassEco, ClassEco Plus
# 1 Star:
#Year.of.First.Flight,Orgin.CityCorpus Christi, TX, Orgin.CityDayton, OH

View(dataCleaned)

#Verify if its possible to achieve better R sqaured value considering only 3 star statistically significant variables

m1 <- lm(formula = Satisfaction ~ Airline.Status+Age+Gender+Price.Sensitivity+No.of.Flights.p.a.+Type.of.Travel+Class, data = dataCleaned)
summary(m1)
#Obtained adjusted R square value of 0.422 which is lesser than previous iteration. 
#However, Class attribute with value Eco Plus turned about to be 2 star in this model

#Now these results need to be matched with association rules mining. 
#Then few other models can be tried for the attributes which majorily affect low customer satisfaction
