#First of all, start by cleaning the workspace and setting the working directory
rm(list=ls())
setwd("C:/Users/AndrewTawdros/Desktop/4th Year/Second Term/Big Data/Project")
getwd()

#Including Libraries
library(lubridate)
library(ggplot2)
library(e1071)
library(rpart)
library(rpart.plot) 
library(randomForest)
library(pROC) #ROC curve for analysis
library(dplyr)
library(rsample)
library(rpart)
library(skimr)
library(caret)
library(ipred)
library(tree)


                          ### Draft Question ###
# Which is the most in-demand hotel?
# What are the factors driving the cancellations in hotels?
# Which type of rooms are most preferred?
# Is there any incorrect data in the dataset and how to handle them?
# What is the percentage of bookings cancelled?



###################################################################################################
#################################### [1] Analysis #################################################
###################################################################################################

# Import dataset into a data frame.
HotelBooking <- read.csv("Dataset/hotel_bookings.csv",header = TRUE, sep=",")
dim(HotelBooking)

# Get more insight into data by exploring the first and the last ten rows in the dataset.
head(HotelBooking, 10)
tail(HotelBooking, 10)

# Show summary of all variables in the data frame.
summary(HotelBooking)


#################################### [1.1] Pre-Processing #########################################
#HotelBooking$arrival_date_month <-factor(HotelBooking$arrival_date_month, levels = month.name)


# #---Step 1: Variable Identification---

# Find data type
str(HotelBooking)

# Adjust data type (Int -> Factor)
HotelBooking$is_repeated_guest <- as.factor(ifelse(HotelBooking$is_repeated_guest==1, "Yes", "No"))
HotelBooking$arrival_date_year <- as.factor(HotelBooking$arrival_date_year) 
HotelBooking$arrival_date_week_number <- as.factor(HotelBooking$arrival_date_week_number) 
HotelBooking$arrival_date_day_of_month <- as.factor(HotelBooking$arrival_date_day_of_month) 
HotelBooking$is_canceled=as.factor(HotelBooking$is_canceled)

# Adjust data type (Int -> Numeric)
HotelBooking$is_canceled=as.numeric(ifelse(HotelBooking$is_canceled==1, 1, 0))

# Adjust data type (Character -> Factor)
HotelBooking$arrival_date_month <- as.factor(HotelBooking$arrival_date_month)
# Change the order of levels 
HotelBooking$arrival_date_month <- factor(HotelBooking$arrival_date_month, levels = month.name)
# #---Step 2: Deal With Missing Value---

# Test Missing Value
colSums(is.na(HotelBooking)) #( Column Children has 4 missing value)

# Remove rows with NA
HotelBooking <- na.omit(HotelBooking)
sum(is.na(HotelBooking))


#drops <- c("company","country","adr_pp","reservation_status_date","agent","reservation_status","meal","distribution_channel","market_segment")
#data_RF<-data_RF[ , !(names(data_RF) %in% drops)]



#################################### [1.2] visualization ##########################################

resort_hotel <- HotelBooking[which(HotelBooking$reservation_status!="No-Show"& HotelBooking$hotel == "Resort Hotel"),]
city_hotel <- HotelBooking[which(HotelBooking$reservation_status!="No-Show"& HotelBooking$hotel == "City Hotel"),]


# City hotels vs Resort hotels
ggplot(HotelBooking,aes(x=factor(hotel))) +
  geom_bar(col ="black",fill="#993333",alpha=0.5) +
  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_x_discrete("Hotel") +
  scale_y_continuous("Count")

# Graph of Number of arrival Date by Month
ggplot(HotelBooking,aes(factor(arrival_date_month,levels=month.name))) +
  geom_bar(col ="black",fill="#993333",alpha=0.5) +
  theme(axis.text.x = element_text(face="bold", size=8, angle=30)) +
  scale_y_continuous("Count",limits = c(0,15000),breaks=seq(0,15000,by=1500)) +
  scale_x_discrete("Month")

#Number of city hotel and Resort Hotel cancelled or not cancelled 
ggplot(data = HotelBooking,aes(factor(is_canceled)))+
  geom_bar( col='black', fill="#993333", alpha = 0.5) +
  facet_wrap(~hotel) +
  scale_x_discrete("Canceled",labels = c("No","Yes")) +
  scale_y_continuous("Count",limits = c(0,50000),breaks=seq(0,47222,by=5000))  +
  theme(axis.text.x = element_text(face="bold", size=10))

#Canceled and Lead time
ggplot(data = HotelBooking, aes(x = factor(is_canceled), y = lead_time  )) + 
  geom_boxplot(col='black', fill="#993333", alpha = 0.5) +
  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_y_continuous("Lead Time",limits = c(0,800),breaks=seq(0,800,by=100)) +
  scale_x_discrete("Canceled",labels = c("No","Yes"))

#Year of arrival
barplot(table(HotelBooking$arrival_date_year), col='#993333')

#May and October have  the highest waiting times; these months represent the times right before and after peak reservation months (respectively)
ggplot(HotelBooking, aes(x=arrival_date_month, y=days_in_waiting_list, group=1)) + stat_summary(fun="mean", geom="line", col="navy") + 
  ggtitle("Average Days on Waiting List by Arrival Month") + ylab("Waiting List: Average Days") + xlab("Month") +
  theme(axis.text.x=element_text(angle=40))



### plot of price of room types for each person per night ###
all_ag <- subset(HotelBooking,is_canceled!="1")
all_ag$adr_pp <- all_ag$adr/(all_ag$adults+all_ag$children)
room_price <- subset(all_ag,select=c(hotel,reserved_room_type,adr_pp))
ggplot(data = room_price, aes(x =reserved_room_type, y = adr_pp,
                              fill=hotel))+ geom_boxplot()+ labs(title = "                          Price of room types for each person per night",x = "Room type",y="Price")

#cancellation per months
ggplot(HotelBooking, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()


#################################### [1.3] Extracting insights ####################################

# Number of arrival Date by Month
d <- HotelBooking %>% 
  group_by(arrival_date_month) %>%
  count() %>%
  arrange(match(arrival_date_month,month.name))
d <- data.frame(ArrivalDateMonth = d$arrival_date_month,N =d$n)
d

#Online market segment's cancellation is more
HotelBooking%>% group_by(HotelBooking$market_segment)  %>% summarise(length(is_canceled))

#City Hotel Cancellation is more
HotelBooking %>% group_by(HotelBooking$hotel)  %>% summarise(length(is_canceled))

#Couples booking cancellation is more
HotelBooking %>% group_by(HotelBooking$adults)  %>% summarise(length(is_canceled))

#'A' type room cancellation is higher
HotelBooking %>% group_by(HotelBooking$reserved_room_type)  %>% summarise(length(is_canceled))


                                              ## Time Series ##
#Reservation status date by year
ts<- HotelBooking%>%group_by(reservation_status_date)%>%summarise(n=n())
ts$reservation_status_date <- as.Date(ts$reservation_status_date)

#Time Series Analysis
ggplot(ts, aes(reservation_status_date, n)) + geom_line()

ts <- ts %>% filter(!is.na(n))
ts

data <- ts(ts$n, frequency=365)

# Frequency is set with 365 because it's daily
components <- stl(data, 'periodic')
# seasonal, trend, remainder
plot(components)





#################################### [1.4] Model/classifier #######################################

#Remove unwanted columns 
drops <- c("company","country","adr_pp","company","reservation_status_date","agent","reservation_status")
HotelBooking<-HotelBooking[ , !(names(HotelBooking) %in% drops)]

#splitting dataset into training and testing data
set.seed(0)
n=nrow(HotelBooking)
shuffled=HotelBooking[sample(n),]
trainSet=shuffled[1:round(0.7 * n),]
testSet = shuffled[(round(0.7 * n) + 1):n,]
summary(trainSet)
summary(testSet)

# 95512 rows
dim(trainSet)
#23878 rows
dim(testSet)

table(trainSet$is_canceled)
table(testSet$is_canceled)


                                    ### Logistic Regression ###

model1 <- glm(is_canceled ~ hotel + lead_time + arrival_date_month + children +
                market_segment + is_repeated_guest + adults + babies +
                previous_cancellations +
                deposit_type + booking_changes  +
                reserved_room_type + adr + days_in_waiting_list + customer_type +
                total_of_special_requests, 
              data = trainSet , family = "binomial")
summary(model1)

train_pred <-predict(model1, trainSet,type = 'response')
library(knitr)
library(ROCR)
library(verification)
pred <- prediction(train_pred,trainSet$is_canceled)
aucObj <- performance(pred, measure="auc")
rocObj = performance(pred, measure="tpr", x.measure="fpr")  # creates ROC curve obj
max <- which.max(slot(aucObj,"y.values")[[1]])

auc = aucObj@y.values[[1]]
auc   # the auc score: tells you how well the model predicts.

# plot the roc curve
plot(rocObj, main = paste("Area under the curve:", auc))

train_pred1 <- ifelse(train_pred >  0.5, 1,0)
mean(trainSet$is_canceled == train_pred1)

tble <- table(Actual = trainSet$is_canceled,Predicted = train_pred1 )
tble


test_pred <-predict(model1, testSet,type = 'response')

test_pred1 <- ifelse(test_pred > 0.5 , 1,0)

#test accuracy 80.62%
mean(testSet$is_canceled == test_pred1)

#confusion matrix
tble1 <- table(Actual = testSet$is_canceled,Predicted = test_pred1 )
tble1


TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble[1,])
P <- sum(tble[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)
1 - sum(diag(tble1))/sum(tble1)

##Accuracy test = 83.25%
pred1 <- prediction(test_pred,testSet$is_canceled)
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc


                                    ### Random Forest ### [Final Solution]
features<-setdiff(names(trainSet), "is_canceled")

rf<-randomForest(formula=is_canceled~hotel + lead_time + arrival_date_month + children +
                   market_segment + is_repeated_guest + adults + babies +
                   previous_cancellations +
                   deposit_type + booking_changes  +
                   reserved_room_type + adr + days_in_waiting_list + customer_type +
                   total_of_special_requests, data = trainSet)
print(rf)


#prediction
pred_rf<-predict(rf,testSet,target="class")
pred_rf
confusionMatrix(pred_rf, as.factor(testSet$is_canceled))

##Accuracy test = 84.16%
