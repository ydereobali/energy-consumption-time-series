########################################################################
# Title: Time Series Analysis of Energy Metering Data
# Author: Yigit Dereobali
# Name: ubiqum module 3 task 2 incl_download
# Description: 47 months minutely power consumption data of 1 main meter 
# and 3 submeters is available in a SQL Database on AWS. 
# Data is download data from AWS and processed. 
# It is analysed if submeter data improves the accuracy of 
# monthly consumption prediction of Linear and Holt Winters models.
# Date: 29.06.2019
# Version: 1.2
########################################################################


# LIBRARIES ####
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(data.table)
library(plotly)
library(stats)
library(RMySQL)
library(lubridate)
library(zoo)
library(forecast)



# DOWNLOAD and PROCCESS DATA ####
# Connect to Database 
con = dbConnect(
  MySQL(),
  user = 'deepAnalytics',
  password = 'Sqltask1234!',
  dbname = 'dataanalytics2018',
  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com'
)
dbListTables(con)

# Create Data Frames
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

# Combine Data Frames
rawdata <- rbind(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)
mydata <- rawdata


# Detect Incomplete Data
colnames(mydata)[colSums(is.na(mydata)) > 0]
mydata$Date[rowSums(is.na(mydata)) > 0]
missing.rows <- data.table(meter3.missingdates = mydata$Date[rowSums(is.na(mydata)) > 0])


# Omit Incomplete Observations
mydata <- na.omit(mydata)
colnames(mydata)[colSums(is.na(mydata)) > 0]
colSums(is.na(mydata))


# Chage Data Types
mydata$Global_reactive_power <- as.numeric(mydata$Global_reactive_power)
mydata$Global_intensity <- as.numeric(mydata$Global_intensity)
mydata$Voltage <- as.numeric(mydata$Voltage)
mydata$Global_active_power <- as.numeric(mydata$Global_active_power)
mydata$Sub_metering_1 <- as.numeric(mydata$Sub_metering_1)
mydata$Sub_metering_2 <- as.numeric(mydata$Sub_metering_2)
mydata$Date <- as.Date(mydata$Date, format = "%Y-%m-%d")


# Change Global Active Power Unit to Watthour
mydata$Global_active_power <- mydata$Global_active_power * 1000 / 60


# Calculate Consumption Other Than Submeters 1, 2 and 3
mydata <- mydata %>% 
  mutate(Others = Global_active_power - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)


# Combine Month and Year in a New Attribute Column
mydata <- mydata %>% 
  mutate(DateTime = paste(Date, Time))
#mydata <- subset(mydata, select = -c(Date, Time))


# Change Date Time Data Type
mydata$DateTime <- as.POSIXct(mydata$DateTime, "%Y-%m-%d %H:%M:%S")
attr(mydata$DateTime, "tzone") <- "GMT"


# Create, Factorize and Order Year
mydata$year <- year(mydata$DateTime)
mydata$year <- factor(mydata$year, ordered = TRUE)


# Create, Factorize and Order Hour
mydata$hour <- hour(mydata$DateTime)
mydata$hour <- factor(mydata$hour, ordered = TRUE)


# Create, Factorize and Order Day
mydata$day <- day(mydata$DateTime)
mydata$day <- factor(mydata$day, ordered = TRUE)


# Create, Factorize and Order Month
mydata$month <- month(mydata$DateTime)
mydata$month <- factor(mydata$month, ordered = TRUE)


# Create, Factorize and Order Week
mydata$week <- week(mydata$DateTime)
mydata$week <- factor(mydata$week, ordered = TRUE)


# Create, Factorize and Order Weekdays
mydata$weekday <- weekdays(mydata$DateTime) 
mydata$weekday <- ordered(
  mydata$weekday,
  levels = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday") )


# Filter Complete Years
complete_years <- mydata %>% 
  filter(year != 2006 & year != 2010)


# Calculate Annual Power Consumption of Complete Years
yearly.complete_years <- complete_years %>% 
  group_by(year) %>% 
  summarise(meter1 = sum(Sub_metering_1),
            meter2 = sum(Sub_metering_2),
            meter3 = sum(Sub_metering_3),
            others = sum(Others),
            total = sum(Global_active_power))


# Monthly Consumption Data of the Whole Time Period
# Create a New Attribute for Calender Month of Each Year
mydata$MonthAndYear <- as.yearmon(mydata$Date, format = "%m/%Y")


# Create Monthly Power Consumption Data Over the Whole Time Period
monthly.mydata <- mydata %>% 
  group_by(MonthAndYear) %>% 
  summarise(meter1 = sum(Sub_metering_1),
            meter2 = sum(Sub_metering_2),
            meter3 = sum(Sub_metering_3),
            others = sum(Others),
            total = sum(Global_active_power))


# VISUALIZE CONSUMPTION DATA ####
# Plot Annual Consumption of Complete Years
g1 <- ggplot(data = yearly.complete_years) +
  geom_line( aes(x = year, y = yearly.complete_years$total, group = 1,  color = "Total")) +
  geom_line( aes(x = year, y = yearly.complete_years$others, group = 1, color = "Others")) +
  geom_line( aes(x = year, y = yearly.complete_years$meter3, group = 1, color = "SubMeter3")) +
  geom_line( aes(x = year, y = yearly.complete_years$meter1, group = 1, color = "SubMeter1")) +
  geom_line( aes(x = year, y = yearly.complete_years$meter2, group = 1, color = "SubMeter2")) +
  ylab("power consumtion in watt hours") +
  xlab("years") +
  ggtitle("power consumption yearly") +
  scale_y_continuous(labels = scales::comma)
g1
ggplotly(g1)


# Plot Monthly Power Consumption Over the Whole Time Period
g2 <- ggplot(data = monthly.mydata) +
  geom_line( aes( x= MonthAndYear, y = monthly.mydata$total, group = 1,  color = "Total")) +
  geom_line( aes( x= MonthAndYear, y = monthly.mydata$others, group = 1, color = "Others")) +
  geom_line( aes( x= MonthAndYear, y = monthly.mydata$meter3, group = 1, color = "SubMeter3")) +
  geom_line( aes( x= MonthAndYear, y = monthly.mydata$meter1, group = 1, color = "SubMeter1")) +
  geom_line( aes( x= MonthAndYear, y = monthly.mydata$meter2, group = 1, color = "SubMeter2")) +
  ylab("power consumtion in watt hours") +
  xlab("months") +
  ggtitle("power consumption monthly") +
  scale_y_continuous(labels = scales::comma)
g2
ggplotly(g2)


# Create Daily Power Consumption Data Over the Whole Time Period
daily.mydata <- mydata %>% 
  group_by(Date) %>% 
  summarise(meter1 = sum(Sub_metering_1),
            meter2 = sum(Sub_metering_2),
            meter3 = sum(Sub_metering_3),
            others = sum(Others),
            total = sum(Global_active_power))


# Plot Daily Consumption Data Over the Whole Time Period
g3 <- ggplot(data = daily.mydata) +
  geom_point( aes( x= Date, y = daily.mydata$total, group = 1,  color = "Total")) +
  geom_point( aes( x= Date, y = daily.mydata$others, group = 1, color = "Others")) +
  geom_point( aes( x= Date, y = daily.mydata$meter3, group = 1, color = "SubMeter3")) +
  geom_point( aes( x= Date, y = daily.mydata$meter1, group = 1, color = "SubMeter1")) +
  geom_point( aes( x= Date, y = daily.mydata$meter2, group = 1, color = "SubMeter2")) +
  ylab("power consumtion in watt hours") +
  xlab("days") +
  ggtitle("power consumption daily") +
  scale_y_continuous(labels = scales::comma)
g3
ggplotly(g3)


# LINEAR REGRESSION ####
# Use Jan 2007- December 2009 Monthly Total Consumption Data to Predict Jan-November 2010 Monthly Consumptions
monthly <- monthly.mydata[c(2:37), ]
ts.monthly.T <- ts(monthly$total, frequency=12, start=c(2007,1))
lm.monthly.T <- tslm(ts.monthly.T ~ trend + season) 
summary(lm.monthly.T)
accuracy(lm.monthly.T)
forecast.lm.monthly.T <- forecast(lm.monthly.T, h=11, level=c(80,90))
gT <- plot(forecast.lm.monthly.T, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Others Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.O <- ts(monthly$others, frequency=12, start=c(2007,1))
lm.monthly.O <- tslm(ts.monthly.O ~ trend + season) 
summary(lm.monthly.O)
accuracy(lm.monthly.O)
forecast.lm.monthly.O <- forecast(lm.monthly.O, h=11, level=c(80,90))
g.O <- plot(forecast.lm.monthly.O, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Submeter1 Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.S1 <- ts(monthly$meter1, frequency=12, start=c(2007,1))
lm.monthly.S1 <- tslm(ts.monthly.S1 ~ trend + season) 
summary(lm.monthly.S1)
accuracy(lm.monthly.S1)
forecast.lm.monthly.S1 <- forecast(lm.monthly.S1, h=11, level=c(80,90))
g.S1 <- plot(forecast.lm.monthly.S1, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Submeter2 Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.S2 <- ts(monthly$meter2, frequency=12, start=c(2007,1))
lm.monthly.S2 <- tslm(ts.monthly.S2 ~ trend + season) 
summary(lm.monthly.S2)
accuracy(lm.monthly.S2)
forecast.lm.monthly.S2 <- forecast(lm.monthly.S2, h=11, level=c(80,90))
g.S2 <- plot(forecast.lm.monthly.S2, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Submeter3 Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.S3 <- ts(monthly$meter3, frequency=12, start=c(2007,1))
lm.monthly.S3 <- tslm(ts.monthly.S3 ~ trend + season) 
summary(lm.monthly.S3)
accuracy(lm.monthly.S3)
forecast.lm.monthly.S3 <- forecast(lm.monthly.S3, h=11, level=c(80,90))
g.S3 <- plot(forecast.lm.monthly.S3, ylab= "Watt-Hours", xlab="Time")


# HOLT WINTERS ####
# Use Jan 2007- December 2009 Monthly Total Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.T <- ts(monthly$total, frequency=12, start=c(2007,1))
# Decompose Time Series
components.ts.monthly.T <- decompose(ts.monthly.T)
summary(components.ts.monthly.T)
# Seasonal Adjuted Time Series
ts.monthly.T.Adjusted <- ts.monthly.T #- components.ts.monthly.T$seasonal
hw.monthly.T <- HoltWinters(ts.monthly.T.Adjusted, beta=FALSE, gamma=TRUE) 
summary(hw.monthly.T)
#accuracy(hw.monthly.T)
forecast.hw.monthly.T <- forecast(hw.monthly.T, h=11, level=c(80,90))
gT2 <- plot(forecast.hw.monthly.T, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Others Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.O <- ts(monthly$others, frequency=12, start=c(2007,1))
# Decompose Time Series
components.ts.monthly.O <- decompose(ts.monthly.O)
summary(components.ts.monthly.O)
# Seasonal Adjuted Time Series
ts.monthly.O.Adjusted <- ts.monthly.O #- components.ts.monthly.O$seasonal
hw.monthly.O <- HoltWinters(ts.monthly.O.Adjusted, beta=FALSE, gamma=TRUE) 
summary(hw.monthly.O)
#accuracy(hw.monthly.O)
forecast.hw.monthly.O <- forecast(hw.monthly.O, h=11, level=c(80,90))
g.O <- plot(forecast.hw.monthly.O, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Submeter1 Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.S1 <- ts(monthly$meter1, frequency=12, start=c(2007,1))
# Decompose Time Series
components.ts.monthly.S1 <- decompose(ts.monthly.S1)
summary(components.ts.monthly.S1)
# Seasonal Adjuted Time Series
ts.monthly.S1.Adjusted <- ts.monthly.S1 #- components.ts.monthly.S1$seasonal
hw.monthly.S1 <- HoltWinters(ts.monthly.S1.Adjusted, beta=FALSE, gamma=TRUE ) 
summary(hw.monthly.S1)
#accuracy(hw.monthly.S1)
forecast.hw.monthly.S1 <- forecast(hw.monthly.S1, h=11, level=c(80,90))
g.S1 <- plot(forecast.hw.monthly.S1, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Submeter2 Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.S2 <- ts(monthly$meter2, frequency=12, start=c(2007,1))
# Decompose Time Series
components.ts.monthly.S2 <- decompose(ts.monthly.S2)
summary(components.ts.monthly.S2)
# Seasonal Adjuted Time Series
ts.monthly.S2.Adjusted <- ts.monthly.S2 #- components.ts.monthly.S2$seasonal
hw.monthly.S2 <- HoltWinters(ts.monthly.S2.Adjusted, beta=FALSE, gamma=TRUE) 
summary(hw.monthly.S2)
#accuracy(hw.monthly.S2)
forecast.hw.monthly.S2 <- forecast(hw.monthly.S2, h=11, level=c(80,90))
g.S2 <- plot(forecast.hw.monthly.S2, ylab= "Watt-Hours", xlab="Time")


# Use Jan 2007- December 2009 Monthly Submeter3 Consumption Data to Predict Jan-November 2010 Monthly Consumptions
ts.monthly.S3 <- ts(monthly$meter3, frequency=12, start=c(2007,1))
# Decompose Time Series
components.ts.monthly.S3 <- decompose(ts.monthly.S3)
summary(components.ts.monthly.S3)
# Seasonal Adjuted Time Series
ts.monthly.S3.Adjusted <- ts.monthly.S3 #- components.ts.monthly.S3$seasonal
hw.monthly.S3 <- HoltWinters(ts.monthly.S3.Adjusted, beta=FALSE, gamma=TRUE) 
summary(hw.monthly.S3)
#accuracy(hw.monthly.S3)
forecast.hw.monthly.S3 <- forecast(hw.monthly.S3, h=11, level=c(80,90))
g.S3 <- plot(forecast.hw.monthly.S3, ylab= "Watt-Hours", xlab="Time")


# VALIDATION 2010 ####
monthly.2010 <- data.table(monthly.mydata[c(38:48), ])


# Linear Regression Total
df.T <- as.data.frame(forecast.lm.monthly.T$mean)
monthly.2010 <- cbind(monthly.2010, df.T)
colnames(monthly.2010)[7] <- "LM.Total.Prediction"


# Linear Regression Others
df.O <- as.data.frame(forecast.lm.monthly.O$mean)
monthly.2010 <- cbind(monthly.2010, df.O)
colnames(monthly.2010)[8] <- "LM.Others.Prediction"


# Linear Regression Submeter1
df.S1 <- as.data.frame(forecast.lm.monthly.S1$mean)
monthly.2010 <- cbind(monthly.2010, df.S1)
colnames(monthly.2010)[9] <- "LM.S1.Prediction"


# Linear Regression Submeter2
df.S2 <- as.data.frame(forecast.lm.monthly.S2$mean)
monthly.2010 <- cbind(monthly.2010, df.S2)
colnames(monthly.2010)[10] <- "LM.S2.Prediction"


# Linear Regression Submeter3
df.S3 <- as.data.frame(forecast.lm.monthly.S3$mean)
monthly.2010 <- cbind(monthly.2010, df.S3)
colnames(monthly.2010)[11] <- "LM.S3.Prediction"


# Sum of all Linear Regression Submeter Predictions
monthly.2010$LM.SumSubs <- (monthly.2010$LM.Others.Prediction +
                              monthly.2010$LM.S1.Prediction +
                              monthly.2010$LM.S2.Prediction +
                              monthly.2010$LM.S3.Prediction)


# Holt Winters Total
df.T <- as.data.frame(forecast.hw.monthly.T$mean)
monthly.2010 <- cbind(monthly.2010, df.T)
colnames(monthly.2010)[13] <- "HW.Total.Prediction"


# Holt Winters Others
df.O <- as.data.frame(forecast.hw.monthly.O$mean)
monthly.2010 <- cbind(monthly.2010, df.O)
colnames(monthly.2010)[14] <- "HW.Others.Prediction"


# Holt Winters Submeter1
df.S1 <- as.data.frame(forecast.hw.monthly.S1$mean)
monthly.2010 <- cbind(monthly.2010, df.S1)
colnames(monthly.2010)[15] <- "HW.S1.Prediction"


# Holt Winters Submeter2
df.S2 <- as.data.frame(forecast.hw.monthly.S2$mean)
monthly.2010 <- cbind(monthly.2010, df.S2)
colnames(monthly.2010)[16] <- "HW.S2.Prediction"


# Holt Winters Submeter3
df.S3 <- as.data.frame(forecast.hw.monthly.S3$mean)
monthly.2010 <- cbind(monthly.2010, df.S3)
colnames(monthly.2010)[17] <- "HW.S3.Prediction"


# Sum of all Holt Winters Submeter Predictions
monthly.2010$HW.SumSubs <- (monthly.2010$HW.Others.Prediction +
                              monthly.2010$HW.S1.Prediction +
                              monthly.2010$HW.S2.Prediction +
                              monthly.2010$HW.S3.Prediction)


# Adjust November by Factor + 15%
monthly.2010[11,6] <- 1.15 * monthly.2010[11,6]

# Plot Actual and Predicted Monthly Consumptions
g21 <- ggplot(data = monthly.2010) +
  geom_line( aes( x= MonthAndYear, y = monthly.2010$total, group = 1,  color = "Actual Consumption Nov. Adjusted")) +
  geom_line( aes( x= MonthAndYear, y = monthly.2010$LM.Total.Prediction, group = 1, color = "LM.Total.Prediction")) + 
  geom_line( aes( x= MonthAndYear, y = monthly.2010$LM.SumSubs, group = 1, color = "LM.SumSubs.Prediction")) + 
  geom_line( aes( x= MonthAndYear, y = monthly.2010$HW.Total.Prediction, group = 1, color = "HW.Total.Prediction")) + 
  geom_line( aes( x= MonthAndYear, y = monthly.2010$HW.SumSubs, group = 1, color = "HW.SumSubs.Prediction")) + 
  ylab("power consumtion in watt hours") +
  xlab("months") +
  ggtitle("Monthly Consumptions and Predictions 2010") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))
g21
ggplotly(g21)


# Error Analysis Linear Regression
postResample(monthly.2010$LM.Total.Prediction,monthly.2010$total)
postResample(monthly.2010$LM.SumSubs, monthly.2010$total)


# Error Analysis Holt Winters
postResample(monthly.2010$HW.Total.Prediction,monthly.2010$total)
postResample(monthly.2010$HW.SumSubs, monthly.2010$total)





