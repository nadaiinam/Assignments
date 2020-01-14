#set the working directory
setwd("C:/Users/Nada/Desktop/Rfolder")


getwd()
dwhdata <- read.csv("original.dataset.csv",header=TRUE,sep=",")

View(dwhdata)


str(dwhdata)

head(dwhdata)
head(dwhdata,9)

tail(dwhdata)
tail(dwhdata,9)

class(dwhdata)
class(dwhdata$datetime)

colnames(dwhdata)

dim(dwhdata)
names(dwhdata)
View(dwhdata)

#print the whole data
print(dwhdata)

View(dwhdata$humidity)

#DESCRIPTIVE STATISTICS#

View(dwhdata)
# 6 statistical data types (minimum, 1st quartile, median, 3rd quartile, maximum, mean)
#only for numerical variables
#will give values for all variable types
summary(dwhdata$temp)
summary(dwhdata$humidity)
summary(dwhdata$windspeed)
summary(dwhdata$registered)

#get 5 statistical data types of numerical data (minimum, 1st quartile, median, 3rd quartile, maximum
fivenum(dwhdata$temp)
fivenum(dwhdata$atemp)
fivenum(dwhdata$humidity)
fivenum(dwhdata$windspeed)
fivenum(dwhdata$registered)

season <- dwhdata$season
class(season)
hist(season)

#season is looking odd; need to cast it to string
season <- as.factor(season) #use as.factor function
class(season) #verifying the cast
plot(season) #plot to see frequency

#casting the numbers 1, 2, 3, 4 to string values
install.packages("car")
library(car)
season <- recode(season, "1='Winter';2='Spring';3='Summer';4='Autumn'")
dwhdata$season <- season #assign recode to data frame
View(dwhdata)
plot(dwhdata$season)

#dealing with holiday
holiday <-dwhdata$holiday
class(holiday)
holiday <- as.factor(holiday)
plot(holiday)
holiday <- recode(holiday, "0='No.Holiday';1='Holiday'")
plot(holiday)
dwhdata$holiday <- holiday
View(dwhdata)

#dealing with workingday
workingday <- dwhdata$workingday
workingday <- as.factor(workingday)
plot(workingday)
workingday <- recode(workingday, "0='Weekend';1='Weekday'")
dwhdata$workingday <- workingday
View(dwhdata)

#dealing with weather
weather <- dwhdata$weather
weather <- as.factor(weather)
plot(weather)
weather <- recode(weather, "1='Sunny';2='Cold';3='Rainy';4='Storm'")
dwhdata$weather <- weather
View(dwhdata)

rm(list = c('weather','holiday','season','workingday'))

#view types
str(dwhdata)

#dealing with datetime variable
datetime <- dwhdata$datetime
datetime <- as.POSIXct(datetime, format = "%m/%d/%Y %H:%M") 
dwhdata$datetime <- datetime
str(dwhdata)

library(plyr)
library(lubridate)

temp <- day(datetime)
View(temp)

temp <- month(datetime)
View(temp)

temp <- year(datetime)
View(temp)

temp <- hour(datetime)
View(temp)

temp <- minute(datetime)
View(temp)

#DEALING WITH STRING (FACTOR) VARIABLES#
#categorical variables seem need to be decrypted as follows...
#results should be written down
plot(dwhdata$season,dwhdata$holiday)
plot(season,workingday)
#After comparing the above two plots, it seems better to consider
#workingday rather than holiday (which has an uneven distribution
#of season across its values as compared to values of workingday)

#removing holiday from the data frame
data <- subset(data,select=-holiday)
View(data)

plot(season,weather)
#across all seasons, snowy is non-existent; mostly sunny and windy
#this is strange as weather should be different in different seasons
#doubtful variable
data <- subset(data,select=-weather)
View(data)

# numerical now
str(data)

#DEALING WITH NUMERIC VARIABLES#
#analyzing the diff difference between temp and atemp
install.packages("reshape2")
install.packages("ggplot2")
library(reshape2)
library(ggplot2)

#making a data frame of temp, atemp and datetime
temp<-data$temp
atemp<-data$atemp
time<-data$datetime
newdf = cbind.data.frame(temp, atemp, datetime)
View(newdf)
str(newdf)

#plottin both atemp and temp vs time
ggplot(newdf, aes(time)) + 
  geom_line(aes(y = temp, colour = "temp")) + 
  geom_line(aes(y = atemp, colour = "atemp"))
#both temp and atemp are varying exactly together

# the difference
scatterplot(temp,atemp)
#straight line tells both are changing together

cor(temp,atemp)
#0.98 means 98% similarity; if one changes, other changes 98% exactly
#similarly

#so we should remove atemp (as temp measurement is more reliable)
data <- subset(data, select=-atemp)
View(data)

# If  to focus only on registered shoppers
data <- subset(data, select=-casual)
data <- subset(data, select=-count)
View(data)

#finding any correlations between numerical variables and registered
#how much a variable is affecting registered
registered <- data$registered
humidity <- data$humidity
windspeed <- data$windspeed

#temp and registered
cor(temp,registered)
scatterplot(temp,registered)
#somewhat correlated (temperature is affecting shopper turnout)

#windspeed and registered
cor(windspeed,registered)
scatterplot(windspeed,registered)
#not correlated but we see lesser turnout when windspeed is high

#humidity and registered
cor(humidity,registered)
scatterplot(humidity,registered)
#not correlated but we see lesser turnout when humidity is high or low

#I kept the humidity, windspeed and atemp as correlations are there 

#looking at box plots also (to check normal distribution)
boxplot(registered)
boxplot(humidity)
boxplot(windspeed)

hist(registered)
#histogram with normal plot (registered)
hist(registered, prob=TRUE)
curve(dnorm(x, mean=mean(registered), sd=sd(registered)), add=TRUE)

#histogram with normal plot (windspeed)
hist(windspeed, prob=TRUE)
curve(dnorm(x, mean=mean(windspeed), sd=sd(windspeed)), add=TRUE)

#histogram with normal plot (humidity)
hist(humidity, prob=TRUE)
curve(dnorm(x, mean=mean(humidity), sd=sd(humidity)), add=TRUE)

####COMPARING STRING AND NUMERICAL VARIABLES##

#is the mean registered user count changing across different seasons
fit <- aov(registered ~ season, data=data)

#what are the mean registered counts across each season type
print(model.tables(fit,"means"),digits=3)

#view them in boxplot
boxplot(registered~season,data=data)

#investigating whether differences are significant
summary(fit)
#there is significant difference; 

#same as above for working day
fit2 <- aov(registered ~ workingday, data=data)
plot(fit2)
#what are the mean registered counts across each season type
print(model.tables(fit2,"means"),digits=3)

#viewing them in boxplot
boxplot(registered~workingday,data=data)

#investigating whether differences are significant
summary(fit2)
#there is significant difference

#apparently number of registered voters is different across 
#different seasons and across weekdays and weekends

# a plot against time
ggplot(data, aes(data$datetime)) + 
  geom_line(aes(y = data$registered, colour = "registered"))  
#the counts seem to be increasing overall and there are regular
#intervals when shoppers are not there (holidays)
View(data)

write.csv(data,file="cleandata.csv")
library(Rserve);Rserve()


sapply(data, mean, na.rm=TRUE)

#create dataset only of registerd
newdataframe <- cbind(data$registered)
View(newdataframe)

means <- sapply(newdataframe, mean) 
View(means)

square <- sapply(newdataframe, function(x) x^2) 
View(square)

barplot(means)

median <- sapply(newdataframe, median) 
barplot(median)

range <- sapply(newdataframe, range) 
barplot(range)

quantile <- sapply(newdataframe, quantile) 
barplot(quantile)

sd <- sapply(newdataframe, sd) 
barplot(sd)

var <- sapply(newdataframe, var) 
barplot(var)

boxplot(data$registered)
boxplot(data$windspeed)
boxplot(data$humidity)
boxplot(data$temp)

rm(list=ls(all=TRUE)) 

cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rnames,cnames))
