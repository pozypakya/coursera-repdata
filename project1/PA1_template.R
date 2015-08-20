
## ----load all the libraries------------------------------------------------------------

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if (is.installed('dplyr') == 'FALSE') {install.packages("dplyr")} else{library(dplyr)}
if (is.installed('ggthemes') == 'FALSE') {install.packages("ggthemes")} else{library(ggthemes)}
if (is.installed('scales') == 'FALSE') {install.packages("scales")} else{library(scales)}
if (is.installed('RColorBrewer') == 'FALSE') {install.packages("RColorBrewer")} else{library(RColorBrewer)}
if (is.installed('lubridate') == 'FALSE') {install.packages("lubridate")} else{library(lubridate)}
if (is.installed('ggplot2') == 'FALSE') {install.packages("ggplot2")} else{library(ggplot2)}
if (is.installed('plyr') == 'FALSE') {install.packages("plyr")} else{library(plyr)}


## Loading and preprocessing the data

# set working directory
curdir <-setwd("D:/Google Drive/Coursera/Assignment 4.1/ExData_Plotting2")
file.url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(file.url,destfile=paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""))
unzip(paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""),exdir=paste(curdir,sep=""),overwrite=TRUE)

## ---------Loading and preprocessing the data--------------------------
data <- read.csv(paste(curdir,'/activity.csv',sep=""))
## ---------Ignore missing value--------------------------
data <- subset(data, is.na(data$steps) == F)
totalPerDay <- ddply(data, .(date), summarise, steps=sum(steps))


## ---------Plot /  Make a histogram of the total number of steps taken each day--------------------------
hist(totalPerDay$steps , main="Number of Steps", 
     xlab="Total number of steps taken each day", ylab = "Number of Days")

## ---------Calculate and report the mean and median of the total number of steps taken per day--------------------------
# mean
mean(totalPerDay$steps)
# median
median(totalPerDay$steps)	 
	 
	 
## ---------What is mean total number of steps taken per day?--------------------------
data$date<-as.Date(data$date)
data$Weekday<-wday(data$date, label = TRUE, abbr = FALSE)

  
  
## ---------What is mean total number of steps taken per day?--------------------------
library(ggplot2)
#Calculate the total number of steps taken per day
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
#Calculate and report the mean and median of the total number of steps taken per day
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
#If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
hist(total.steps)




## --------What is the average daily activity pattern?--------------------------
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



## ------------------------------------------------------------------------
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")


## ------------------------------------------------------------------------
averages[which.max(averages$steps),]


## ----how_many_missing----------------------------------------------------
missing <- is.na(data$steps)
# How many missing
table(missing)


## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)


## ------------------------------------------------------------------------
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)


## ------------------------------------------------------------------------
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)


## ------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")

