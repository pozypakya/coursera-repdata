
# Reproducible Research: Peer Assessment 1

##load all the libraries

#```r
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if (is.installed('dplyr') == 'FALSE') {install.packages("dplyr")} else{library(dplyr)}
if (is.installed('ggthemes') == 'FALSE') {install.packages("ggthemes")} else{library(ggthemes)}
if (is.installed('scales') == 'FALSE') {install.packages("scales")} else{library(scales)}
if (is.installed('RColorBrewer') == 'FALSE') {install.packages("RColorBrewer")} else{library(RColorBrewer)}
if (is.installed('lubridate') == 'FALSE') {install.packages("lubridate")} else{library(lubridate)}
if (is.installed('ggplot2') == 'FALSE') {install.packages("ggplot2")} else{library(ggplot2)}
if (is.installed('plyr') == 'FALSE') {install.packages("plyr")} else{library(plyr)}
if (is.installed('knitr') == 'FALSE') {install.packages("knitr")} else{library(knitr)}
#```
## Loading and preprocessing the data

## set working directory
#```r
curdir <-getwd()
file.url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(file.url,destfile=paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""))
unzip(paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""),exdir=paste(curdir,sep=""),overwrite=TRUE)
#```

## Loading and preprocessing the data
#```r
data <- read.csv(paste(curdir,'/activity.csv',sep=""))
#```

##Ignore missing value
#```r
dataClean <- subset(data, is.na(data$steps) == F)
totalPerDay <- ddply(dataClean, .(date), summarise, steps=sum(steps))
#```

## Plot /  Make a histogram of the total number of steps taken each day
#```r
if("figure"%in%dir()==FALSE) dir.create("figure")
png(filename=paste(curdir,'/figure/unnamed-chunk-1.png',sep=""),width=480,height=480,units='px')
h <- hist(totalPerDay$steps , breaks = 20, main="Number of Steps", 
     xlab="Total number of steps taken each day", ylab = "Number of Days",col="red")
print(h)
graphics.off()
#```

## Calculate and report the mean and median of the total number of steps taken per day
## mean
#```r
mean(totalPerDay$steps)
#```

## median
#```r
median(totalPerDay$steps)	 
#```


## ---------What is the average daily activity pattern?--------------------------

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
averagePerInterval <- ddply(dataClean, .(interval), summarise, steps=mean(steps))


# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
if("figure"%in%dir()==FALSE) dir.create("figure")
png(filename=paste(curdir,'/figure/unnamed-chunk-2.png',sep=""),width=480,height=480,units='px')
h1 <- plot(averagePerInterval$interval, averagePerInterval$steps,axes = F, type="l", col="red", xlab="Time", ylab="Average Number of Steps",
     main="Average Daily Activity Pattern")
axis(1,at=c(0,600,1200,1800,2400), label = c("0:00","6:00","12:00","18:00","24:00"))
axis(2)
print(h1)
graphics.off()

maxSteps <- averagePerInterval[which.max(averagePerInterval$steps),] # 8.35 + 5-minute  = (8.35-8.40)

## ---------Imputing missing values--------------------------

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missingvalCount <- sum(is.na(data$steps))  # 2304

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Fill NA with the average value for each 5 minutes interval
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
missingValFillin  <- data 
for (i in 1:nrow(missingValFillin )){
    if (is.na(missingValFillin $steps[i])){
        missingValFillin $steps[i] <- averagePerInterval$steps[which(missingValFillin $interval[i] == averagePerInterval$interval)]}
}

missingValFillin <- arrange(missingValFillin, interval) # sorting the data by interval
missingvalCount <- sum(is.na(missingValFillin$steps)) # 0 ; test count the missing value 


# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

# TotalStepsMissingValueFillin
totalPerDayStepsMissingvalueFillin <- ddply(missingValFillin, .(date), summarise, steps=sum(steps))

# Try plot the data
if("figure"%in%dir()==FALSE) dir.create("figure")
png(filename=paste(curdir,'/figure/unnamed-chunk-3.png',sep=""),width=480,height=480,units='px')
h2 <- hist(totalPerDayStepsMissingvalueFillin$steps, breaks = 20, main="Number of Steps", xlab="Total number of steps taken each day", ylab = "Number of Days",col="red")
print(h2)
graphics.off()



mean(totalPerDayStepsMissingvalueFillin$steps) # 10766.19
median(totalPerDayStepsMissingvalueFillin$steps) # 10766.19
abs(mean(totalPerDay$steps)-mean(totalPerDayStepsMissingvalueFillin$steps)) # 0
abs(median(totalPerDay$steps)- median(totalPerDayStepsMissingvalueFillin$steps))/median(totalPerDay$steps) #0.0001104207

totalDifference <- sum(totalPerDayStepsMissingvalueFillin$steps) - sum(dataClean$steps)  # 86129.51


## ---------Are there differences in activity patterns between weekdays and weekends?--------------------------

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Sys.setlocale("LC_TIME", "English") 
missingValFillin$weekdays <- weekdays(as.Date(missingValFillin$date))
missingValFillin$weekdays <- ifelse(missingValFillin$weekdays %in% c("Saturday", "Sunday"),"weekend", "weekday")



average <- ddply(missingValFillin, .(interval, weekdays), summarise, steps=mean(steps))

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
library(lattice)
xyplot(steps ~ interval | weekdays, data = average, layout = c(1, 2), type="l", xlab = "Interval", ylab = "Number of steps")


	 
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

