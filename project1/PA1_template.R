
## ----load all the libraries------------------------------------------------------------

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if (is.installed('dplyr') == 'FALSE') {install.packages("dplyr")} else{library(dplyr)}
if (is.installed('ggthemes') == 'FALSE') {install.packages("ggthemes")} else{library(ggthemes)}
if (is.installed('scales') == 'FALSE') {install.packages("scales")} else{library(scales)}
if (is.installed('RColorBrewer') == 'FALSE') {install.packages("RColorBrewer")} else{library(RColorBrewer)}
if (is.installed('lubridate') == 'FALSE') {install.packages("lubridate")} else{library(lubridate)}
if (is.installed('ggplot2') == 'FALSE') {install.packages("ggplot2")} else{library(ggplot2)}


## Loading and preprocessing the data

# set working directory
curdir <-setwd("D:/Google Drive/Coursera/Assignment 4.1/ExData_Plotting2")
file.url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(file.url,destfile=paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""))
unzip(paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""),exdir=paste(curdir,sep=""),overwrite=TRUE)

## ---------Loading and preprocessing the data--------------------------
data <- read.csv(paste(curdir,'/activity.csv',sep=""))


## ---------What is mean total number of steps taken per day?--------------------------
data$date<-as.Date(data$date)
data$Weekday<-wday(data$date, label = TRUE, abbr = FALSE)



make.sums.ggplot<- function(data.dataframe, RBrewers.colors = "Greens"){
  
  #Transmform the data and get averages.

  #Sum up steps of the data.frame, grouped by date
  data.sums <- data.dataframe %>%
    group_by(date, Weekday) %>%
    summarise(total_steps = sum(steps))
  
  # Create a vector of brewer greens for representing the ordinal change from Monday to Sunday.
  # Brewer.pal takes an integer (n) and an RBrewer color set. 
  # It then creates (n) gradients of color that cover the passed in color set. 
  # 7 is passed in because there are 7 factors: each day of the week.
  my.cols <- brewer.pal(7, RBrewers.colors)
  # Ordered factors start on Sunday. Rather than reordering the date factor variables, we can just reassign the color for Saturday to Sunday as well. This will make the Weekends stand out.
  my.cols[1] <- my.cols[7]
  # Get max number of steps for largest interval for extending y-axis to fit labels.
  max.sum <- max(data.sums$total_steps, na.rm = TRUE)
  
  ggplot(data.sums, aes(x = date, y = total_steps, fill = Weekday)) + geom_bar(stat = "identity") +
    scale_x_date(breaks="1 day", 
                 limits = as.Date(c('2012-10-03','2012-11-28'))) +
    theme_wsj() +    
    theme(axis.text.x  = element_text(size=10,
                                      angle=45,
                                      colour="black",
                                      vjust=1,
                                      hjust=1)) + 
    scale_fill_manual(values = my.cols) + 
    geom_text(aes(x = date, 
                  y = total_steps, 
                  label = total_steps, 
                  angle  = 90, 
                  size = 5, 
                  hjust = -0.1), 
              color = "brown", 
              show_guide  = F) + 
    coord_cartesian(ylim=c(0,max.sum*1.15)) +
    geom_hline(aes ( yintercept = mean(total_steps, na.rm = TRUE)), 
               color = "chocolate3", 
               size = 1.5, 
               alpha = .50) + 
    geom_hline(aes ( yintercept = median(total_steps, na.rm = TRUE)), 
               color = "darkred", 
               alpha = .50) +
    geom_text(aes(label = paste("Overall Mean =", round(mean(total_steps, na.rm = TRUE), 2) ),
                  x = as.Date('2012-10-05'),
                  y = 20200), 
              color = "chocolate3", 
              size = 4) +
    geom_text(aes(label = paste("Overall Median = ", round(median(total_steps, na.rm = TRUE), 2) ),
                  x = as.Date('2012-10-05'),
                  y = 19700),
              color = "darkred",
              size = 4) +
    ylab("Total Steps taken per day") +
    xlab(NULL)
}

make.sums.ggplot(data, "Blues")
  
  
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

