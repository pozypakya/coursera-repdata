# Reproducible Research: Peer Assessment 1

##load all the libraries

```r
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if (is.installed('dplyr') == 'FALSE') {install.packages("dplyr")} else{library(dplyr)}
if (is.installed('ggthemes') == 'FALSE') {install.packages("ggthemes")} else{library(ggthemes)}
if (is.installed('scales') == 'FALSE') {install.packages("scales")} else{library(scales)}
if (is.installed('RColorBrewer') == 'FALSE') {install.packages("RColorBrewer")} else{library(RColorBrewer)}
if (is.installed('lubridate') == 'FALSE') {install.packages("lubridate")} else{library(lubridate)}
if (is.installed('ggplot2') == 'FALSE') {install.packages("ggplot2")} else{library(ggplot2)}
if (is.installed('plyr') == 'FALSE') {install.packages("plyr")} else{library(plyr)}
if (is.installed('knitr') == 'FALSE') {install.packages("knitr")} else{library(knitr)}
```
## Loading and preprocessing the data

# set working directory
```r
curdir <-getwd()
file.url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(file.url,destfile=paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""))
unzip(paste(curdir,'/repdata%2Fdata%2Factivity.zip',sep=""),exdir=paste(curdir,sep=""),overwrite=TRUE)
```

## Loading and preprocessing the data
```r
data <- read.csv(paste(curdir,'/activity.csv',sep=""))
```

##Ignore missing value
```r
dataClean <- subset(data, is.na(data$steps) == F)
totalPerDay <- ddply(dataClean, .(date), summarise, steps=sum(steps))
```

## Plot /  Make a histogram of the total number of steps taken each day
```r
if("figure"%in%dir()==FALSE) dir.create("figure")
png(filename=paste(curdir,'/figure/unnamed-chunk-1.png',sep=""),width=480,height=480,units='px')
h <- hist(totalPerDay$steps , breaks = 20, main="Number of Steps", 
     xlab="Total number of steps taken each day", ylab = "Number of Days",col="red")
print(h)
graphics.off()
```

## Calculate and report the mean and median of the total number of steps taken per day
# mean
```r
mean(totalPerDay$steps)
```

# median
```r
medi
