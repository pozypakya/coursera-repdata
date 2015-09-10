
# Reproducible Research: Peer Assessment 2

# > load all the libraries


# # ```r
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if (is.installed('dplyr') == 'FALSE') {install.packages("dplyr");library(dplyr)} else{library(dplyr)}
if (is.installed('ggthemes') == 'FALSE') {install.packages("ggthemes");library(ggthemes)} else{library(ggthemes)}
if (is.installed('scales') == 'FALSE') {install.packages("scales");library(scales)} else{library(scales)}
if (is.installed('RColorBrewer') == 'FALSE') {install.packages("RColorBrewer");library(RColorBrewer)} else{library(RColorBrewer)}
if (is.installed('lubridate') == 'FALSE') {install.packages("lubridate");library(lubridate)} else{library(lubridate)}
if (is.installed('ggplot2') == 'FALSE') {install.packages("ggplot2");library(ggplot2)} else{library(ggplot2)}
if (is.installed('plyr') == 'FALSE') {install.packages("plyr");library(plyr)} else{library(plyr)}
if (is.installed('knitr') == 'FALSE') {install.packages("knitr");library(knitr)} else{library(knitr)}
if (is.installed('lattice') == 'FALSE') {install.packages("lattice");library(lattice)} else{library(lattice)}
if (is.installed('RCurl') == 'FALSE') {install.packages("RCurl");library(RCurl)} else{library(RCurl)}
if (is.installed('reshape') == 'FALSE') {install.packages("reshape");library(reshape)} else{library(reshape)}
if (is.installed('car') == 'FALSE') {install.packages("car");library(car)} else{library(car)}
if (is.installed('gridExtra') == 'FALSE') {install.packages("gridExtra");library(gridExtra)} else{library(gridExtra)}
if (is.installed('grid') == 'FALSE') {install.packages("grid");library(grid)} else{library(grid)}
if (is.installed('xtable') == 'FALSE') {install.packages("xtable");library(xtable)} else{library(xtable)}

# ```
## Loading and preprocessing the data

# > set working directory
# # ```r
curdir <-getwd()
file.url<-'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(file.url,destfile=paste(curdir,'/repdata%2Fdata%2FStormData.csv.bz2',sep=""))
# ```
# > Read the CSV
# # ```r
# storm <- read.csv(bzfile(paste(curdir,'/repdata%2Fdata%2FStormData.csv.bz2',sep="")),nrows=1500)
storm <- read.csv(bzfile("c://repdata%2Fdata%2FStormData.csv.bz2"))
# tail(storm)
#
length(unique(storm$EVTYPE))
event_types <- tolower(storm$EVTYPE)
# replace all punct. characters with a space
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
length(unique(event_types))

capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

storm$EVTYPE <- sapply(tolower(storm$EVTYPE),capitalize)

library(plyr)
casualties <- ddply(storm, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused most death and injury
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 10)
fatal_events[, c("EVTYPE", "fatalities")]
injury_events[, c("EVTYPE", "injuries")]

exp_transform <- function(e) {
    # h -> hundred, k -> thousand, m -> million, b -> billion
    if (e %in% c('h', 'H'))
        return(2)
    else if (e %in% c('k', 'K'))
        return(3)
    else if (e %in% c('m', 'M'))
        return(6)
    else if (e %in% c('b', 'B'))
        return(9)
    else if (!is.na(as.numeric(e))) # if a digit
        return(as.numeric(e))
    else if (e %in% c('', '-', '?', '+'))
        return(0)
    else {
        stop("Invalid exponent value.")
    }
}

prop_dmg_exp <- sapply(storm$PROPDMGEXP, FUN=exp_transform)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(storm$CROPDMGEXP, FUN=exp_transform)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_exp)
econ_loss <- ddply(storm, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))
				   

# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)


prop_dmg_events[, c("EVTYPE", "prop_dmg")]
crop_dmg_events[, c("EVTYPE", "crop_dmg")]

# Set the levels in order
p1 <- ggplot(data=fatal_events,
             aes(x=reorder(EVTYPE, fatalities), y=fatalities, fill=fatalities)) +
    geom_bar(stat="identity") +
    coord_flip() +
    ylab("Total number of fatalities") +
    xlab("Event type") +
    theme(legend.position="none")

p2 <- ggplot(data=injury_events,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    ylab("Total number of injuries") +
    xlab("Event type") +
    theme(legend.position="none")
	
grid.arrange(p1, p2 , ncol=1, nrow=2, top = "Top deadly weather events in the US (1950-2011)")



library(ggplot2)
library(gridExtra)
# Set the levels in order
p1 <- ggplot(data=prop_dmg_events,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
    geom_bar(stat="identity") +
    coord_flip() +
    xlab("Event type") +
    ylab("Property damage in dollars (log-scale)") +
    theme(legend.position="none")

p2 <- ggplot(data=crop_dmg_events,
             aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    xlab("Event type") +
    ylab("Crop damage in dollars") + 
    theme(legend.position="none")
	
grid.arrange(p1, p2 , ncol=1, nrow=2, top = "Weather costs to the US economy (1950-2011)")
	
#print(xtable(data.frame(fatal_events), caption="Storm Events ranked by Injuries Caused"), type="html")








