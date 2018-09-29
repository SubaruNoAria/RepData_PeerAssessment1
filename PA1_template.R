#This is the R code to read and process the data

##============Load data======================================================
##1. Loading and preprocessing the data======================================
setwd("./RepData_PeerAssessment1")
unzip(zipfile = "activity.zip")
Activity <- read.csv("activity.csv", 
                 header = TRUE, 
                 colClasses = c("numeric", "Date", "numeric"))

##============Calculate the mean total number================================
##2. What is mean total number of steps taken per day?=======================
library(ggplot2)
total_steps <- tapply(Activity$steps, 
                      Activity$date, 
                      FUN = sum, 
                      na.rm = TRUE)
qplot(total_steps, 
      binwidth = 1000, 
      xlab = "Total number of steps taken/day")
mean(total_steps, na.rm = TRUE)
median(total_steps, na.rm = TRUE)

##============Average daily activity=========================================
##3. What is the average daily activity pattern?=============================
average <- aggregate(x = list(steps = Activity$steps), 
                   by = list(interval = Activity$interval),
                   FUN = mean, 
                   na.rm = TRUE)
ggplot(data = average, aes(x = interval,
                           y = steps)) +
        geom_line() + 
        labs(title = "Average daily activity",
             x = "5-minute interval",
             y = "average number of steps taken")
##Which 5-minute interval contains the maximum number of steps
average[which.max(average$steps),]
##============Missing values=================================================
##4. Imputing missing values=================================================
##Total number of missing value
MissValue <- is.na(Activity$steps)
table(MissValue)

##Strategt of replace missing value
##If calculate the mean number of steps per 5-minute interval
##Then this can be added to the dataset as NA value
library(plyr)
library(dplyr)
library(timeDate)
##First, create a copy of data set with NA rows and remove all rows with NA
Activity_NA <- Activity
Activity_NoNA <- Activity[complete.cases(Activity),]
##Also define the weekdays
Activity$weekday <- weekdays(Activity$date)
##Second, calculate the mean per day and interval
mean_weekday <- ddply(Activity_NA, 
                      .(interval, weekday),
                      summarise,
                      steps = round(mean(steps,
                                         na.rm = TRUE),2))
##Total number of steps per day 
Mean_Steps <- ddply(Activity_NA, 
                    .(interval),
                    summarise,
                    steps = mean(steps,
                                 na.rm = TRUE))
##Get list of indices of NA
naIdx <- which(is.na(Activity_NA$steps))
##Merge Activity_NA with Mean_Steps
merged_NA <- merge(Activity_NA, 
                   Mean_Steps,
                   by = "interval",
                   suffixes = c(".actual", ".stepsInt"))

##Create new dataset with merged
Activity_Complete <- Activity_NA

##Replace NA with steps value
Activity_Complete[naIdx, "steps"] <- merged_NA[naIdx, "steps.stepsInt"]

##Check
paste("Missing values in new dataset ", sum(is.na(Activity_Complete)))
##Should return 0

##Calculate total number of steps per day
Steps_Day <- ddply(Activity_Complete, 
                   .(date), 
                   summarise,
                   steps = round(sum(steps, na.rm = TRUE), 0))

##Create new dataset with NA filled and plot histogram
plot(Steps_Day$date,
     Steps_Day$steps,
     type = "h",
     main = "Histogram of daily steps",
     xlab = "Date",
     ylab = "Steps per day")
abline(h = mean(Steps_Day$steps, na.rm = TRUE),
       col = "red",
       lwd = 2)

##Calculate mean and median
Sum_Steps_Day <- ddply(Activity_Complete, 
                       .(date),
                       summarise,
                       steps = sum(steps))

#Mean
paste("Mean steps per day is ", round(mean(Sum_Steps_Day$steps), 0))
#Median
paste("Median steps per day is ", round(median(Sum_Steps_Day$steps), 0))

##============Differeces between weekdays and weekends=======================
##5. Are there differences in activity patterns between weekdays and weekends?
#Set weekday or weekend
Activity_Complete$daytype <- lapply(Activity_Complete$date, 
                                    function(x) ifelse(isWeekday(x, wday = 1:5), 
                                                                               'weekday', 
                                                                               'weekend'))

#Convert list to vector
Activity_Complete$daytype <- unlist(Activity_Complete$daytype)

#Create factor variable
Activity_Complete$daytype <- as.factor(Activity_Complete$daytype)

Day_Interval_Steps <- ddply(Activity_Complete, 
                            .(interval, daytype),
                            summarise,
                            steps = mean(steps,
                                         na.rm = TRUE))

#Plot the time series
ggplot(Day_Interval_Steps, 
       aes(x = interval,
           y = steps)) +
        geom_line(col = "red") + 
        labs(title = "Number of Steps per interval (weekday/weekend)",
             x = "Interval",
             y = "Number of steps") +
        facet_grid(daytype ~ .)
        
