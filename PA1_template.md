---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

Load Necessary Packages


```r
library(ggplot2)
library(plyr)
library(dplyr)
library(timeDate)
```

## Loading and preprocessing the data

```r
setwd("F:/Courses/Coursera/John_Hopkins_University_Data_Science/5_Reproducible_Research/Week 2/RepData_PeerAssessment1")
unzip(zipfile = "activity.zip")
Activity <- read.csv("activity.csv", 
                 header = TRUE, 
                 colClasses = c("numeric", "Date", "numeric"))
```

What is mean total number of steps taken per day?

```r
total_steps <- tapply(Activity$steps, 
                      Activity$date, 
                      FUN = sum, 
                      na.rm = TRUE)
qplot(total_steps, 
      binwidth = 1000, 
      xlab = "Total number of steps taken/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(total_steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(total_steps, na.rm = TRUE)
```

```
## [1] 10395
```


What is the average daily activity pattern?

```r
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
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval contains the maximum number of steps


```r
average[which.max(average$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

Strategt of replace missing value
If calculate the mean number of steps per 5-minute interval
Then this can be added to the dataset as NA value
First, create a copy of data set with NA rows and remove all rows with NA
and also define the weekdays

```r
Activity$weekday <- weekdays(Activity$date)
Activity_NA <- Activity
Activity_NoNA <- Activity[complete.cases(Activity),]
```

Second, calculate the mean per day and interval

```r
mean_weekday <- ddply(Activity_NA, 
                      .(interval, weekday),
                      summarise,
                      steps = round(mean(steps,
                                         na.rm = TRUE),2))
```
Total number of steps per day 

```r
Mean_Steps <- ddply(Activity_NA, 
                    .(interval),
                    summarise,
                    steps = mean(steps,
                                 na.rm = TRUE))
```
Get list of indices of NA

```r
naIdx <- which(is.na(Activity_NA$steps))
```
Merge Activity_NA with Mean_Steps

```r
merged_NA <- merge(Activity_NA, 
                   Mean_Steps,
                   by = "interval",
                   suffixes = c(".actual", ".stepsInt"))
```
Create new dataset with merged

```r
Activity_Complete <- Activity_NA
```
Replace NA with steps value

```r
Activity_Complete[naIdx, "steps"] <- merged_NA[naIdx, "steps.stepsInt"]
```

Check

```r
paste("Missing values in new dataset ", sum(is.na(Activity_Complete)))
```

```
## [1] "Missing values in new dataset  0"
```
Should return 0

Calculate total number of steps per day

```r
Steps_Day <- ddply(Activity_Complete, 
                   .(date), 
                   summarise,
                   steps = round(sum(steps, na.rm = TRUE), 0))
```

Create new dataset with NA filled and plot histogram

```r
plot(Steps_Day$date,
     Steps_Day$steps,
     type = "h",
     main = "Histogram of daily steps",
     xlab = "Date",
     ylab = "Steps per day")
abline(h = mean(Steps_Day$steps, na.rm = TRUE),
       col = "red",
       lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
Calculate mean and median

```r
Sum_Steps_Day <- ddply(Activity_Complete, 
                       .(date),
                       summarise,
                       steps = sum(steps))
```
Mean

```r
paste("Mean steps per day is ", round(mean(Sum_Steps_Day$steps), 0))
```

```
## [1] "Mean steps per day is  10890"
```
Median

```r
paste("Median steps per day is ", round(median(Sum_Steps_Day$steps), 0))
```

```
## [1] "Median steps per day is  11015"
```


## Are there differences in activity patterns between weekdays and weekends?

Set weekday or weekend

```r
Activity_Complete$daytype <- lapply(Activity_Complete$date, 
                                    function(x) ifelse(isWeekday(x, wday = 1:5), 
                                                                               'weekday', 
                                                                               'weekend'))
```
Convert list to vector

```r
Activity_Complete$daytype <- unlist(Activity_Complete$daytype)
```
Create factor variable

```r
Activity_Complete$daytype <- as.factor(Activity_Complete$daytype)

Day_Interval_Steps <- ddply(Activity_Complete, 
                            .(interval, daytype),
                            summarise,
                            steps = mean(steps,
                                         na.rm = TRUE))
```

Plot the time series

```r
ggplot(Day_Interval_Steps, 
       aes(x = interval,
           y = steps)) +
        geom_line(col = "red") + 
        labs(title = "Number of Steps per interval (weekday/weekend)",
             x = "Interval",
             y = "Number of steps") +
        facet_grid(daytype ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
