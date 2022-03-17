---
title: "PA1_template"
author: "Desamours Yvelt"
date: "2022-03-17"
output: 
  html_document: 
    keep_md: yes
---


Load all needed packages


```r
library(ggplot2)
library(dplyr)
```

Reading in the dataset and/or processing the data
=================================================

Load the dataset


```r
data = read.csv('activity.csv')
```

Some informations about the dataset


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

What is mean total number of steps taken per day?
=================================================

Histogram of the total number of steps taken each day


```r
totalStepsPerDay = summarise(group_by(data, date), totalsteps=sum(steps, na.rm = TRUE))
ggplot(totalStepsPerDay) + geom_histogram(aes(totalsteps), na.rm=TRUE)
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

Mean total number of steps taken per day


```r
(meanValue = mean(totalStepsPerDay$totalsteps, na.rm=TRUE))
```

```
## [1] 9354.23
```

Median total number of steps taken per day

```r
(medianValue = median(totalStepsPerDay$totalsteps, na.rm=TRUE))
```

```
## [1] 10395
```

  
  
What is the average daily activity pattern?
===========================================

Series plot of the 5-minute interval and the average number of steps taken, maveraged across all days


```r
meanStepsPerInterval = summarise(group_by(data, interval), meansteps=mean(steps, na.rm=TRUE))
ggplot(meanStepsPerInterval, aes(x=interval, meansteps)) + geom_line()
```

![](PA1_template_files/figure-html/time serie plot-1.png)<!-- -->

The 5-minute interval that, on average, contains the maximum number of steps

```r
maxValue =max(meanStepsPerInterval$meansteps) 
meanStepsPerInterval[match( maxValue,meanStepsPerInterval$meansteps), 2]
```

```
## # A tibble: 1 × 1
##   meansteps
##       <dbl>
## 1      206.
```
  
  
Imputing missing values
=======================

Total number of missing values


```r
sum(is.na(data))
```

```
## [1] 2304
```

Strategy for filling missing value

Missing value are replaced by the mean of total steps corresponding to that 5-minutes interval.

New dataset with missing values filled.


```r
newData = data.frame(data)
newData$steps = ifelse(is.na(newData$steps), meanStepsPerInterval$meansteps, newData$steps)
```

Histogram of the total number of steps taken each day after missing values are imputed


```r
newTotalStepPerDays = summarise(group_by(newData, date), totalsteps=sum(steps, na.rm = TRUE))
ggplot(newTotalStepPerDays) + geom_histogram(aes(totalsteps))
```

![](PA1_template_files/figure-html/new hist-1.png)<!-- -->

Mean total number of steps taken per day


```r
(mean(newTotalStepPerDays$totalsteps, na.rm = TRUE))
```

```
## [1] 10766.19
```

Median total number of steps taken per day

```r
(median(newTotalStepPerDays$totalsteps, na.rm = TRUE))
```

```
## [1] 10766.19
```

These values are now different from the estimates from the first part of the assignment.
Imputing missing data causes an increase of the estimates of the total daily number of steps.
  
  
Are there differences in activity patterns between weekdays and weekends?
=========================================================================
A new factor variable is created in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
days = weekdays(as.Date(newData$date))
weekPart = as.factor(ifelse(days=='Saturday' | days == 'Sunday', 'weekend', 'weekday'))
newData = cbind(newData, weekPart)
```

A preview of the new dataset

```r
str(newData)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekPart: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

A panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
newMeanStepsPerInterval = summarise(group_by(newData, interval), meansteps=mean(steps, na.rm=TRUE), weekpart=weekPart)
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

```r
ggplot(newMeanStepsPerInterval, aes(interval, meansteps)) + geom_line()+facet_wrap(facets = vars(weekpart))
```

![](PA1_template_files/figure-html/panel plot-1.png)<!-- -->
