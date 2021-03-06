# Reproducible Research: Peer Assessment 1
Herbert Amaya  


## Loading and preprocessing the data

Loading the data from activity.zip file.

```r
knitr::opts_chunk$set(fig.path='figure/',echo=TRUE)
data <- read.csv(unz("activity.zip","activity.csv"))
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


## What is mean total number of steps taken per day?

First, calculate the total number of steps taken per day. 

```r
data.dailyStepsAvg <- group_by(data,date)
data.dailyStepsAvg <- summarize(data.dailyStepsAvg,steps=sum(steps,na.rm=TRUE))
```

Drawing a histogram with the results.

```r
hist(data.dailyStepsAvg$steps,main = "Histogram of Total Number of Steps taken each day",xlab = "Steps per Day")
```

![](figure/unnamed-chunk-2-1.png) 

### Mean and Median Calculation

```r
mean(data.dailyStepsAvg$steps,na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(data.dailyStepsAvg$steps,na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Calculating the average number of steps taken per interval across all days.


```r
data.intervalAvg <- group_by(data,interval)
data.intervalAvg <- summarize(data.intervalAvg,avg_steps=mean(steps,na.rm=TRUE))
```

Drawing a time series plot with the results.

```r
library(ggplot2)
ggplot(data.intervalAvg,aes(interval,avg_steps)) + geom_line()
```

![](figure/unnamed-chunk-5-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#Sorting in descendant order, and then extracting the first element.
fiveMin.Interval.MaxSteps = arrange(data.intervalAvg,desc(avg_steps))[1,]
```
5-minute interval with the maximum number of steps:

```r
print.data.frame(fiveMin.Interval.MaxSteps)
```

```
##   interval avg_steps
## 1      835  206.1698
```

## Imputing missing values

Total Number of Missing values in the dataset

```r
missingValues <- sapply(data, function(x) sum(is.na(x)))
names(missingValues)[1]="Total missing values"
print(missingValues[1])
```

```
## Total missing values 
##                 2304
```
Filling in the missing values with the mean for the 5-minute interval, and creating a new data set.

```r
data.noMissing <- 
    mutate(data,
           steps=ifelse(is.na(steps),
                        data.intervalAvg[which(data.intervalAvg$interval == interval),]$avg_steps
                        ,steps))
```
Histogram of the total number of steps taken each day

```r
data.dailyStepsAvg <- group_by(data.noMissing,date)
data.dailyStepsAvg <- summarize(data.dailyStepsAvg,steps=sum(steps,na.rm=TRUE))
hist(data.dailyStepsAvg$steps,main = "Histogram of Total Number of Steps taken each day",xlab = "Steps per Day")
```

![](figure/unnamed-chunk-10-1.png) 

Mean and Median total number of steps taken per day

```r
mean(data.dailyStepsAvg$steps,na.rm=TRUE)
```

```
## [1] 9530.724
```

```r
median(data.dailyStepsAvg$steps,na.rm = TRUE)
```

```
## [1] 10439
```

###Do these values differ from the estimates from the first part of the assignment? 
Yes, they slightly differ.

###What is the impact of imputing missing data on the estimates of the total daily number of steps?
Imputing missing data increases the amount of 'valid data' for the analysis. For this particular case, it has slightly weighted on the final results.

## Are there differences in activity patterns between weekdays and weekends?

To answer this question some transformation of the data is required.

First, each individual record has to be classified as a weekend or a weekday.

```r
weekend.labels <- c("Saturday","Sunday")
day_type.levels <- c("weekend","weekday")
data.dayType <- mutate(data.noMissing,day_type = factor(ifelse(weekdays(as.Date(date)) %in% weekend.labels,"weekend","weekday"),levels = day_type.levels))
```

Once above step is done, the data has to be group by weekdays/weekends.

```r
data.dayType.intervalAvg <- group_by(data.dayType,interval,day_type)
data.dayType.intervalAvg <- summarize(data.dayType.intervalAvg,avg_steps=mean(steps,na.rm=TRUE))
```


Finally, now that the data has been properly processed, we will create a graph for comparing the average steps per interval for weekends versus weekdays.

```r
q <- qplot(interval,avg_steps,data=data.dayType.intervalAvg,facets=.~day_type,geom ="line") + facet_wrap( ~ day_type, nrow=2) 
print(q)
```

![](figure/unnamed-chunk-14-1.png) 

### Analysis of the results
During weekends the distribution of steps across the day seems to be more uniform than weekdays, for which there is a peak of steps from 750 to 1000 interval, follow by a sustantial decrease of steps for the rest of the day, with a small uptick from 1750 to 1900 interval.


