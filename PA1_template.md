---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv(unz("activity.zip", "activity.csv"), colClasses=c("numeric","Date", "numeric"))
```

## What is mean total number of steps taken per day?

```r
library(dplyr)
days <- group_by(data,date)
stepsperday <- summarize(days,steps = sum(steps, na.rm = TRUE))

hist(stepsperday$steps,
        main= "Steps per day",
                xlab="Number of steps", ylab="Frequency")
steps_mean <- mean(stepsperday$steps,na.rm = TRUE)
lines(c(steps_mean,steps_mean), c(0,40), col = "blue", lty = 2)
steps_median <- median(stepsperday$steps, na.rm = TRUE)
lines(c(steps_median,steps_median), c(0,40), col = "darkblue", lty = 3)
text(steps_mean, 25 , cex = 0.8, pos = 2, paste("mean:",format(steps_mean, digits=2, nsmall=2)))
text(steps_median, 25 ,cex = 0.8, pos = 4, paste("median:",format(steps_median, digits=2, nsmall=2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is the average daily activity pattern?

```r
interval <- group_by(data,interval)
stepsperperiod <- summarize(interval,steps = mean(steps, na.rm = TRUE))
max_interval <- stepsperperiod[which.max(stepsperperiod$steps),]$interval
plot(stepsperperiod$interval, stepsperperiod$steps, type= "l", xlab= "Interval", ylab= "Number of Steps (Avarage)")
lines(c(max_interval,max_interval), c(-10,200), col = "red", lty = 3)
text(max_interval, 150 ,cex = 0.8, pos = 4, paste("Max. interval: ",max_interval))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Imputing missing values
### counting NAs


```r
table(is.na(data$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```
### filling missing values with interval mean


```r
data_new <- data
data_new <- mutate(data_new, steps = ifelse(is.na(steps), stepsperperiod[which(stepsperperiod$interval == interval),]$steps[1], steps))
```

### histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day


```r
days <- group_by(data_new,date)
stepsperday <- summarize(days,steps = sum(steps, na.rm = TRUE))

hist(stepsperday$steps,
        main= "Steps per day with imputing missing data ",
                xlab="Number of steps", ylab="Frequency")
steps_mean1 <- mean(stepsperday$steps,na.rm = TRUE)
lines(c(steps_mean,steps_mean), c(0,40), col = "blue", lty = 2)
steps_median1 <- median(stepsperday$steps, na.rm = TRUE)
lines(c(steps_median,steps_median), c(0,40), col = "gray", lty = 3)
lines(c(steps_median1,steps_median1), c(0,40), col = "darkblue", lty = 3)
text(steps_mean1, 25 , cex = 0.8, pos = 2, paste("mean:",format(steps_mean1, digits=2, nsmall=2)))
text(steps_median1, 25 ,cex = 0.8, pos = 4, paste("median:",format(steps_median1, digits=2, nsmall=2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

>The histogram above is made from the dataset with the NA values replaced with the mean number of the steps of the respective 5 minutes interval.  
We can see a small variation on the histogram, comparing to the original, with mean value slightly increasing. 

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
data_new <- mutate(data_new, weekday = ifelse(weekdays(date)=="domingo" | weekdays(date)=="sábado","weekend", "weekday" ))
data_new$weekday <- as.factor(data_new$weekday)

interval_week <- group_by(data_new,weekday,interval)
steps_week <- summarize(interval_week,steps = mean(steps,na.rm = TRUE))

xyplot(steps ~ interval | weekday, data = steps_week, layout= c(1,2), type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



