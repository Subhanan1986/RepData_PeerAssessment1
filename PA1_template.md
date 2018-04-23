---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv())

```r
dataset_activity <- read.csv("activity.csv", as.is = TRUE)
```
##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
dataset_activity_filter <- na.omit(dataset_activity)
```

## What is mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(steps~ date , dataset_activity_filter ,sum)
```
##### 2. Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, main = "Histogram: Total number of steps per day",xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
##### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_steps_per_interval <- aggregate(steps ~ interval, dataset_activity_filter, mean)
avg_steps_per_day <- aggregate(steps ~ date, dataset_activity_filter, mean)
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
hight_avg_steps <- which.max(avg_steps_per_interval$steps)
print (avg_steps_per_interval[hight_avg_steps,2])
```

```
## [1] 206.1698
```
## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
no_of_missing_val <- table(is.na(dataset_activity))
no_of_missing_val
```

```
## 
## FALSE  TRUE 
## 50400  2304
```
##### 2. Identify the avg steps for that interval in avg_steps_per_interval and Substitute the NA value with that value

```r
for (i in 1:nrow(dataset_activity)) {
  if(is.na(dataset_activity$steps[i])){
    value_avg <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == dataset_activity$interval[i])]
    dataset_activity$steps[i] <- value_avg
  }
}
```
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_per_day_impute <- aggregate(steps~ date , dataset_activity ,sum)
hist(steps_per_day_impute$steps, main = "Histogram of total number of steps per day (IMPUTED)", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(steps_per_day_impute$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_impute$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
week_day <- function(date_val) {
  wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
  if  (!(wd == 'Saturday' || wd == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}
```

##### 2. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dataset_activity$day_type <- as.factor(sapply(dataset_activity$date, week_day))
steps_per_day_impute <- aggregate(steps ~ interval+day_type, dataset_activity, mean)
library(ggplot2)
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = day_type)) +
  theme_gray() +
  facet_grid(day_type ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y=expression("No of Steps")) +
  ggtitle("No of steps Per Interval by day type")
print(plt)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

