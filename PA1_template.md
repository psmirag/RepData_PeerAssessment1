# Reproducible Research: Peer Assessment 1
### Pablo  Smiraglia - Nov-2015

## Loading and preprocessing the data

```r
sourceFile <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(sourceFile, temp, method="curl")
data <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
data$dateInterval <- as.POSIXct(with(data, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
    format="%Y-%m-%d %H:%M",tz="")
```

## What is mean total number of steps taken per day? 

```r
dailyTotals <- aggregate(steps~date, data, sum, na.rm = TRUE)
hist(dailyTotals$steps, main = "Histogram of total daily steps.", xlab ="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

What is mean total number of steps taken per day? 

```r
mean(dailyTotals$steps)
```

```
## [1] 10766.19
```

What is median total number of steps taken per day? 

```r
median(dailyTotals$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
intervalsMeans <- aggregate(steps~interval, data, mean, na.rm = TRUE)
plot(x=intervalsMeans$interval, y=intervalsMeans$steps, 
        type = "l", 
        ylab = "Steps", 
        xlab = "Interval", 
        main = "Average number of steps per interval (across all days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
which.max(intervalsMeans$steps)
```

```
## [1] 104
```

This is the maximum mean number of steps 

```r
max(intervalsMeans$steps)
```

```
## [1] 206.1698
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

We will replace the NA values with the mean for that interval across all days(from table intervalsTotals)

```r
data2 <- data
for (i in 1:nrow(data2)){
         if (is.na(data2$steps[i])){
                 data2$steps[i]<- round(intervalsMeans$steps[which(intervalsMeans$interval == data2$interval[i])],)
         }
}
```

Make a histogram of the total number of steps taken each day

```r
daily2Totals <- aggregate(steps~date, data, sum, na.rm = TRUE)
hist(daily2Totals$steps, main = "Histogram of total daily steps.", xlab ="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

What is mean total number of steps taken per day? 

```r
mean(daily2Totals$steps)
```

```
## [1] 10766.19
```

What is median total number of steps taken per day? 

```r
median(daily2Totals$steps)
```

```
## [1] 10765
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? : No, the median and mean did not change due to the strategy used to remove the NAs.

## Are there differences in activity patterns between weekdays and weekends?  
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data2$date <- as.Date(data2$date)
data2$wday <- "weekday"
for (i in 1:nrow(data2)){
         if (weekdays(data2$date[i]) %in% c("Sunday", "Saturday")){
                 data2$wday[i] <- "weekend"
         }
}
data2$wday <- as.factor(data2$wday)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  


```r
wdayMeans <- aggregate(steps~interval+wday, data2, mean)
library(lattice)
xyplot(steps ~ interval | wday, data=wdayMeans, layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

