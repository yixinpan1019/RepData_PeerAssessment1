# Reproducible Research: Peer Assessment 1

```r
library(lattice)
```



## Loading and preprocessing the data
Load csv data **activity.csv** and convert dates to **R Date class**  

```r
activity_data <- read.csv("activity.csv")
activity_data$date <- as.Date(activity_data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
Compute total number of steps per day  

```r
total_steps_per_day <- tapply(activity_data$steps, activity_data$date, sum)
```

Plot histogram of **total number of steps per day**

```r
hist(total_steps_per_day, col = "blue", xlab = "Total Steps per Day", ylab = "Frequency", 
    main = "Histogram of Total Steps taken per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Compute Mean total steps taken per day

```r
mean(total_steps_per_day, na.rm = TRUE)
```

```
## [1] 10766
```


Compute Median total steps taken per day

```r
median(total_steps_per_day, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Compute mean of steps over all days by time interval

```r
mean_steps_by_interval <- tapply(activity_data$steps, activity_data$interval, 
    mean, na.rm = TRUE)
```

Timeseries plot of of the 5-minute interval and the average number of steps taken, averaged across all days

```r
plot(row.names(mean_steps_by_interval), mean_steps_by_interval, type = "l", 
    xlab = "Time Intervals (5-minute)", ylab = "Mean number of steps taken (all Days)", 
    main = "Average number of Steps Taken at different 5 minute Intervals", 
    col = "blue")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Find the time interval that contains maximum average number of steps over all days

```r
interval_num <- which.max(mean_steps_by_interval)
interval_max_steps <- names(interval_num)
interval_max_steps
```

```
## [1] "835"
```

The ** 835** minute  or ** 104th ** 5 minute interval contains the maximum number of steps on average across all the days


## Imputing missing values
Compute the number of NA values in the activity dataset

```r
num_na_values <- sum(is.na(activity_data))
num_na_values  #Print number of NA values
```

```
## [1] 2304
```


Fill in missing values using the **average interval value across all days**

```r
na_indices <- which(is.na(activity_data))
imputed_values <- mean_steps_by_interval[as.character(activity_data[na_indices, 
    3])]
names(imputed_values) <- na_indices
for (i in na_indices) {
    activity_data$steps[i] = imputed_values[as.character(i)]
}
sum(is.na(activity_data))  # The number of NAs after imptation should be 0
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?


```r
days <- weekdays(activity_data$date)
activity_data$day_type <- ifelse(days == "Saturday" | days == "Sunday", "Weekend", 
    "Weekday")
mean_steps_by_interval <- aggregate(activity_data$steps, by = list(activity_data$interval, 
    activity_data$day_type), mean)
names(mean_steps_by_interval) <- c("interval", "day_type", "steps")
xyplot(steps ~ interval | day_type, mean_steps_by_interval, type = "l", layout = c(1, 
    2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

Let's compute the mean, median, max and min of the steps across all intervals and days by Weekdays/Weekends

```r
tapply(mean_steps_by_interval$steps, mean_steps_by_interval$day_type, function(x) {
    c(MIN = min(x), MEAN = mean(x), MEDIAN = median(x), MAX = max(x))
})
```

```
## $Weekday
##    MIN   MEAN MEDIAN    MAX 
##   0.00  35.61  25.80 230.38 
## 
## $Weekend
##    MIN   MEAN MEDIAN    MAX 
##   0.00  42.37  32.34 166.64
```



