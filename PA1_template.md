# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data

```r
activity<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps

```r
steps_day<-aggregate(activity$steps,by=list(activity$date),"sum",na.rm=TRUE)
names(steps_day)<-c("date","steps")
```

2. Make a histogram of the total number of steps taken each day

```r
hist(steps_day$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the tota number of steps taken per day.

```r
mean(steps_day$steps)
```

```
## [1] 9354.23
```

```r
median(steps_day$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1. Make a time series plot

```r
steps_avg<-aggregate(activity$steps,by=list(activity$interval),"mean",na.rm=TRUE)
names(steps_avg)<-c("interval","steps_avg")
plot(steps_avg$interval,steps_avg$steps_avg,xlab="interval",ylab="steps_avg",type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

answer: 835-minute inverval

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
steps2<-merge(activity,steps_avg,by="interval")
steps3<-steps2[order(steps2$date,steps2$interval),]
steps4<-steps3
for(i in 1:nrow(steps4)){
        if(is.na(steps4[i,"steps"])) steps4[i,"steps"]<-steps4[i,"steps_avg"]
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps5<-aggregate(steps4$steps,by=list(steps4$date),"sum",na.rm=TRUE)
names(steps5)<-c("date","steps")
hist(steps5$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(steps5$steps)
```

```
## [1] 10766.19
```

```r
median(steps5$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels weekday and weekend indicating whether a given date is a weekday or weekend day.

```r
steps4$weekdays<-weekdays(as.Date(steps4$date))
for(i in 1:nrow(steps4)){
        if((steps4[i,"weekdays"]=="Saturday")|(steps4[i,"weekdays"]=="Sunday"))
        steps4[i,"fday"]<-"weekend"
        else steps4[i,"fday"]<-"weekday"
}
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
steps4wd<-steps4[which(steps4$fday=="weekday"),]
steps4we<-steps4[which(steps4$fday=="weekend"),]
steps4wd_avg<-aggregate(steps4wd$steps,by=list(steps4wd$interval),"mean",na.rm=TRUE)
names(steps4wd_avg)<-c("interval","steps_avg")
steps4wd_avg$fday<-"weekday"
steps4we_avg<-aggregate(steps4we$steps,by=list(steps4we$interval),"mean",na.rm=TRUE)
names(steps4we_avg)<-c("interval","steps_avg")
steps4we_avg$fday<-"weekend"
steps4w_avg<-rbind(steps4wd_avg,steps4we_avg)
library(lattice)
xyplot(steps_avg~interval|fday,data=steps4w_avg,type="l",layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
