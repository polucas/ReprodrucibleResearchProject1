# PA1
Jed  
3 September 2017  


## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the ???quantified self??? movement ??? a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

*Dataset: Activity monitoring data [52K]
*The variables included in this dataset are:
*steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
*date: The date on which the measurement was taken in YYYY-MM-DD format
*interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment 

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)
Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data

1. Show any code that is needed to
2. Load the data (i.e. read.csv())
3. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity <- read.csv("C:/Docs/Data Science Course/ReproducibleRes/activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
library(ggplot2)
```

## Q1 What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

4. Calculate the total number of steps taken per day. If you do not understand the difference 
between a histogram and a barplot, research the difference between them. 
5. Make a histogram of the total number of steps taken each day
6. Calculate and report the mean and median of the total number of steps taken per day


```r
## Q1
activityNA <- activity[complete.cases(activity),]
str(activityNA)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
totalsteps <- aggregate( steps ~ date, data=activity, FUN=sum, na.rm=T)
str(totalsteps)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
ggplot(totalsteps, aes(x=steps)) + geom_histogram(fill="blue", col="black", binwidth=1000)+
      labs(title="Total daily steps per day", x="total number of steps", y="frequency")
```

![](Project1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meansteps <- mean(totalsteps$steps)
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps <- median(totalsteps$steps)
mediansteps
```

```
## [1] 10765
```

## Q2 What is the average daily activity pattern?
7. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
number of steps taken, averaged across all days (y-axis)
8. Which 5-minute interval, on average across all the days in the dataset, contains the maximum 
number of steps?


```r
## Q2
stepsperinterval <- aggregate(activityNA$steps ~ activityNA$interval, data=activityNA, FUN=mean)
head(stepsperinterval)
```

```
##   activityNA$interval activityNA$steps
## 1                   0        1.7169811
## 2                   5        0.3396226
## 3                  10        0.1320755
## 4                  15        0.1509434
## 5                  20        0.0754717
## 6                  25        2.0943396
```

```r
ggplot(stepsperinterval, aes(x=stepsperinterval[,1], y=stepsperinterval[,2])) + 
      geom_line(color="blue", size=1) + 
      labs(title="Average daily steps per interval", x="Interval", y="average steps per day")
```

![](Project1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
ordered <- stepsperinterval[order(-stepsperinterval[,2]),]
head(ordered)
```

```
##     activityNA$interval activityNA$steps
## 104                 835         206.1698
## 105                 840         195.9245
## 107                 850         183.3962
## 106                 845         179.5660
## 103                 830         177.3019
## 101                 820         171.1509
```

```r
maxinterval <- ordered[1,]
maxinterval
```

```
##     activityNA$interval activityNA$steps
## 104                 835         206.1698
```

## Q3 Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). 
The presence of missing days may introduce bias into some calculations or summaries of the data.
9. Calculate and report the total number of missing values in the dataset (i.e. the total number of 
rows with NAs)
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not 
need to be sophisticated. For example, you could use the mean/median for that day, or the mean for 
that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
10. Make a histogram of the total number of steps taken each day and Calculate and report the mean
and median total number of steps taken per day. Do these values differ from the estimates from the 
first part of the assignment? What is the impact of imputing missing data on the estimates of the 
total daily number of steps?


```r
## Q3
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
activity1 <- activity
na <- is.na(activity1$steps)
avgint <- tapply(activity1$steps, activity1$interval, mean, na.rm=T)
activity1$steps[na] <- avgint[as.character(activity1$interval[na])]
head(activity1)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
sum(is.na(activity1$steps))
```

```
## [1] 0
```

```r
totalsteps1 <- aggregate( steps ~ date, data=activity1, FUN=sum)
ggplot(totalsteps1, aes(x=steps)) + geom_histogram(fill="blue", col="black", binwidth=1000)+
    labs(title="Total daily steps per day", x="total number of steps", y="frequency")
```

![](Project1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
summary(totalsteps)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
summary(totalsteps1)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

## Q4 Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
11. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating
whether a given date is a weekday or weekend day.
12. Make a panel plot containing a time series plot (i.e. type = "l" ) of the 5-minute interval 
(x-axis) and the average number of steps taken, averaged across all weekday days or weekend days 
(y-axis). See the README file in the GitHub repository to see an example of what this plot should 
look like using simulated data.


```r
## Q4
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity1$day = as.factor(ifelse(is.element(weekdays(as.Date(activity1$date)),weekdays), 
                                    "Weekday", "Weekend"))
head(activity1)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 Weekday
## 2 0.3396226 2012-10-01        5 Weekday
## 3 0.1320755 2012-10-01       10 Weekday
## 4 0.1509434 2012-10-01       15 Weekday
## 5 0.0754717 2012-10-01       20 Weekday
## 6 2.0943396 2012-10-01       25 Weekday
```

```r
stepsperintervalperday <- aggregate(activity1$steps ~ activity1$interval + activity1$day, 
                                    data=activity1, FUN=mean)
head(stepsperintervalperday)
```

```
##   activity1$interval activity1$day activity1$steps
## 1                  0       Weekday      2.25115304
## 2                  5       Weekday      0.44528302
## 3                 10       Weekday      0.17316562
## 4                 15       Weekday      0.19790356
## 5                 20       Weekday      0.09895178
## 6                 25       Weekday      1.59035639
```

```r
ggplot(stepsperintervalperday, aes(x=stepsperintervalperday[,1], y=stepsperintervalperday[,3], 
                                   color = stepsperintervalperday[,2])) + 
    geom_line(size=1) +
    labs(title="Average daily steps per interval per day type", x="interval", 
         y="average steps per day") + facet_wrap (~ stepsperintervalperday[,2], ncol=1, nrow=2)
```

![](Project1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->












