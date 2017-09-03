getwd()

activity <- read.csv("C:/Docs/Data Science Course/ReproducibleRes/activity.csv")
str(activity)
library(ggplot2)
## Q1
activityNA <- activity[complete.cases(activity),]
str(activityNA)
totalsteps <- aggregate( steps ~ date, data=activity, FUN=sum, na.rm=T)
str(totalsteps)
ggplot(totalsteps, aes(x=steps)) + geom_histogram(fill="blue", col="black", binwidth=1000)+
      labs(title="Total daily steps per day", x="total number of steps", y="frequency")
meansteps <- mean(totalsteps$steps)
meansteps
mediansteps <- median(totalsteps$steps)
mediansteps

## Q2
stepsperinterval <- aggregate(activityNA$steps ~ activityNA$interval, data=activityNA, FUN=mean)
head(stepsperinterval)
ggplot(stepsperinterval, aes(x=stepsperinterval[,1], y=stepsperinterval[,2])) + 
      geom_line(color="blue", size=1) + 
      labs(title="Average daily steps per interval", x="Interval", y="average steps per day")

ordered <- stepsperinterval[order(-stepsperinterval[,2]),]
head(ordered)
maxinterval <- ordered[1,]
maxinterval

## Q3
sum(is.na(activity$steps))
activity1 <- activity
na <- is.na(activity1$steps)
avgint <- tapply(activity1$steps, activity1$interval, mean, na.rm=T)
activity1$steps[na] <- avgint[as.character(activity1$interval[na])]
head(activity1)
sum(is.na(activity1$steps))
totalsteps1 <- aggregate( steps ~ date, data=activity1, FUN=sum)
ggplot(totalsteps1, aes(x=steps)) + geom_histogram(fill="blue", col="black", binwidth=1000)+
    labs(title="Total daily steps per day", x="total number of steps", y="frequency")
summary(totalsteps)
summary(totalsteps1)

## Q4
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity1$day = as.factor(ifelse(is.element(weekdays(as.Date(activity1$date)),weekdays), 
                                    "Weekday", "Weekend"))
head(activity1)
stepsperintervalperday <- aggregate(activity1$steps ~ activity1$interval + activity1$day, 
                                    data=activity1, FUN=mean)
head(stepsperintervalperday)
ggplot(stepsperintervalperday, aes(x=stepsperintervalperday[,1], y=stepsperintervalperday[,3], 
                                   color = stepsperintervalperday[,2])) + 
    geom_line(size=1) +
    labs(title="Average daily steps per interval per day type", x="interval", 
         y="average steps per day") + facet_wrap (~ stepsperintervalperday[,2], ncol=1, nrow=2)
    








