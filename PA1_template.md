---
title: "Activity Monitoring Assignment"
author: "Alyssa Copeland"
date: "January 20, 2018"
output: html_document
---

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis

#What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day

##Histogram and Summary of Activity Monitoring Steps per Day

First, we will load the data and examine a histogram of the number of steps taken per day

```{r code for getting the data and histogram,fig.height=5,fig.width=7}
library(knitr);library(dplyr);library(lubridate)

##downloading data from the csv, assuming you have the .zip and extracted it to a csv within a different directory
activity<-read.csv("activity.csv",header=TRUE,sep=",")

##changing data to steps per day calculation
stepper <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)

##Histogram of the data with steps per day
hist(stepper, main="Histogram of Steps per Day",col="lightblue",breaks=40,
     xlab="# of steps taken per day")

```

Next, we will look at the summary of the activity monitoring data in regards to steps taken per day.

```{r summary of the activity monitoring}
##getting the mean and median from summary
summary(stepper)

```

Looking at the steps taken per day, the median number of steps taken per day = 10,395, and the mean number of steps taken per day = 9,354.

#What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

##Time Series Plot of Steps per Day 

Now we will look at the time series plot of the number of steps taken per day at 5 minute intervals

```{r graph of time series of steps taken,fig.height=6,fig.width=9}
##Aggregating data to avg steps into intervals of 5 minutes
avgstepts <- aggregate(x=list(meansteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)

##ggplot of aggregated data
library(ggplot2)
g=ggplot(avgstepts,aes(x=interval,y=meansteps)) 
g=g+geom_line()+xlab("5 minute intervals")+
  ylab("Mean # of steps taken per day")+
  ggtitle("Mean # of Steps per Day at 5 Minute Intervals")
g
```


```{r avgstepts for maximum}
##Max avg steps at what interval
avgstepts[which.max(avgstepts$meansteps),]

```

Interval 835 has the maximum number of steps on average with 206.  Meaning, in that 5 minute interval, people on average are walking 206 steps.

#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r missing data}
##Number of missing data
sum(is.na(activity$steps))
```

In this data set for activity monitoring, there are 2,304 missing values in the steps column

To replace the missing values, I will use the mean of the steps to fill in the NAs.  This would not be a preferred method because it can create bias or show us something that is not there.  But for comparison sakes, I will use this method.

```{r replacing NA with the mean for  steps in activity}
##Replacing all the NAs with the mean for the missing values in steps column
activity2<- activity
nas <- is.na(activity2$steps)
avgint<- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify=TRUE)
activity2$steps[nas] <- avgint[as.character(activity2$interval[nas])]

sum(is.na(activity2$steps))

```

This indicates that there is no more missing data.  Now, I will calculate the number of steps taken at each 5 min interval and graph it with the new data.

##Graph of Steps Taken at 5 Minute Intervals with New Data

```{r histogram of new data}
##New data with steps per day applied
stepper2 <- tapply(activity2$steps, activity2$date, sum, na.rm=TRUE)

##Histogram of the new data
hist(stepper2, main="Histogram of Steps per Day, New Data",col="Tomato1",breaks=40,
     xlab="# of steps taken per day")

```

```{r summary of new data}
summary(stepper2)

```

The new median steps per day = 10,766 and the mean steps per day = 10,766.

Due to using the mean to replace all of the NAs, the mean and median are now equal to each other, and they are different from the mean and median without the missing values being imputed (median=10,395 and mean=9,354).

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

##Looking at the Steps Taken During the Weekdays vs Weekends

```{r weekday and weekend data column addition and graphing}
##Change format of the date for easier conversion to weekday and weekend days
activity2$date <- as.POSIXct(activity2$date, format="%Y-%m-%d")

##Compute weekdays from the dates in the new data
activity2<- data.frame(date=activity2$date, 
                           weekday=tolower(weekdays(activity2$date)), 
                           steps=activity2$steps, 
                           interval=activity2$interval)

##compute a difference between weekday and weekend into a new column
activity2<- cbind(activity2, 
                      daytype=ifelse(activity2$weekday == "saturday" | 
                                     activity2$weekday == "sunday", "weekend", 
                                     "weekday"))

head(activity2)


```

```{r new data weekday and weekend with mean steps per day,fig.height=7,fig.width=8}
##Changing the data for the mean steps taken per day
new_stepmean <- activity2 %>%
  group_by(interval, daytype) %>%
  summarise(steps = mean(steps))

##ggplot of the new data with weekday compared to weekend steps per day
f <- ggplot(new_stepmean, aes(x=interval, y=steps, color = daytype))
f<-f+geom_line() +
  facet_wrap(~daytype, ncol = 1, nrow=2)+
  ggtitle("Weekday vs Weekend Mean Steps at 5 Minute Intervals")
f
```

Comparing the weekend vs weekday step taken at 5 minute intervals, it looks like on average, people are taking more steps during the weekend.  Though there is a spike on the weekday at that 835 interval that is higher than any weekend intervals, after that the steps remain usually between ~15-100 steps per interval.  The weekend steps on the other hand, flucuates from 25-150 steps per interval.  Thus, there is a greater spread of steps per interval on the weekends in comparison to the weekdays.  Probably because there is more variability in the activities that are done during the weekend; whereas the weekdays are much more predictable in activities (i.e. working).








