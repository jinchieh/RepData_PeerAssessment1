---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## a. Loading and preprocessing the data

Download file from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

Make sure "activity.csv" is in the working directory.

### 1. Load the data

```{r}
library(data.table)
myData <- read.table("activity.csv", header=TRUE, na.strings="NA", sep=',')
```

Convert Date into a date variable:
```{r}
myData$date <- as.Date(myData$date, format="%Y-%m-%d")
```

Verify that variables are in the right format:
```{r}
str(myData)
```

## b. What is mean total number of steps taken per day?

Remove NA values:
```{r}
myData2 <- na.omit(myData)
```

### 1. Calculate the total number of steps taken per day
```{r}
sum.date <- aggregate(myData2[,1],by=list(myData2$date),sum)
names(sum.date) <- c("date","total_steps")
head(sum.date)
```

### 2. Make a histogram of the total number of steps taken each day
Load graphics packages "ggplot2" and plot.
```{r}
library(ggplot2)
qplot(x=total_steps, data=sum.date) + labs(y='Frequency', x='Total Number of Steps')
```

### 3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
summary(sum.date$total_steps)
```

Mean = 10770 steps per day, Median = 10760 steps per day

## c. What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
avg_per_interval <- aggregate(myData2[,1], by=list(myData2$interval),mean)
names(avg_per_interval) <- c("interval", "mean")
qplot(interval, mean, data=avg_per_interval, geom="line", stat="identity",xlab="Interval", ylab="Avg No. of Steps")
```

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max <- max(avg_per_interval$mean)
avg_per_interval[avg_per_interval$mean == max,]$interval
```

Interval 835, averaged over all days, contains the maximum number of steps (`r max` steps).

## d. Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(myData))
```

Total number of missing values is `r sum(is.na(myData))`.

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Replace missing values with the mean for each particular 5-minute interval.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Represent the new dataset by "myData3".

Create a new column representing the mean for each interval averaged across all days:
```{r}
myData3 <- merge(myData, avg_per_interval, by.x = "interval", by.y = "interval")
myData3 <- myData3[order(myData3[,3],myData3[,1]),]
myData3 <- subset(myData3[,c(2,3,1,4)])
myData3$date <- as.Date(myData3$date, format="%Y-%m-%d")
head(myData3)
```

Replace missing values with mean value for each interval:
```{r}
#create index of all cells with missing values
ind <- which(is.na(myData), arr.ind=TRUE)
#replace missing values for index
myData3[ind] <- myData3[,4]
head(myData3)
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Create a new data frame "sum.date2" representing the total number of steps taken for each day:
```{r}
sum.date2 <- aggregate(myData3[,1],by=list(myData3$date),sum)
names(sum.date2) <- c("date","total_steps")
head(sum.date2)
```

Plot histogram:
```{r}
library(ggplot2)
qplot(x=total_steps, data=sum.date2) + labs(y='Frequency', x='Total Number of Steps')
```

```{r}
summary(sum.date2$total_steps)
```

Mean = 10770 steps per day, Median = 10770 steps per day

There is little change in the mean and median even after inputing the missing values.

## e. Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
myData3$day <- weekdays(myData3$date)
myData3$weekday_weekend[myData3$day == "Saturday" | myData3$day == "Sunday"] <- "weekend" 
myData3$weekday_weekend[myData3$day != "Saturday" & myData3$day != "Sunday"] <- "weekday" 
myData3$weekday_weekend <- as.factor(myData3$weekday_weekend)
#Sample of result
rbind(myData3[1,],myData3[1500,])
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Create new data frame "avg_weekday_weekend" for the average number of steps taken, averaged across all weekday days or weekend days.

Use "plyr" package.
```{r}
library(plyr)
avg_weekday_weekend <- ddply(myData3, .(interval, weekday_weekend), summarize, mean = round(mean(steps), 3))
avg_weekday_weekend <- avg_weekday_weekend[order(avg_weekday_weekend[,2],avg_weekday_weekend[,1]),]
head(avg_weekday_weekend)
tail(avg_weekday_weekend)
```

Use "lattice" package to plot line graph.
```{r}
library(lattice)
xyplot(mean ~ interval | weekday_weekend, type = "l", data=avg_weekday_weekend,layout=c(1,2))
```

As shown from the graph, excluding the mornings between around 0500 and 1000, the evenings between around 1700 and 2000 and the approximate sleeping hours, the weekends record a larger number of steps than weekdays on average.