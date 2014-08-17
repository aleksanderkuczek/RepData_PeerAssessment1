# Reproducible Research: Peer Assessment 1
by Aleksander Kuczek

Code styling in accordance with Google's R Style Guide
https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
```{r setoptions,echo=FALSE}
library(knitr)
library(data.table)
opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
I'm ignoring the missing values in the dataset.
```{r}
unzip("activity.zip")
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data <- transform(data, date = as.Date(date))
dataTable <- data.table(data)
dataTableNoNAs <- dataTable[complete.cases(dataTable), ]
```

## What is mean total number of steps taken per day?

```{r}
totalData <- dataTableNoNAs[, list(sum=sum(steps), 
                             mean=mean(steps), 
                             median=as.integer(median(steps))), 
                       by=date]

```
1. Make a histogram of the total number of steps taken each day
```{r}
hist(totalData$sum,
     main = "Total number od steps taken each day",
     xlab = "Total number od steps"
     )
```
2. Calculate and report the mean and median total number of steps taken per day
```{r}
totalData[,list(date,mean,median)]
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
averageDaliyPattern <- dataTableNoNAs[, list(mean=mean(steps)), 
                       by=interval]
plot(averageDaliyPattern$interval, 
     averageDaliyPattern$mean, 
     type = "l",
     main = "Average number of steps taken in 5-minute intervals across all 
     days",
     xlab = "5-minute interval",
     ylab = "average number of steps taken")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
averageDaliyPattern[which.max(averageDaliyPattern$mean), ]
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
nrow(dataTable[!complete.cases(dataTable), ])
```
2. Filling in all of the missing values in the dataset with the mean of that 5-minute interval across all days.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
dataTableFilledNAs <- merge(x = dataTable, y = averageDaliyPattern, 
                            by= "interval", 
                            all = TRUE)
dataTableFilledNAs[,steps := ifelse(is.na(steps), mean, steps)]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
totalDataFilledNAs <- dataTableFilledNAs[, list(sum=sum(steps), 
                             mean=mean(steps), 
                             median=as.integer(median(steps))), 
                       by=date]
par(mfrow = c(1, 2))
hist(totalData$sum,
     main = "No NA's",
     xlab = "Total number od steps"
     )
hist(totalDataFilledNAs$sum,
     main = "Filled NA's",
     xlab = "Total number od steps"
     )
totalDataFilledNAs[,list(date,mean,median)]
```
As you can see the values after filling NA's differs from the estimates from the first part of the assignment.
```{r}
summary(totalData)
summary(totalDataFilledNAs)

```
Imputing missing data on the estimates of the total daily number of steps change the number of steps form `r sum(totalData$sum)` to `r format(sum(totalDataFilledNAs$sum), scientific = FALSE)`.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
dataTableFilledNAs$dayType <- ifelse(weekdays(dataTable$date) == "Sunday" | weekdays(dataTable$date) == "Saturday" ,"Weekend", "Weekday")

```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
library("lattice")
averageDaliyPatternDayType <- dataTableFilledNAs[, list(mean=mean(steps)), 
                       by= c("interval", "dayType")]
xyplot(mean~interval|dayType,
       data = averageDaliyPatternDayType,
       type = "l", 
       layout = c(1,2),
       ylab = "Number of steps (mean)",
       xlab = "5-minute interval")
```