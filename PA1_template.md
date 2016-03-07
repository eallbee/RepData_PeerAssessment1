# Reproducible Research: Peer Assessment 1

library(ggplot2)

## Loading and preprocessing the data
activityData <- read.csv("activity.csv")


## What is mean total number of steps taken per day?
sumAllSteps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
qplot(sumAllSteps, xlab="steps per day")


mean(sumAllSteps, na.rm=TRUE)
median(sumAllSteps, na.rm=TRUE)


## What is the average daily activity pattern?
averageDailyActivity <- aggregate(x=list(steps=activityData$steps),by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)

ggplot(data=averageDailyActivity, aes(x=interval, y=steps)) +
    geom_line()
    
averageDailyActivity[which.max(averageDailyActivity$steps),]    

## Imputing missing values

stepsNA <- is.na(activityData$steps)
# How many missing
table(stepsNA)

imputedValue <- function(steps, interval) {
    imputed <- NA
    if (!is.na(steps))
        imputed <- c(steps)
    else
        imputed <- (averageDailyActivity[averageDailyActivity$interval==interval, "steps"])
    return(imputed)
}
imputedActivityData <- activityData

imputedActivityData$steps <- mapply(imputedValue, imputedActivityData$steps, imputedActivityData$interval)


sumAllSteps <- tapply(imputedActivityData$steps, imputedActivityData$date, FUN=sum)
qplot(sumAllSteps, binwidth=1000, xlab="steps per day")

mean(sumAllSteps)
median(sumAllSteps)



## Are there differences in activity patterns between weekdays and weekends?
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
imputedActivityData$daytype <- as.factor(sapply(imputedActivityData$date, daytype))

averages <- aggregate(steps ~ interval + daytype, data=imputedActivityData, mean)

ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .) +
    xlab("interval") + ylab("steps")

