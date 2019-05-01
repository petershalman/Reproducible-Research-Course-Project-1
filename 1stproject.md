## introduction 
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

## Loading and preprocessing the data:

```{r}
activity<- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
head(activity)
```

## What is mean total number of steps taken per day?

```{r}
library ("dplyr")
```
```{r fig.width=12, fig.height=10}
tmSteps <- activity %>% group_by(date) %>% summarize(total.steps = sum(steps, na.rm = T), mean.steps = mean(steps, na.rm = T))
head(tmSteps)
```
```{r}
library("ggplot2")
```
```{r fig.width=12, fig.height=10}
g <- ggplot(tmSteps, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + labs(y = "Frequency") + labs(x = "Total steps/day")
```

For Calculating the median we use the following formula:
        
```{r}
summary(tmSteps$total.steps)
summary(tmSteps$mean.steps)
```
```{r}
aveSteps <- activity %>% group_by(interval) %>% summarize(mean.steps = mean(steps, na.rm = T))
g<- ggplot(aveSteps, aes(x = interval, y = mean.steps))
g + geom_line() + 
        labs(y = "Mean number of steps") + labs(x = "Interval")
```
The largest amount of steps occurs between time intervals 500 and 1000 and The maximum average number of steps is: 206 and occurs in time interval 835

## Imputing missing values

```{r}
sum(is.na(activity$steps))
```

For filling all of the missig values, we can use the mean of that day or the mean of the mean for that 5-minute interval.
But before we making decision which method we should use, we have to calculate the percentage of missing value of both method and choose the methos which has got the
lower missing value.

```{r}
mean(is.na(activity$steps))
```
As it can be seen, we have got 13% missing data. NOw we test the one.

```{r}
mean(is.na(aveSteps$mean.steps))
```

So based on the above calculation, we choose the right data from aveSteps dataset.

```{r}
newActivity<-activity
for (i in 1:nrow(newActivity)) {
        if (is.na(newActivity$steps[i])) {
                intIndex <- newActivity$interval[i]
                nmValue <- subset(aveSteps, interval==intIndex)
                newActivity$steps[i] <- nmValue$mean.steps
        }
}
head(newActivity)
newTM <- newActivity %>% group_by(date) %>% summarize(total.steps = sum(steps, na.rm = T))
g <- ggplot(newTM, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) +labs(y = "Frequency") + labs(x = "Total steps/day")
```
The new graph shows that it is similar to the first graph but the first columnd at left has been removed. It shows that filling values with the interval means has increased the frequencies around median.

```{r}
summary (tmSteps$total.steps)
sd(tmSteps$total.steps, na.rm=T)
summary (newTM$total.steps)
sd(newTM$total.steps, na.rm=T)
```

The mean and the median numbers are almost the same. The Standard Deviation of new data is smaller . It means by filling NAs with the mean values, the distribution has been closed around the center. Meanwhile, as we can see the new data min value has increased.

## Are there differences in activity patterns between weekdays and weekends?

```{r}
newActivity$date<-as.Date(newActivity$date)
newActivity$day <- ifelse(weekdays(newActivity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
wkend <- filter(newActivity, day == "weekend")
wkday <- filter(newActivity, day == "weekday")
wkend <- wkend %>%  group_by(interval) %>% summarize(mean.steps = mean(steps))
wkend$day <- "weekend"
wkday <- wkday %>%  group_by(interval) %>% summarize(mean.steps = mean(steps))
wkday$day <- "weekday"
newInt <- rbind(wkend, wkday)
newInt$day <- as.factor(newInt$day)
head(newInt)
g <- ggplot (newInt, aes (interval, mean.steps))
g + geom_line() + facet_grid (day~.) + labs(y = "Number of Steps") + labs(x = "Interval")
```
