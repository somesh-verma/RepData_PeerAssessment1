Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
"quantified self" movement - a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring
data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values
    are coded as NA)
-   date: The date on which the measurement was taken in YYYY-MM-DD
    format
-   interval: Identifier for the 5-minute interval in which measurement
    was taken

### Data Load

Load the data using read.csv/tbl\_df and for understanding of data, lets
print the o/p of summary

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lattice)

    activityData <- tbl_df(read.csv(file = "activity.csv", na.strings = "NA"))
    summary(activityData)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

### What is mean total number of steps taken per day?

    activityDataDate <- activityData %>%
        group_by(date) %>%
          mutate(totalSteps=sum(steps, na.rm=T))
    hist(activityDataDate$totalSteps, col="blue", 
        main="Number of Steps Taken Per Day", xlab="Number of Steps", 
        ylab="Count/Frequency", breaks=10)

![](PA1_template_files/figure-markdown_strict/numberOfSteps-1.png)

#### Mean and Median of the Daily Step Count

    sprintf("Mean of Total Steps: %f Median of Total Steps: %f", mean(activityDataDate$totalSteps), median(activityDataDate$totalSteps))

    ## [1] "Mean of Total Steps: 9354.229508 Median of Total Steps: 10395.000000"

### What is the average daily activity pattern?

    activityPattern <- activityData %>%
        group_by(interval) %>%
          mutate(meanSteps=mean(steps, na.rm=T))
    plot(activityPattern$interval, activityPattern$meanSteps, type="l", col="blue", xlab = "Interval", ylab = "Mean Step Count")

![](PA1_template_files/figure-markdown_strict/activityPattern-1.png)

    maximumInterval<- which.max(activityPattern$meanSteps)
    activityPattern[maximumInterval, ]

    ## Source: local data frame [1 x 4]
    ## Groups: interval [1]
    ## 
    ##   steps       date interval meanSteps
    ##   <int>     <fctr>    <int>     <dbl>
    ## 1    NA 2012-10-01      835  206.1698

### Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    sprintf("Number of Rows with NA: %d", sum(is.na(activityData)))

    ## [1] "Number of Rows with NA: 2304"

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    activityDataImput <- activityData %>%
        group_by(interval) %>%
        mutate(steps=ifelse(is.na(steps), mean(steps, na.rm=T), steps))

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    activityDataDate <- activityData %>%
        group_by(date) %>%
          mutate(totalSteps=sum(steps, na.rm=T))
    hist(activityDataDate$totalSteps, col="blue", 
        main="Number of Steps Taken Per Day", xlab="Number of Steps", 
        ylab="Count/Frequency", breaks=10)

![](PA1_template_files/figure-markdown_strict/numberOfSteps2-1.png)

### Are there differences in activity patterns between weekdays and weekends?

    activityDataWeekdays <- activityData %>%
        mutate(weekdays=ifelse((weekdays(as.Date(as.character(date)),T) == "Sun") | (weekdays(as.Date(as.character(date)), T) == "Sat"),"Weekends","Weekdays"))

    activityDataWeekdaysSummary <- activityDataWeekdays %>%
      group_by(weekdays, interval)  %>% 
      summarize(totalSteps=sum(steps, na.rm=TRUE), averageSteps=mean(steps, na.rm=TRUE))


    xyplot(averageSteps~interval|weekdays, data=activityDataWeekdaysSummary, type='l', layout=(c(1,2)),
       main="Average Daily Activity Pattern by Weekday/Weekend", col="blue", 
       ylab="Average No. of Steps Taken per 5 min Interval", xlab="5 min Time Interval") 

![](PA1_template_files/figure-markdown_strict/ActivityPatternWeekDaysWeekEnds-1.png)
