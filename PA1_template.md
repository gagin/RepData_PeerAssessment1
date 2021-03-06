# Reproducible Research: Peer Assessment 1

This is an exercise in using literate programming tools in R - original dataset
has number of steps taken during 5-minute intervals over many days. We are to
ask and answer some questions about this data. 

## Loading and preprocessing the data

Main text of the document will
show research points, while code and technical data handling specifics should
stay in the code chunks, which will have to be commented on their own.


```r
## The data file is supposed to be downloaded prior to running the code,
## as conditioned by this exercise.

# Directory is custom for your computer, set it correctly.
setwd("~/Coursera-R/RepData_PeerAssessment1/")

# Original Github branch has the data zipped, let's extract it.
if(!file.exists("activity.csv")) unzip("activity.zip")

# read the file
data<-read.csv("activity.csv")

# Later we will need to have intervals as actual time for charts,
# so let us make them uniform to ease strptime conversion.
data$interval<-sapply(data$interval,
                      function(i){
                              i.length<-nchar(as.character(i))
                              if(i.length<4){
                                prefix<-paste(rep(0,4-i.length),collapse="")
                                paste0(prefix,i)}
                              else i
                              }
                      )
data$date<-as.Date(data$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
sum<-tapply(data$steps,
            data$date,
            sum,
            na.rm=TRUE)
hist(sum,
     xlab="Daily steps taken",
     main="",
     breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
summary(sum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```


Mean is 9354,
median is 10400. This person apparently
knows a saying "10000 Steps a Day to Keep the Doctor Away" as they seem
to target this amount of steps.         

## What is the average daily activity pattern?



```r
pattern<-tapply(data$steps,
                data$interval,
                mean,
                na.rm=TRUE)

# Convert interval to times to make x labels more meaningful

plot(strptime(names(pattern),"%H%M"),
     pattern/5,
     type="l",
     xlab="Time of day",  
     ylab="Average steps per minute")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
which.max(pattern)
```

```
## 0835 
##  104
```

The most step usually happen in five minutes that start on
08:35.


## Imputing missing values

How many missing values there's in the dataset?

```r
sum(is.na(data))
```

```
## [1] 2304
```

Let's replace NAs with average value for the interval and make a new dataset.


```r
d<-data
d$steps<-sapply(seq_along(d$steps),
                function(n){
                  if(is.na(d$steps[n])){
                          pattern[names(pattern)==d$interval[n]]
                  } else d$steps[n]
                  }
                  )
```



Now, if we rebuild the histogram, we should expect number of steps,
as well as median and mean, to go up.


```r
sum1<-tapply(d$steps,
            d$date,
            sum,
            na.rm=TRUE)
hist(sum1,
     xlab="Daily steps taken (with NAs replaced)",
     main="",
     breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
summary(sum1)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

We don't have a peak column at left side anymore, the distribution looks more
reasonable now, although mean and median still close enough to their previous
levels.

## Are there differences in activity patterns between weekdays and weekends?


```r
# Let's load libraries for this one silently with message=FALSE.
library(dplyr)
library(lattice)
```


```r
# First, add a new column that will indicate if it's a weekend or not.  
# Then juxtapose weekday and weekend data to see how pattern is changed.  
weekpart<-function(dt){
        if(weekdays(dt) %in% c("Saturday","Sunday")) "weekend" else "weekday"}
weekpart<-sapply(d$date,weekpart)
d<-cbind(d,weekpart)

# summarise step means for each 5 minutes interval separately for each weekpart
patternw<-d %>% group_by(interval,weekpart) %>% summarise(steps=mean(steps))

# vertical placement of charts allows to see per-hour comparison more clearly
with(patternw,
            xyplot(steps/5 ~ as.numeric(interval) | weekpart,
                   layout=1:2,
                   xlab="Hour of the day",
                   ylab="Number of steps per minute",
                   type="l",
                   scales=list(
                           # Instead of numeric, draw interval as time again,
                           # but in a simpler way this time - as xyplot()
                           # has some issues with POSIXlt
                           x=list(
                                   at=seq(0,2400,200),
                                   labels=as.character(seq(0,24,2))
                                   )
                           )
                   )
            )
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Morning walking peak is more pronounced on weekdays, so there's some weekday
routine. Average weekend's morning peak is not as tall, and doesn't dominate
as much over the following day. The person doesn't sleep in much on weekends
though.
