if (!(require(ggplot2, quietly=T))) {
    install.packages('ggplot2')
}
library(ggplot2)
## Loading and preprocessing the data

## Show any code that is needed to load the data (i.e. read.csv())
dir.create("./data", showWarnings=FALSE) 

if(!file.exists("./data/household_power_consumption.txt")) {
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, destfile = "./data/data.zip")
    unzip(zipfile="./data/data.zip", files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = "./data", unzip = "internal",
          setTimes = FALSE)
}

activity.data <- read.csv("./data/activity.csv", 
                          header = TRUE , 
                          stringsAsFactors = FALSE)

## Process/transform the data (if necessary) into a format 
## suitable for your analysis

activity.data$steps <- as.numeric(activity.data$steps)

## 
# No Longer need the commented code below, but it's nice to have around.
# startTime <- strptime("2012-10-01 0:0:0", "%Y-%m-%d %H:%M")
# activity.data$time <- seq.POSIXt(startTime, by = "5 mins", length.out = 17568)
# activity.data$time <- format(activity.data$time, format = "%H:%M")
##
activity.data$index <- rep(1:288, times = 61)

##
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values 
# in the dataset.
#
# Make a histogram of the total number of steps taken each day
##

##Find total steps per day
steps.per.day <- aggregate(steps ~ date, 
                           data = activity.data,
                           sum)

## Find optimal bin-width of histogram using Scott's normal refernce rule
h <- (3.5 * (sd(steps.per.day$steps))) / (nrow(steps.per.day))^(1/3)

## Plot histogram
hist <- ggplot(steps.per.day, aes(x=steps)) +
    geom_histogram(binwidth = h, colour="black", fill="white") +
    xlab("Steps Per Day") + 
    ylab("Frequency (Days)") +
    ggtitle("Total Number of Steps Per Day")
hist

## Calculate and report the mean and median total number of steps taken per day

## Mean:
mean.steps <- mean(steps.per.day$steps, na.rm = TRUE) # 10766.19

## Median
median.steps <- median(steps.per.day$steps, na.rm = TRUE) # 10765

## What is the average daily activity pattern?
mean.time <- aggregate(steps ~ index 
                       data = activity.data,
                       mean)

##
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
##

g <- ggplot(mean.time, aes(x = index, y = steps, group = 1)) + 
    geom_line(size = 1) + 
    xlab("Time (5 minute intervals)") +
    ylab("Mean Number of Steps") +
    ggtitle("Mean Number of Steps Taken At A Given Time Each Day") +
    scale_x_discrete(breaks=c(1, 49, 97, 145, 193,  245, 288), 
                     labels=c("12 Midnight", "4:00 A.M.", "8:00 A.M.", "12 Noon", 
                              "4:00 P.M.", "8:00 P.M.", "11:55 P.M."))
g

##
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
##

mean.time <- cbind(mean.time, activity.data$interval[1:288])
names(mean.time) <- c("index", "steps", "interval")

max.interval <- mean.time[which.max(mean.time$steps),]$interval
max.interval # [1] 835

## Imputing missing values
#
# Note that there are a number of days/intervals where there are missing 
# values (coded as NA). The presence of missing days may introduce bias into 
# some calculations or summaries of the data.
#
#
# Calculate and report the total number of missing values in the 
# dataset (i.e. the total number of rows with NAs)
##

count.NA <- sum(is.na(activity.data))
count.NA # [1] 2304

##
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.
##

## We are going to replace the NAs with the 5-minute interval mean
merged.data <- merge(activity.data, mean.time, by = 'index')
for (i in 1:nrow(merged.data)){
    if (is.na(merged.data$steps.x[i])) {
        merged.data$steps.x[i] <- merged.data$steps.y
    }
}


##
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
##
activity.data.filled <- merged.data[, c(2, 3, 4, 1)]
names(activity.data.filled) <- c("steps", "date", "interval", "index")

##  
# Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?
##

full.steps <- aggregate(steps ~ date, 
                        data = activity.data.filled,
                        sum)

## Find optimal bin-width of histogram (again) using Scott's normal refernce rule
h <- (3.5 * (sd(full.steps$steps))) / ((nrow(full.steps))^(1/3))

## Plot histogram
hist2 <- ggplot(full.steps, aes(x=steps)) +
    geom_histogram(binwidth = h, colour="black", fill="white") +
    xlab("Steps Per Day") + 
    ylab("Frequency (Days)") +
    ggtitle("Total Number of Steps Per Day")
hist2

## Mean:
mean.full <- mean(full.steps$steps) # [1] 9419.08
mean.difference <- ((mean.steps - mean.full) / mean.full) * 100
# 14.3% difference

## Median
median.full <- median(full.steps$steps) # 10395
median.difference <- ((median.steps - median.full) / median.full) * 100 
# 3.5% difference

# Conclusion: Not a substantial difference

##
# Are there differences in activity patterns between weekdays and weekends?
#
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.
#
# Create a new factor variable in the dataset with two levels - "weekday" 
# and "weekend" indicating whether a given date is a weekday or weekend day.
##

activity.data.filled$date <- as.POSIXct(activity.data.filled$date)
activity.data.filled$day <- weekdays(activity.data.filled$date)
activity.data.filled$day <- ifelse(activity.data.filled$day == c("Saturday", "Sunday"), 
                                   "Weekend", "Weekday")
activity.data.filled$day <- as.factor(activity.data.filled$day)

full.steps <- aggregate(steps ~ index + day, activity.data.filled, mean)

## Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what this 
## plot should look like using simulated data.
panel <- ggplot(full.steps, aes(x = index, y = steps, group = 1)) + 
    facet_grid(. ~ day) +
    geom_line(size = 1) + 
    xlab("Time (5 minute intervals)") +
    ylab("Mean Number of Steps") +
    ggtitle("Average Number of Steps by Weekday and Weekend") +
    scale_x_discrete(breaks=c(1, 97, 193, 288), 
                     labels=c("0:00", "12", 
                              "8:00 P.M.", "11:55 P.M."))