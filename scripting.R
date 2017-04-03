## libs
library(dplyr)
library(lattice)

## preprocessing
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              "activity.zip")
unzip("activity.zip")
my_data <- read.csv("activity.csv")
my_data$date <- as.Date(my_data$date)
clean_data <- my_data[!is.na(my_data$steps), ]

## mean analysis
steps_per_day <- summarize(group_by(clean_data, date), daily_steps = sum(steps))
hist(steps_per_day$daily_steps, 
     main = "Histogram of total number of steps taken each day", 
     xlab = "Steps per day")
mean(steps_per_day$daily_steps)
median(steps_per_day$daily_steps)

## average daily activity pattern
avg_steps_per_interval <- summarize( 
  group_by(clean_data, interval), avg_steps = mean(steps))
plot(avg_steps_per_interval$interval, avg_steps_per_interval$avg_steps, 
     type = "l", 
     main = "Average number of steps per 5-minute interval", 
     xlab = "Number of 5-minute interval", 
     ylab = "Average number of steps across all days")
avg_steps_per_interval[which.max(avg_steps_per_interval$avg_steps),]

## imputing missing values
count(my_data[is.na(my_data$steps),])
dt <- as.data.table(my_data)
dt$steps <- as.numeric(dt$steps)
dt <- dt %>% 
  group_by(interval) %>% 
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

steps_per_day_imp <- summarize(group_by(dt, date), daily_steps = sum(steps))
hist(steps_per_day_imp$daily_steps, 
     main = "Histogram of total number of steps taken each day",
     sub = "NAs imputed by mean of 5-minute interval",
     xlab = "Steps per day")
mean(steps_per_day_imp$daily_steps)
median(steps_per_day_imp$daily_steps)

## weekdays vs. weekends
dt <- dt %>%  
  mutate(day_indicator = 
           ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday", 
                  "weekend", 
                  "weekday"))
avg_steps_per_daytype <- summarize(group_by(dt, interval, day_indicator), avg = mean(steps))
attach(avg_steps_per_daytype)
xyplot(avg~interval|day_indicator, 
       type = "l", 
       layout=c(1,2), 
       main = "Average steps per interval and per type of day",
       xlab = "Number of 5-minute interval",
       ylab = "Average number of steps")