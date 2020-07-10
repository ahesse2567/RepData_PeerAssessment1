library(tidyverse)

activity <- read_csv("activity.csv")

activity <- activity %>% 
    mutate(interval = as.integer(interval)) %>% 
    mutate(steps = as.integer(steps)) %>% 
    filter(!is.na(steps)) %>% # completely removing rows with NA will help for later calculations of means and medians
    glimpse()

activity.complete <- activity

# Get average steps per day

activityByDay <- split(activity, activity$date)

stepsPerDay <- sapply(activityByDay, function(x) sum(x[[1]], na.rm = TRUE))
stepsPerDay <- as.data.frame(stepsPerDay)
stepsPerDay


# histogram of steps per day
ggplot(stepsPerDay, aes(x = stepsPerDay)) +
    geom_histogram(bins = 25) +
    ggtitle("Steps Per Day") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Steps Per Day") +
    ylab("Count")

# calculate mean and median steps per day

mean(stepsPerDay$stepsPerDay)
median(stepsPerDay$stepsPerDay)

# daily activity patterns

stepsByInterval <- activity %>% 
    group_by(interval) %>% 
    summarize(meanSteps = mean(steps), medianSteps = median(steps))
head(stepsByInterval)

ggplot(stepsByInterval, aes(x=interval, y=meanSteps)) +
    geom_line() +
    ggtitle("Average Steps Per Day for Each 5-minute Inverval")+
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Interval") +
    ylab("Average Steps During Inverval")

max(stepsByInterval$steps)
which(stepsByInterval$steps == max(stepsByInterval$steps))
stepsByInterval[104,]


ggplot(stepsByInterval, aes(x=interval, y=medianSteps)) +
    geom_line() +
    ggtitle("Median Steps Per Day for Each 5-minute Inverval")+
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Interval") +
    ylab("Median Steps During Inverval")



ggplot(stepsByInterval) +
    geom_line(aes(x=interval, y=meanSteps, color = "blue")) +
    geom_line(aes(x=interval, y=medianSteps, color = "red")) +
    labs("")
    ggtitle("Mean and Median Steps Per Day for Each 5-minute Inverval")+
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Interval") +
    ylab("Steps During Inverval")



# total number of missing values

activity <- read_csv("activity.csv")
sum(apply(activity, MARGIN = 1, anyNA))
