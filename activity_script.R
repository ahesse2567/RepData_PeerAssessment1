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

max(stepsByInterval$meanSteps)
which(stepsByInterval$meanSteps == max(stepsByInterval$meanSteps))
stepsByInterval[104,]

ggplot(stepsByInterval, aes(x=interval, y=medianSteps)) +
    geom_line() +
    ggtitle("Median Steps Per Day for Each 5-minute Inverval")+
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Interval") +
    ylab("Median Steps During Inverval")


# mean and median steps overlay
ggplot(stepsByInterval) +
    geom_line(aes(x=interval, y=meanSteps, color = "red")) +
    geom_line(aes(x=interval, y=medianSteps, color = "blue")) +
    ggtitle("Mean and Median Steps Per Day for Each 5-minute Inverval") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Interval") +
    ylab("Steps During Inverval")

ggplot(stepsByInterval) +
    geom_line(aes(x=interval, y=meanSteps, color = "blue")) +
    geom_line(aes(x=interval, y=medianSteps, color = "red")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Mean and Median Steps Per Day for Each 5-minute Inverval",
         x = "Interval", y = "Steps During Interval", color = "Measure") +
    scale_color_manual(labels = c("Mean", "Median"), values = c("red", "blue"))

sum(stepsByInterval$meanSteps)
sum(stepsByInterval$medianSteps)


activity.imputed <- read_csv("activity.csv")

for(i in 1:nrow(activity.imputed)) {
    if(is.na(activity.imputed[i,1])) {
        interval <- activity.imputed[[3]][[i]]
        row <- which(stepsByInterval[[1]] == interval)
        activity.imputed[i,1] <- stepsByInterval[row,2]
    }
}

imputedStepsPerDay <- tapply(activity.imputed$steps, activity.imputed$date, sum)
imputedStepsPerDay <- as.data.frame(imputedStepsPerDay)
head(imputedStepsPerDay)

ggplot(imputedStepsPerDay) +
    geom_histogram(aes(x = imputedStepsPerDay)) +
    labs(title = "Imputed Steps Per Day", x = "Steps", y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5))

mean(imputedStepsPerDay$imputedStepsPerDay)
median(imputedStepsPerDay$imputedStepsPerDay)

# total number of missing values

activity <- read_csv("activity.csv")
sum(apply(activity, MARGIN = 1, anyNA))

activity.complete <- activity %>% 
    mutate(interval = as.integer(interval)) %>% 
    mutate(steps = as.integer(steps)) %>% 
    filter(!is.na(steps)) %>% # completely removing rows with NA will help for later calculations of means and medians
    glimpse()



# weekend vs. weekday activity



library(chron) # for the is.weekend function

activity.complete <- activity.complete %>% 
    mutate(day = weekdays(date)) %>% 
    mutate(weekend = is.weekend(date)) %>% # TRUE indicates weekend
    glimpse()

weekday_vs_weekend_steps <- activity.complete %>% 
    group_by(weekend, interval) %>% 
    summarize(meanSteps = mean(steps), medianSteps = median(steps))

ggplot(weekday_vs_weekend_steps) +
    geom_line(aes(x = interval, y = meanSteps, color = weekend)) +
    facet_grid(weekday_vs_weekend_steps$weekend) +
    ggtitle("Average Steps Per Day for Each 5-minute Inverval")+
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Interval") +
    ylab("Average Steps During Inverval")
