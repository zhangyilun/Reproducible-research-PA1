setwd("F:/Documents/University/3A 2014 WT4 Fall/coursera/Data Science 5 - Reproducible Research/Peer assessment 1")

require(ggplot2)

# load data
data <- read.csv("activity.csv")

# mean total number of steps taken per day
# 1. histogram of the total number of steps taken per day
x <- tapply(data$steps,data$date,sum,na.rm=T)
hist(x)
# 2. mean
mean(x)
# 3. median
median(x)

# what is the average daily activity pattern
y <- tapply(data$steps,data$interval,mean,na.rm=T)
plot(y,type="l")

# imput missing data
# count number of missings
sum(is.na(data$steps))
# imput with mean of that day
full <- data
for (day in unique(full$date)) {
  sub <- full[full$date==day,]
  if (nrow(sub[is.na(sub$steps)==T,]) > 0){
    m <- mean(sub$steps,na.rm=T)
    if (is.nan(m)) {
      m <- 0
    }
    full[full$date==day & is.na(full$steps)==T,]$steps <- m
  }
}
# new data frame is "full"
# histogram of new data
z <- tapply(full$steps,full$date,sum,na.rm=T)
hist(z)
# mean
mean(z)
# median
median(z)

# weekday and weekend difference
full$date <- as.Date(full$date)
# create new factor variable
full$weekday <- weekdays(full$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")
full[full$weekday == TRUE,]$weekday <- "weekday"
full[full$weekday == FALSE,]$weekday <- "weekend"
full$weekday <- as.factor(full$weekday)

# plot
ggplot(full,aes(x=interval,y=steps)) +
  geom_line() +
  facet_wrap(~ weekday,scale="free") +
  labs(y="Number of steps",x="Interval") +
  ggtitle("Difference between weekdays and weekends\n") +
  theme(plot.title = element_text(lineheight=1,face="bold",
                                  color="black",size=20))


