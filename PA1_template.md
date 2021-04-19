---
title: "Reproducible Research: Peer Assessment 1"
author: "Darwin R. Nava F."
date: "19 de abril de 2021"
output: html_document
---
  
'''{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
'''


## Loading and preprocessing the data  
'''{r}  
library(dplyr)
steps <- read.csv("./activity.csv", sep=",", header=TRUE)  
steps <- na.omit(steps)  
steps$date <- as.Date(steps$date)  
by_day <- group_by(steps, date)  
steps_daily <- summarize(by_day, steps=sum(steps))  
'''

## What is mean total number of steps taken per day?  
```{r}  
par(mar=c(4,4,2,1))  
hist(steps_daily$steps, breaks=30, xlab="Number of steps", ylab = "Frequency", main = "Distribution. Number of steps daily", col="yellow", xlim=c(0, 25000))  
abline(v= mean(steps_daily$steps, na.rm = TRUE), lwd=2, lty=4, col="red")  
abline(v= median(steps_daily$steps, na.rm = TRUE), lwd=2,lty=2,col="blue")  
m <- paste("Mean:",mean(steps_daily$steps, na.rm = TRUE))  
md <- paste("Median:",median(steps_daily$steps, na.rm = TRUE))  
legend("topright", cex=0.7, pch=c(16,16),col=c("red", "blue"), legend=c(m, md))  
print(paste(m, " ", md))  
```  

## What is the average daily activity pattern?  
```{r}  
library(dplyr) 
by_interval <- group_by(steps, interval)  
steps_interval <- (summarize(by_interval, steps=mean(steps)))  
plot(steps_interval$interval, steps_interval$steps, type = "l", xlab = "5-minute interval", ylab = "The average number of steps taken", main = "The average daily activity pattern", col="Yellow")  
abline(v= 835, lwd=1,lty=2,col="blue")  
abline(h= max(steps_interval$steps, na.rm = TRUE), lwd=1,lty=2,col="red")  
legend("topright", cex=0.7, pch=c(16,16),col=c("red", "blue"), legend=c("Y-Steps Max: 206.16", "X-Interval:835"))  
print("The 5-minute interval which on average across all the days in the dataset, contains the maximum number of steps:")  
steps_interval[steps_interval$steps==max(steps_interval$steps),]  
```  

## Imputing missing values  
```{r}  
raw_data <- read.csv("./activity.csv", sep=",", header=TRUE)  
print("The total number of rows with Nas")  
sum(is.na(raw_data))  

library(impute)  
raw_data$date <- as.numeric(as.Date(raw_data$date))  
raw_data_imputed <-  as.data.frame(impute.knn(as.matrix(raw_data))$data)
raw_data_imputed$date <- as.Date(raw_data_imputed$date, origin = "1970-01-01")  
by_day_2 <- group_by(raw_data_imputed, date)  
steps_daily_2 <- summarize(by_day_2, steps=sum(steps))  

par(mar=c(4,4,2,1))  
hist(steps_daily_2$steps, breaks=30, xlab="Number of steps", ylab = "Frequency", main = "Distribution. Number of steps daily", col="yellow", xlim=c(0, 25000))  
abline(v= mean(steps_daily_2$steps, na.rm = TRUE), lwd=2, lty=4, col="red")  
abline(v= median(steps_daily_2$steps, na.rm = TRUE), lwd=2,lty=2,col="blue")  
m2 <- paste("Mean:",mean(steps_daily_2$steps, na.rm = TRUE))  
md2 <- paste("Median:",median(steps_daily_2$steps, na.rm = TRUE))  
legend("topright", cex=0.7, pch=c(16,16),col=c("red", "blue"), legend=c(m2, md2))  

print(paste("before imputing: ",m, " ", md))  
print(paste("after imputing: ",m2, " ", md2))  
```  

## Are there differences in activity patterns between weekdays and weekends?  
```{r}  
steps2 <- raw_data_imputed  
library(lubridate) 
library(ggplot2)
steps2$weektype <- ifelse(wday(steps2$date, label = TRUE) %in% c("lu\\.","ma\\.","mi\\.","ju\\.","vi\\." ),"Weekdays","Weekends")  
by_interval_weektype <- group_by(steps2, interval, weektype)  
steps_interval_weektype <- (summarize(by_interval_weektype, steps=mean(steps), weektype=weektype))   

qplot(interval, steps,data=steps_interval_weektype, facets=weektype~.,geom="line" , xlab = "5-minute interval", ylab = "The average number of steps taken", main = "The average daily activity pattern")  
``` 

*It is observed:*  
  *Max Weekdays: x= 835 y= 207. Mean= 33.36489*  
  *Max Weekends: x= 915 y= 153. Mean= 40.02226*  
  *More activity during the weekends in contrast to the weekdays.*  
  
  *end*