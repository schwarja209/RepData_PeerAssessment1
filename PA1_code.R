#Loading and preprocessing data

library(plyr)
library(dplyr)
library(lattice)

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","data.zip")
unzip("data.zip")

data<-read.csv("activity.csv")

data$date<-as.Date(data$date)
dataClean<-data[!is.na(data$steps),]


#What is mean total number of steps taken per day?

totalSteps1<-select(dataClean,date,steps)%>%
    group_by(date)%>%
    summarize(steps=sum(steps))

with(totalSteps1,hist(steps,breaks=20,col="lightblue",
                     main="Total Number of Steps per Day", 
                     xlab="Cumulative Steps"))

totalSteps1_mean<-mean(totalSteps1$steps)
totalSteps1_median<-median(totalSteps1$steps)


#What is the average daily activity pattern?

averageSteps1<-select(dataClean,interval,steps)%>%
    group_by(interval)%>%
    summarize(steps=mean(steps))

with(averageSteps1,plot(interval,steps,type="l",xlab="TIme Interval",ylab="Avg. Steps",
     main="Time Series Plot of Avg Steps in 5-minute Intervals"))

averageStepsMax<-averageSteps1[which.max(averageSteps1$steps),]


#Imputing missing values

dataNAs<-is.na(data)
dataNAs_sum<-sum(dataNAs)

stepsFill<-function(steps,interval){
    filled<-NA
    if(!is.na(steps)) filled<-steps
    else filled<-(averageSteps1[averageSteps1$interval==interval,"steps"])
    return(filled)
}
dataFilled<-mutate(data,steps=as.numeric(mapply(FUN=stepsFill,steps,interval)))

totalSteps2<-select(dataFilled,date,steps)%>%
    group_by(date)%>%
    summarize(steps=sum(steps))

with(totalSteps2,hist(steps,breaks=20,col="lightblue",
                     main="Total Number of Steps per Day", 
                     xlab="Cumulative Total Number of Steps"))

totalSteps2_mean<-mean(totalSteps2$steps)
totalSteps2_median<-median(totalSteps2$steps)


#Are there differences in activity patterns between weekdays and weekends?

findDateType<-function(date){
    if (date %in% c("Saturday", "Sunday"))
        return("Weekend")
    else return("Weekday")
}
dataFilled_expansion<-mutate(dataFilled,dateType=weekdays(date))%>%
    mutate(dateType=as.factor(sapply(dateType,FUN=findDateType)))

averageSteps2<-select(dataFilled_expansion,dateType,interval,steps)%>%
    group_by(dateType,interval)%>%
    summarize(steps=mean(steps))

xyplot(steps~interval|dateType,data=averageSteps2,type="l",layout=c(1,2),
       ylab="Avg. Steps",xlab="Time Interval",main="Time Series Plot of Avg Steps in 5-minute Intervals")