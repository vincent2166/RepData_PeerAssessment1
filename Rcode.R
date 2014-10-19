setwd("~/datasciencecoursera/RepData_PeerAssessment1")

#read data and process date to date class
data<-read.csv('activity.csv')
data$date<-as.Date(data$date, format="%Y-%m-%d")

#create a clean without null
data.p1<-data[!is.na(data$steps),c(1,2)]

#calculate the sum of each day
data.p1<-tapply(data.p1$steps, data.p1$date, sum)
data.p1<-as.data.frame(data.p1)
data.p1$date<-as.Date(rownames(data.p1))
colnames(data.p1)<-c('sum', 'date')

#create graph
library(ggplot2)
ggplot(data.p1, aes(x=date,y=sum)) + geom_histogram(stat='identity')

#calculate the mean and median
mean(data.p1$sum)
median(data.p1$sum)

#calculate the mean of each interval
data.p2<-data[!is.na(data$steps),c(1,3)]
data.p2<-tapply(data.p2$steps, data.p2$interval, mean)
data.p2<-as.data.frame(data.p2)
data.p2$date<-as.integer(rownames(data.p2))
colnames(data.p2)<-c('mean','interval')

#create graph
library(ggplot2)
ggplot(data.p2, aes(x=interval,y=mean)) + geom_line() + geom_point()

#day with max average
data.p2$mean<-as.numeric(data.p2$mean)
data.p2[which.max(data.p2[,1]),2]

#count the number of NAs
sum(is.na(data$steps))

#calculate the mean of each interval
data.p3<-data[!is.na(data$steps),c(1,3)]
data.p3<-tapply(data.p3$steps, data.p3$interval, mean)
data.p3<-as.data.frame(data.p3)
data.p3$date<-as.integer(rownames(data.p3))
colnames(data.p3)<-c('mean','interval')

#seperate data into NA rows and non-NA rows
data.NA<-data[is.na(data$steps),]
data.nonNA<-data[!is.na(data$steps),]

#replace steps with in NA data with the mean of each interval
data.NA<-merge(x=data.NA, y=data.p3, by='interval', all.x=TRUE)
data.NA<-data.NA[,c(1,4,3)]
colnames(data.NA)<-c('interval','steps','date')

#recombine to make a whole dataset
data.comb<-rbind(data.nonNA,data.NA)

#calculate sums of each day
data.p4<-tapply(data.comb$steps, data.comb$date, sum)
data.p4<-as.data.frame(data.p4)
data.p4$date<-as.Date(rownames(data.p4))
colnames(data.p4)<-c('sum', 'date')

#create histogram
library(ggplot2)
ggplot(data.p4, aes(x=date,y=sum)) + geom_histogram(stat='identity')

#calculate the mean and median
mean(data.p4$sum)
median(data.p4$sum)

#add in a column for weekday and weekend flag
data.comb$day<-ifelse(weekdays(data.comb$date) %in% 
                    c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
                    'Weekday',
                    'Weekend')

#split into weekday and weekend
data.comb.day<-subset(data.comb,day=='Weekday')
data.comb.end<-subset(data.comb,day=='Weekend')

#calculate the mean of each interval
data.p5.day<-as.data.frame(tapply(data.comb.day$steps, data.comb.day$interval, mean))
data.p5.day$date<-as.integer(rownames(data.p5.day))
colnames(data.p5.day)<-c('mean','interval')
data.p5.day$day<-'Weekday'

data.p5.end<-as.data.frame(tapply(data.comb.end$steps, data.comb.end$interval, mean))
data.p5.end$date<-as.integer(rownames(data.p5.end))
colnames(data.p5.end)<-c('mean','interval')
data.p5.end$day<-'Weekend'

#recombine to make a full dataset
data.p5<-rbind(data.p5.day, data.p5.end)

#create graph
library(ggplot2)
ggplot(data.p5, aes(x=interval,y=mean)) + facet_grid(day ~ .) + geom_line() + geom_point()