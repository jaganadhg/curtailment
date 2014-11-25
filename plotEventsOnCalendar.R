#This file reads DR events and plots them on a calendar plot
library(quantmod)
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)

# read event data 
data12 = read.csv("DRevents2012.csv")
data13 = read.csv("DRevents2013.csv")
data14 = read.csv("DRevents2014.csv")
eventData = rbind(data12,data13,data14)
events = count(eventData,"Date")
dat = data.frame(date=as.Date(events$Date,format = "%m/%d/%Y"),
                 freq=events$freq)

dat$year = as.numeric(as.POSIXlt(dat$date)$year+1900)
dat$month = as.numeric(as.POSIXlt(dat$date)$mon+1)
dat$monthf = factor(dat$month,levels=as.character(1:12),
                    labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
dat$weekday = as.POSIXlt(dat$date)$wday
dat$weekdayf = factor(dat$weekday,levels=rev(0:6),
                      labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)
dat$yearmonth = as.yearmon(dat$date)
dat$yearmonthf = factor(dat$yearmonth)
dat$week = as.numeric(format(dat$date,"%W"))
dat = ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))

# plot
P= ggplot(dat, aes(monthweek, weekdayf, fill = freq)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) +
  scale_fill_gradient(low="yellow", high="red") +
  labs(plot.title = "Time-Series Calendar Heatmap") +
  xlab("Week of Month") + ylab("")
P
