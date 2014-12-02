# This file does the following:
# reads total curtailment for each DR event
# (uses FB: fixed baseline)
# plots curtailment on a calendar

library(quantmod)
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)

# read curtailment data for each event
events = read.csv("curtailment-FB.csv")

dat = data.frame(date=as.Date(events$date),
                 curtailment=events$curtailment)

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
P= ggplot(dat, aes(monthweek, weekdayf, fill = curtailment)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) +
  scale_fill_gradient(low="yellow", high="red") +
  labs(plot.title = "Time-Series Calendar Heatmap") +
  xlab("Week of Month") + ylab("")
P
