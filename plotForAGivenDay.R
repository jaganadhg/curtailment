# This file reads and plots kwh values for a given day

library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)

givenDay = "2014-09-16"
dayFile = paste("export-", givenDay, ".csv", sep="")
vectorLength = 199
beginDR = 54 # 1:15 PM
endDR = 69 # 5:00 PM
DRst = c("GTR", "VFD", "DUTY", "GTR & VFD",
         "GTR & DUTY", "VFD & DUTY", "GTR & VFD & DUTY")

# read names of buildings with DR events
#buildings = c("SAL", "VKC", "WPH") # 78,4, 3 
buildings = c("ESH","DML","CDF","CAL","KAM","SCI","GER","JMC")
bid = c(326, 18, 316, 281, 70, 328, 19, 321)

setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/QuickAnalysis/")
data = read.csv(dayFile)
ToD = seq(c(as.POSIXct("2014-09-16 00:00:00")),
          c(as.POSIXct("2014-09-16 23:45:00")), length.out = 96)

# repeat for each building
i = 8
bdata = subset(data, szCity==bid[i])
#timestamp = as.POSIXct(bdata$timeLogged, format="MM")

# calculate drop during DR
ref = bdata$Total[beginDR-1]
DRdata = bdata$Total[beginDR:endDR]
# ref = bdata$Total[31]
# DRdata = bdata$Total[31:endDR]
drop = sum(ref-DRdata)

myDF = data.frame(ToD = ToD,
                  kwh = bdata$Total)
#myDF = data.frame(ToD = timestamp,
#                  kwh = bdata$Total)
p1 = ggplot(myDF,aes(ToD,kwh)) +
  geom_point() +
  geom_line()

p2 = p1 + 
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels=date_format("%H:%M")) +
  xlab("Time of Day") +
  ylab("Electricity Consumption (kWh)") +
  ggtitle(paste(buildings[i], "- total drop = ", drop," kWh",sep=""))

p2
