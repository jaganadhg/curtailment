# plot histogram of events per building

library(ggplot2)
library(reshape2)
library(plyr)

setwd("~/Desktop/curtailment/MAPE/mape-ws/")
filesWS = list.files(pattern="*.csv")
numFiles = length(filesWS)

setwd("~/Desktop/curtailment/MAPE/")
errors = read.csv("avg-mapes.csv", head = TRUE)
errors = errors[1:32,]

#----------------------------
#1. scatter plot kwh vs mape

# read DR vectors
setwd("~/Desktop/curtailment/")

# Find buildings from the DR schedule
schedule12 = read.csv("data/DRevents2012.csv",header=TRUE)
schedule13 = read.csv("data/DRevents2013.csv",header=TRUE)
schedule14 = read.csv("data/DRevents2014.csv",header=TRUE)

buildings12 = as.character(unique(schedule12$Building))
buildings13 = as.character(unique(schedule13$Building))
buildings14 = as.character(unique(schedule14$Building))

testBuildings = unique(c(buildings13,buildings14))
allBuildings = unique(c(buildings12,buildings13,buildings14))
numBuildings = length(allBuildings)

# do for all buildings
vectorLength = 199
avgKWH = NULL
newbd = NULL
for (j in 1:numBuildings){
  bd = allBuildings[j]
  
  setwd("~/Desktop/curtailment/makedatasets/DRdataset/")
  fList = list.files(pattern = paste("^",bd,sep=""))
  if(length(fList)==0){
    next
  }
  numDays = length(fList)
  
  # read DR vectors
  DRvectors = NULL
  skipped = NULL
  for (n in 1:numDays){
    vector = read.csv(fList[n],header=TRUE,as.is=TRUE)  
    vector = vector[,1]
    if(length(vector) != vectorLength){
      skipped = c(skipped,n)
      next  # skip this record
    }
    DRvectors = rbind(DRvectors,vector)
  }
  avgKWH = c(avgKWH,mean(DRvectors[,c(1:96)]))
  newbd = c(newbd, bd)
}  
avgKWH = avgKWH[-c(16, 24, 25)]
newbd = newbd[-c(16, 24, 25)]


  #-----------------------
# 4. Paired boxplots
df1 = melt(df, id="building")
g1 = ggplot(df1) +
  geom_boxplot(aes(x=building, y=value,color=variable)) + 
  theme(axis.text.x = element_text(angle=90, vjust=1)) + 
  theme(legend.position = "top")
g1  
