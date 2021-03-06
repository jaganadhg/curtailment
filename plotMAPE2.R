# scatter plots 

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
sdKWH = NULL
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
  sdKWH = c(sdKWH, sd(DRvectors[,c(1:96)]))
  newbd = c(newbd, bd)
}  
avgKWH = avgKWH[-c(16, 24, 25)]
sdKWH = sdKWH[-c(16, 24, 25)]
newbd = newbd[-c(16, 24, 25)]

# frame the data
df = data.frame(building = as.character(errors$building),
                avgKWH = avgKWH,
                sdKWH = sdKWH,
                IDS = errors$Histmean,
                Ensemble = errors$EnsRF)

#1. scatter plot kwh vs mape
df1 = melt(df, id = c("building","avgKWH","sdKWH"))
g1 = ggplot(df1) + theme_bw() +
  geom_point(size=3,
             aes(avgKWH,value,
                 color = variable,
                 shape = variable)) +
  geom_smooth(method=lm, aes(x=avgKWH, y=value, color = variable)) 
g2 = g1 + xlab("Average electricity consumption (in kWH)") + ylab("MAPE") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

g3 = g2 + theme(legend.text = element_text(size = 16)) +
  theme(axis.title = element_text(size=14)) +
  theme(axis.text = element_text(size=14))
g3

# 2. scatter plot sd kwh vs mape
g1 = ggplot(df1) + theme_bw() +
  geom_point(size=3,
             aes(sdKWH,value,
                 color = variable,
                 shape = variable)) +
  geom_smooth(method=lm, aes(x=sdKWH, y=value, color = variable)) 
g2 = g1 + xlab("Standard deviation of electricity consumption (in kWH)") + ylab("MAPE") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

g3 = g2 + theme(legend.text = element_text(size = 16)) +
  theme(axis.title = element_text(size=14)) +
  theme(axis.text = element_text(size=14))
g3

#-----------------------
# 5. Paired boxplots
df1 = melt(df, id="building")
g1 = ggplot(df1) +
  geom_boxplot(aes(x=building, y=value,color=variable)) + 
  theme(axis.text.x = element_text(angle=90, vjust=1)) + 
  theme(legend.position = "top")
g1  
