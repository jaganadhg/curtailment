# This script counts # DR events per year

rm(list=ls())
vectorLength = 199
#read DR vectors
setwd("/Users/saima/Desktop/curtailment/")

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

setwd("/Users/saima/Desktop/curtailment/makedatasets/DRdataset/")

n12 = numeric(numBuildings)
n13 = numeric(numBuildings)
n14 = numeric(numBuildings)
  
for (j in 1:numBuildings){
  bd = allBuildings[j]
  
  # find DR vector file names
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
  # update numdays
  fList = fList[-skipped]
  
  n12[j] = length(grep("2012",fList))
  n13[j] = length(grep("2013",fList))
  n14[j] = length(grep("2014",fList))
}

setwd("/Users/saima/Desktop/curtailment/data/")
df = data.frame(allBuildings,n12,n13,n14)
write.csv(df,"eventCountPerYear.csv",row.names=F)
