# This script reads data from DR event days
# Makes predictions for each DR day

rm(list=ls())
beginDR = 54 # 1:15 PM
endDR = 69 # 5:00 PM
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

# do for all buildings
allMape = list()
allDayCounts = list()
setwd("/Users/saima/Desktop/curtailment/makedatasets/DRdataset/")
for (j in 1:numBuildings){
#for (j in 1:15){
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
  numDays = length(fList)
  
  indices12 = grep("2012",fList)
  indices13 = grep("2013",fList)
  indices14 = grep("2014",fList)
  testIndices = c(indices13,indices14)
  numTestDays = length(testIndices)
  
  ######################## 
  mape = numeric(numTestDays)
  obsDayCount = numeric(numTestDays)
  
  # make predictions
  for (i in 1:numTestDays){
    if(i == 1){
      if(length(indices12) == 0){
        absent2012 = TRUE
        next
      }
      obsIndices = indices12
    }else{
        obsIndices = c(indices12,testIndices[1:i-1])  
    }
    testIndex = testIndices[i]
    testVector = DRvectors[testIndex,(beginDR:endDR)]
    obsData = DRvectors[obsIndices,(beginDR:endDR)] 
    
    if(is.null(dim(obsData))){ # observed data for just 1 day
      predVector = obsData
    }else{
      predVector = apply(obsData,2,mean)  
    }
    
    # calculate errors
    ape = abs(predVector - testVector)/testVector
    mape[i] = mean(ape)
    allMape[[j]] = mape
    
    obsDayCount[i] = length(obsIndices)
    allDayCounts[[j]] = obsDayCount
  } 
  #remove first unused entry for day 1
  if(absent2012 == TRUE){
    allMape[[j]] = allMape[[j]][-1]
    allDayCounts[[j]] = allDayCounts[[j]][-1]
  }
}

# save results
setwd("/Users/saima/Desktop/curtailment/MAPE/")
for(i in 1:length(allMape)){
  if(is.null(allMape[[i]])){
    next  
  }
  df = data.frame(mape = allMape[[i]],
                  daycounts = allDayCounts[[i]])
  opFile = paste("mape-simpleAvg-",allBuildings[i],".csv",sep="")
  write.csv(df,opFile,row.names=F)    
}

