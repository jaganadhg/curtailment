# This script makes predictions for each DR day
# based on historical mean of DR days in the training data for that building

# Moving Window (MW)
# Training window width is static = 2/3 the data
# Starts from initial 2/3 of the data in the training window
# At each iteration one new data is added and one last one is dropped

rm(list=ls())
beginDR = 54 # 1:15 PM
endDR = 69 # 5:00 PM
vectorLength = 199
absent2012 = FALSE

#read DR vectors
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
allMape = list()
allDayCounts = list()

for (j in 1:numBuildings){
  bd = allBuildings[j]

  # find DR vector file names
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
  # update numdays
  fList = fList[-skipped]
  numDays = length(fList)
  numObsDays = round((2/3)*numDays)
  numTestDays = numDays - numObsDays
  
  obsIndices = c(1:numObsDays)
  testIndices = c((numObsDays+1):numDays)
  
  #trainIndices = c(1:numTrainDays)
  #testIndices = c((numTrainDays+1):numDays)
  testDates = substr(fList[testIndices],1,14)
  
  ######################## 
  mape = numeric(numTestDays)
  obsDayCount = numeric(numTestDays)
  allPreds = NULL
  
  # make predictions
  for (i in 1:numTestDays){
    testIndex = testIndices[i]
    testVector = DRvectors[testIndex,(beginDR:endDR)]
    obsData = DRvectors[obsIndices,(beginDR:endDR)] 
    
    if(is.null(dim(obsData))){ # observed data for just 1 day
      predVector = obsData
    }else{
      predVector = apply(obsData,2,mean)  
    }
    allPreds = rbind(allPreds,predVector)
    
    # calculate errors
    ape = abs(predVector - testVector)/testVector
    mape[i] = mean(ape)
    allMape[[j]] = mape
    
    obsDayCount[i] = length(obsIndices)
    allDayCounts[[j]] = obsDayCount
  } 
  
  # save predicted values  
  setwd("~/Desktop/curtailment/Predictions/histmean-test/")
  df2 = data.frame(date = substr(testDates,5,15), preds=allPreds)
  opFile = paste(bd,"-preds.csv",sep="")
  write.csv(df2,opFile,row.names=F) 
  
}

# save results
setwd("~/Desktop/curtailment/MAPE/mape-histmean/")
for(i in 1:length(allMape)){
  if(is.null(allMape[[i]])){
    next  
  }
  df = data.frame(mape = allMape[[i]],
                  daycounts = allDayCounts[[i]])
  opFile = paste("mape-histmean-",allBuildings[i],".csv",sep="")
  write.csv(df,opFile,row.names=F)    
}

