# This script makes predictions for each DR day *in Sample* 
# based on WM model
# morning factor: 4 hrs before DR 
# Traindata is 2/3 of the entire data.
# All predictions are saved

library(MASS)
beginDR = 54 # 1:15 PM
endDR = 69 # 5:00 PM
vectorLength = 199
absent2012 = FALSE

# No. of observed features in preDR signature are
# kwh & temp before DR, and 5 weekdays
preDRindices = c(((beginDR-16)):(beginDR-1),
                 (96+beginDR-16):(96+beginDR-1),
                 (192+1):197)
inDRindices = c(beginDR:endDR)

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

#-------------------------------
# do for all buildings
allMape = list()
allDayCounts = list()
setwd("~/Desktop/curtailment/makedatasets/DRdataset/")

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
  numDays = length(fList)
  numTrainDays = round((2/3)*numDays)
  numTestDays = numDays - numTrainDays
  
  trainIndices = c(1:numTrainDays)
  testIndices = c((numTrainDays+1):numDays)
  
  trainDates = substr(fList[trainIndices],1,14)  
  
  trainData = DRvectors[trainIndices,]  
  allData = trainData
  
  ######################## 
  # modify for insample data
  mape = numeric(numTrainDays) 
  obsDayCount = numeric(numTrainDays)
  numTestDays = numTrainDays
  
  allPreds = NULL
  allObs = NULL
  # make predictions
  for (i in 1:numTestDays){
    trainData = allData[-i,]
    testData = allData [i,]
    trainIndices = 1:dim(trainData)[1] 
    testVector = testData[inDRindices]
     
    #--------------------------
    # define preDR data
    preDRsignatureTest = testData[preDRindices]
    preDRsignatureTrain = trainData[,preDRindices]
    
    #preDRsignatureTest = DRvectors[testIndex,preDRindices]
    #preDRsignatureTrain = DRvectors[trainIndices,preDRindices]
    
    # calculate weights
    weights = preDRsignatureTest %*% ginv(preDRsignatureTrain)
    
    # make predictions    
    #inDRtrain = DRvectors[trainIndices,inDRindices]
    inDRtrain = trainData[,inDRindices]
    predVector = weights %*% inDRtrain

    allPreds = rbind(allPreds,predVector)
    allObs = rbind(allObs,testVector)
    
    #--------------------------
    # calculate errors
    ape = abs(predVector - testVector)/testVector
    mape[i] = mean(ape)
    allMape[[j]] = mape
    
    obsDayCount[i] = length(trainIndices)
    allDayCounts[[j]] = obsDayCount
  }
  
  # save observed and predicted values
  setwd("~/Desktop/curtailment/Obs/wm/")
  df1 = data.frame(date = substr(trainDates,5,15), obs=allObs)
  opFile = paste(bd,"-obs.csv",sep="")
  write.csv(df1,opFile,row.names=F) 
  
  setwd("~/Desktop/curtailment/Predictions/wm/")
  df2 = data.frame(date = substr(trainDates,5,15), preds=allPreds)
  opFile = paste(bd,"-preds.csv",sep="")
  write.csv(df2,opFile,row.names=F) 
}

# save results
setwd("~/Desktop/curtailment/MAPE/mape-wm-insample/")
for(i in 1:length(allMape)){
  if(is.null(allMape[[i]])){
    next  
  }
  df = data.frame(mape = allMape[[i]],
                  daycounts = allDayCounts[[i]])
  opFile = paste("mape-wm-insample-",allBuildings[i],".csv",sep="")
  write.csv(df,opFile,row.names=F)    
}

