# This script makes predictions for each DR day *in Sample* 
# based on WS model - weighted similarity
# morning factor: 4 hrs before DR 
# Traindata is 2/3 of the entire data.
# All predictions are saved

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

#-------------------------------
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
  numTrainDays = round((2/3)*numDays)
  numTestDays = numDays - numTrainDays
  
  trainIndices = c(1:numTrainDays)
  testIndices = c((numTrainDays+1):numDays)
    
  trainDates = substr(fList[trainIndices],1,14)  
  testDates = substr(fList[testIndices],1,14)  
  
  # save obs for testdates
  setwd("~/Desktop/curtailment/Obs/test/")
  df = data.frame(date = substr(testDates,5,15),
                  obs=DRvectors[testIndices,inDRindices])
  opFile = paste(bd,"-obs.csv",sep="")
  write.csv(df,opFile,row.names=F) 
  
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
    testVector = testData [inDRindices]
    
    #--------------------------
    # define preDR data
    preDRsignatureTest = testData[preDRindices]
    preDRsignatureTrain = trainData[,preDRindices]
    
    # calculate similarity of preDRsignature of test day with
    # preDR signature of all train days
    x = rbind(preDRsignatureTest,preDRsignatureTrain)
    d = as.matrix(dist(x))
    d = d[1,2:dim(d)[1]]
    sortedIndices = sort(d,index.return=TRUE)$ix
    
    # calculate weights
    neighbors = length(sortedIndices)
    x = seq(0,4,length=neighbors)
    weights = dexp(x,rate=1)
    sa = sum(weights)
    weights = weights/sa
    
    # define neighborhood
    neighborhood = numeric(16)
    if(neighbors == 1){
      neighborhood = neighborhood + 
                      weights * trainData[inDRindices]
    }else{
      for (l in 1:neighbors){
        neighborhood = neighborhood + 
                      weights[l] * trainData[sortedIndices[l],inDRindices]
      }  
    }

    # make predictions    
    predVector = neighborhood # No morning adjustment
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
  setwd("~/Desktop/curtailment/Obs/ws/")
  df1 = data.frame(date = substr(trainDates,5,15), obs=allObs)
  opFile = paste(bd,"-obs.csv",sep="")
  write.csv(df1,opFile,row.names=F) 
  
  setwd("~/Desktop/curtailment/Predictions/ws/")
  df2 = data.frame(date = substr(trainDates,5,15), preds=allPreds)
  opFile = paste(bd,"-preds.csv",sep="")
  write.csv(df2,opFile,row.names=F) 
  
}

# save mape results
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-ws-insample/")
for(i in 1:length(allMape)){
  if(is.null(allMape[[i]])){
    next  
  }
  df = data.frame(mape = allMape[[i]],
                  daycounts = allDayCounts[[i]])
  opFile = paste("mape-ws-insample-",allBuildings[i],".csv",sep="")
  write.csv(df,opFile,row.names=F)    
}

