# This script makes predictions for each DR day
# based on historical mean of DR days in the training data for that building

# Expanding window (EW)
# Starts from initial 2/3 of the data in the training window
# At each iteration one new data is added

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
trainDayCounts = list()

for (j in 1:numBuildings){
  bd = allBuildings[j]
  cat("\n", bd, ",")
  
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
  
  #-------------------
  # determine initial num of train days & fixed num of test days
  numTrainDays = round((2/3)*numDays)
  numTestDays = numDays - numTrainDays
  testIndices = c((numTrainDays+1):numDays)  
  testDates = substr(fList[testIndices],1,14)
  cat("Total days = ", numDays, 
      ", Num of testdays = ", numTestDays, "\n")
  cat("Num of traindays = ")
  
  mape = numeric(numTestDays)
  allPreds = NULL
  
  # make predictions
  for (i in 1:numTestDays){
    cat(numTrainDays, " ,")
    
    # test data
    testIndex = testIndices[i]
    testVector = DRvectors[testIndex,(beginDR:endDR)]
    
    # train data
    trainIndices = c(1:numTrainDays)
    trainData = DRvectors[trainIndices,(beginDR:endDR)] 
    
    if(is.null(dim(trainData))){ # observed data for just 1 day
      predVector = trainData
    }else{
      predVector = apply(trainData,2,mean)  
    }
    allPreds = rbind(allPreds,predVector)
    
    # calculate errors
    ape = abs(predVector - testVector)/testVector
    mape[i] = mean(ape)
    allMape[[j]] = mape
    
    #trainDayCount[i] = length(trainIndices)
    #trainDayCounts[[j]] = trainDayCount
    
    # add this day to the training data
    numTrainDays = numTrainDays + 1
  
  } # done for all test days
  
  #-------------------------
  # save predicted values  
  setwd("~/Desktop/curtailment/Predictions/histmean-ew-test/")
  df2 = data.frame(date = substr(testDates,5,15), preds=allPreds)
  opFile = paste(bd,"-preds.csv",sep="")
  write.csv(df2,opFile,row.names=F) 
  
} # done for all buildings


# save results
setwd("~/Desktop/curtailment/MAPE/mape-histmean-ew/")
for(i in 1:length(allMape)){
  if(is.null(allMape[[i]])){
    next  
  }
  df = data.frame(mape = allMape[[i]],
                  daycounts = allDayCounts[[i]])
  opFile = paste("mape-histmean-",allBuildings[i],".csv",sep="")
  write.csv(df,opFile,row.names=F)    
}

