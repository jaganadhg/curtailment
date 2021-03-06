# This script makes predictions for each DR day
# based on KNN model - 
# clusters GLOBAL training data into N clusters
# Traindata is 2/3 of the entire data.

library(NbClust)
beginDR = 54 # 1:15 PM
endDR = 69 # 5:00 PM
vectorLength = 199
absent2012 = FALSE
spr = c(16, 24, 25)

# No. of observed features in preDR signature are
# kwh & temp before DR, and 5 weekdays
preDRindices = c(1:(beginDR-1),
                 (96+1):(96+beginDR-1),
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
# do GLOBAL clustering here:
setwd("/Users/saima/Desktop/curtailment/makedatasets/DRdataset/")
globalTrainData = NULL
for (j in 1:numBuildings){
  if(j %in% spr){   # leave out spr data
    next
  }
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
  
  trainData = DRvectors[trainIndices,]
  globalTrainData = rbind(globalTrainData,trainData)
}

#cluster training Days
c = NbClust(globalTrainData, distance = "euclidean", 
              min.nc = 2, max.nc = 20,
              method = "complete", index = "ch")
numClusters = c$Best.nc[1]

clusters = kmeans(globalTrainData, 10)
dailyProfile = clusters$centers  

#-------------------------------
# do for all buildings
allMape = list()
setwd("/Users/saima/Desktop/curtailment/makedatasets/DRdataset/")

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
  
  testIndices = c((numTrainDays+1):numDays) 
  mape = numeric(numTestDays)
  
  # make predictions
  for (i in 1:numTestDays){
    testIndex = testIndices[i]
    testVector = DRvectors[testIndex,inDRindices]
    #--------------------------
    # define preDR data
    preDRsignatureTest = DRvectors[testIndex,preDRindices]
        
    # calculate distance of preDRsignature of test day
    # with all daily Profiles
    x = rbind(preDRsignatureTest,dailyProfile[,preDRindices])
    d = as.matrix(dist(x))
    d = d[1,2:dim(d)[1]]
    # sort from shortest distance to largest
    sortedIndices = sort(d,index.return=TRUE)$ix
    
    # find probability for a given day belonging to a cluster
    prob = 1/d
    prob = prob/sum(prob) #normalize
    
    # make predictions
    predVector = numeric(length(inDRindices))
    for(k in 1:numClusters){
      predVector = predVector +
          dailyProfile[k,inDRindices] * prob[k]
    }

    #--------------------------
    # calculate errors
    ape = abs(predVector - testVector)/testVector
    mape[i] = mean(ape)
    allMape[[j]] = mape
    
  } 
}

# save results
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-knn-global/")
for(i in 1:length(allMape)){
  if(is.null(allMape[[i]])){
    next  
  }
  df = data.frame(mape = allMape[[i]])
  opFile = paste("mape-knn-global",allBuildings[i],".csv",sep="")
  write.csv(df,opFile,row.names=F)    
}

