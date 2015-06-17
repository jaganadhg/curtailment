# ensRF: HM, WS, KNN 
# single global RF model for ALL buildings

library(randomForest)
setwd("~/Desktop/curtailment/Obs/wm/")
fList = list.files()
numBds = length(fList)

# do for each building
allPreds1 = NULL
allPreds2 = NULL
allPreds3 = NULL
allObsx = NULL

for (i in 1:numBds){
  bd = substr(fList[i],1,3)
  cat(bd,",")
  
  #---------------------------------------
  # read observed values
  setwd("~/Desktop/curtailment/Obs/wm/")
  allObs = read.csv(fList[i],header=TRUE,as.is=TRUE)
  allObs = allObs[,2:17]
  
  # read predicted values - HM model
  setwd("~/Desktop/curtailment/Predictions/histmean/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsHM = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsHM = predsHM[,2:17]
  
  # read predicted values - WD model
  setwd("~/Desktop/curtailment/Predictions/wd/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsWD = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsWD = predsWD[,2:17]
  
  # read predicted values - WM model
  setwd("~/Desktop/curtailment/Predictions/wm/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsWM = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsWM = predsWM[,2:17]
  
  # read predicted values - WS model
  setwd("~/Desktop/curtailment/Predictions/ws/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsWS = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsWS = predsWS[,2:17]
  
  # read predicted values - KNN model
  setwd("~/Desktop/curtailment/Predictions/knn/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsKNN = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsKNN = predsKNN[,2:17]
  
  #learn RF model
  preds1 = as.vector(as.matrix(predsHM))
  preds2 = as.vector(as.matrix(predsWS))
  preds3 = as.vector(as.matrix(predsKNN))
  obs = as.vector(as.matrix(allObs))
  
  allPreds1 = c(allPreds1,preds1)
  allPreds2 = c(allPreds2,preds2)
  allPreds3 = c(allPreds3,preds3)
  allObsx = c(allObsx,obs)
}

# learn global model for ALL buildings
df = data.frame(allPreds1,
                allPreds2,
                allPreds3,
                allObsx)
myRF = randomForest(allObsx ~ allPreds1 + allPreds2 + allPreds3, 
                    data = df, method = "anova")


#---------------------------------------
#do predictions for all buildings using the GLOBAL model

for (i in 1:numBds){
  bd = substr(fList[i],1,3)
  cat(bd,",")
  
  # read observed data
  setwd("~/Desktop/curtailment/Obs/test/")
  inFile = paste(bd,"-obs.csv",sep="")
  obsTest = read.csv(inFile,header=TRUE,as.is=TRUE)
  obDates = obsTest$date
  obsTest = obsTest[,2:17]
  
  # read HM prediction
  setwd("~/Desktop/curtailment/Predictions/histmean-test/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsHMtest = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsHMtest = predsHMtest[,2:17]
  
  # read WD prediction
  setwd("~/Desktop/curtailment/Predictions/wd-test/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsWDtest = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsWDtest = predsWDtest[,2:17]
  
  # read WM prediction
  setwd("~/Desktop/curtailment/Predictions/wm-test/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsWMtest = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsWMtest = predsWMtest[,2:17]
  
  # read WS prediction
  setwd("~/Desktop/curtailment/Predictions/ws-test/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsWStest = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsWStest = predsWStest[,2:17]
  
  # read KNN prediction
  setwd("~/Desktop/curtailment/Predictions/knn-test/")
  inFile = paste(bd,"-preds.csv",sep="")
  predsKNNtest = read.csv(inFile,header=TRUE,as.is=TRUE)
  predsKNNtest = predsKNNtest[,2:17]
  
  # make predictions
  allPreds1 = as.vector(as.matrix(predsHMtest))
  allPreds2 = as.vector(as.matrix(predsWStest))
  allPreds3 = as.vector(as.matrix(predsKNNtest))
  
  predsT = predict(myRF, 
                   newdata = data.frame(allPreds1,
                                        allPreds2,
                                        allPreds3))
  # put the preds back in matrix form
  predsT = matrix(predsT,ncol=16)
  
  # calculate errors
  ape = abs(predsT - obsTest)/obsTest
  mape = apply(ape,1,mean)
  
  # save predicted values  
  setwd("~/Desktop/curtailment/Predictions/ensRFglobal-test/")
  df2 = data.frame(date = obDates, preds=predsT)
  opFile = paste(bd,"-preds.csv",sep="")
  write.csv(df2,opFile,row.names=F) 
  
  # save errors
  setwd("~/Desktop/curtailment/MAPE/mape-ensRFglobal/")
  opFile = paste("mape-ensrf-",substr(fList[i],1,3),".csv",sep="")
  write.csv(mape,opFile,row.names=F)    
  
} # done for each building
