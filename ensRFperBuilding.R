# ensRF: HM, WS, KNN 

library(randomForest)
setwd("~/Desktop/curtailment/Obs/wm/")
fList = list.files()
numBds = length(fList)

# do for each building
for (i in 1:numBds){
  bd = substr(fList[i],1,3)
  
  #---------------------------------------
  #learn ensemble model from training data
  
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
  
  #RFmodels = list()
  #preds1 = NULL
  #preds2 = NULL
  #preds3 = NULL
  #obs = NULL
 
  preds1 = as.vector(as.matrix(predsHM))
  preds2 = as.vector(as.matrix(predsWS))
  preds3 = as.vector(as.matrix(predsKNN))
  obs = as.vector(as.matrix(allObs))
  
  df = data.frame(preds1,
                  preds2,
                  preds3,
                  obs)
  myRF = randomForest(obs ~ preds1 + preds2 + preds3, data = df, method = "anova")
  #RFmodels[[j]] = myRF
  
  #---------------------------------------
  # make predictions for the test data 
  
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
  
  # do for each interval
  #ensRFpreds = NULL
  preds1 = as.vector(as.matrix(predsHMtest))
  preds2 = as.vector(as.matrix(predsWStest))
  preds3 = as.vector(as.matrix(predsKNNtest))
  obsT = as.vector(as.matrix(obsTest))
  
  #for (j in 1:16){
    
    predsT = predict(myRF, 
                  newdata = data.frame(preds1,
                                       preds2,
                                       preds3))
    #ensRFpreds = cbind(ensRFpreds, preds)
  #}
  
  # calculate errors
  #ape = abs(ensRFpreds - obsTest)/obsTest
  ape = abs(predsT - obsT)/obsT
  #mape = apply(ape,1,mean)
  mape = mean(ape)
  
  #make in matrix form
  predsT = matrix(predsT,ncol=16)
  mape = matrix(mape,ncol=16)
  
  # save predicted values  
  setwd("~/Desktop/curtailment/Predictions/ensRFpb-test/")
  df2 = data.frame(date = obDates, preds=predsT)
  opFile = paste(bd,"-preds.csv",sep="")
  write.csv(df2,opFile,row.names=F) 
  
  # save errors
  setwd("~/Desktop/curtailment/MAPE/mape-ensRFpb/")
  opFile = paste("mape-ensrf-",substr(fList[i],1,3),".csv",sep="")
  write.csv(mape,opFile,row.names=F)    
  
} # done for each building
