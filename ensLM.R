# use linear model to find model weights
# ensLM1: WM, WS, KNN
# ensLM2: WD, WS, KNN
# ensLM3: HM, WS, KNN 

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
  
  LMmodels = list()
  # do for each interval
  for (j in 1:16){
    
    # build a linear model
    df = data.frame(preds1 = predsHM[,j],
                    preds2 = predsWS[,j],
                    preds3 = predsKNN[,j],
                       obs = allObs[,j])
    myLM = lm(obs ~ preds1 + preds2 + preds3, data = df)
    LMmodels[[j]] = myLM
  }
  
  #---------------------------------------
  # make predictions for the test data 
  
  # read observed data
  setwd("~/Desktop/curtailment/Obs/test/")
  inFile = paste(bd,"-obs.csv",sep="")
  obsTest = read.csv(inFile,header=TRUE,as.is=TRUE)
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
  ensLMpreds = NULL
  for (j in 1:16){
    
    preds = predict(LMmodels[[j]], 
                  newdata = data.frame(preds1 = predsHMtest[,j],
                                       preds2 = predsWStest[,j],
                                       preds3 = predsKNNtest[,j]))
    ensLMpreds = cbind(ensLMpreds, preds)
  }
  
  # calculate errors
  ape = abs(ensLMpreds - obsTest)/obsTest
  mape = apply(ape,1,mean)
  
  # save errors
  setwd("~/Desktop/curtailment/MAPE/mape-enslm3/")
  opFile = paste("mape-enslm-",substr(fList[i],1,3),".csv",sep="")
  write.csv(mape,opFile,row.names=F)    
  
} # done for each building
