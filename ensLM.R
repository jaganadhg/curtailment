# use linear model to find model weights
# ensLM

# learn ensemble models from training data
setwd("~/Desktop/curtailment/Obs/wm/")
fList = list.files()
numBds = length(fList)

# do for each building
for (i in 1:numBds){
  bd = substr(fList[i],1,3)
  
  # read observed values
  setwd("~/Desktop/curtailment/Obs/wm/")
  allObs = read.csv(fList[i],header=TRUE,as.is=TRUE)
  allObs = allObs[,2:17]
  
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
    df = data.frame(preds1 = predsWM[,j],
                    preds2 = predsWS[,j],
                    preds3 = predsKNN[,j],
                       obs = allObs[,j])
    myLM = lm(obs ~ preds1 + preds2 + preds3, data = df)
    LMmodels[[j]] = myLM
  }
}

#----------------------------------
# make predictions on test data
#predict(myLM, newdata = data.frame(preds1 = 49, preds2 = 51, preds3 = 71))

}
