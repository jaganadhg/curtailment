# compare and plot mapes from different models

rm(list=ls())

setwd("/Users/saima/Desktop/curtailment/MAPE/mape-histmean/")
filesHM = list.files(pattern="*.csv")
numFiles = length(filesHM)
numTestDays = numeric(numFiles)
numTrainDays = numeric(numFiles)

# 1. find avg histmean mape for all files
avgError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesHM[i])
  avgError[i] = mean(errors$mape)
  numTestDays[i] = dim(errors)[1]
  numTrainDays[i] = errors$daycounts[1]
}

# 2. find avg wd mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-wd/")
filesWD = list.files(pattern="*.csv")
numFiles = length(filesWD)

avgWDerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWD[i])
  avgWDerror[i] = mean(errors$mape)
}

# 3. find avg wm mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-wm/")
filesWM = list.files(pattern="*.csv")
numFiles = length(filesWM)

avgWMerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWM[i])
  avgWMerror[i] = mean(errors$mape)
}

# 4. find avg ws mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-ws/")
filesWS = list.files(pattern="*.csv")
numFiles = length(filesWS)

avgWSerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWS[i])
  avgWSerror[i] = mean(errors$mape)
}

# 5. find avg knn mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-knn/")
filesKNN = list.files(pattern="*.csv")
numFiles = length(filesKNN)

avgKNNerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesKNN[i])
  avgKNNerror[i] = mean(errors$mape)
}

# 6. find avg knn mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-knn-global/")
filesKNNg = list.files(pattern="*.csv")
numFiles = length(filesKNNg)

avgKNNgError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesKNNg[i])
  avgKNNgError[i] = mean(errors$mape)
}

# save results 
df = data.frame(building = substr(filesWD,9,11),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                mapeHistmean = avgError,
                mapeWD = avgWDerror,
                mapeWM = avgWMerror,
                mapeWS = avgWSerror,
                mapeKNN = avgKNNerror,
                mapeKNNglobal = avgKNNgError)
write.csv(df,"../avg-mapes.csv",row.names=F)
