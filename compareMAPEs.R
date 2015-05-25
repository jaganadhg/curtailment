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

# save results 
df = data.frame(building = substr(filesWD,9,11),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                mapeHistmean = avgError,
                mapeWD = avgWDerror,
                mapeWM = avgWMerror)
write.csv(df,"../avg-mapes.csv",row.names=F)
