# compare and plot mapes from different models
#Insample MAPEs

setwd("/Users/saima/Desktop/curtailment/MAPE/mape-histmean/")
filesHM = list.files(pattern="*.csv")
numFiles = length(filesHM)
numTestDays = numeric(numFiles)
numTrainDays = numeric(numFiles)

# 1. find avg ws mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-ws-insample/")
filesWS = list.files(pattern="*.csv")
numFiles = length(filesWS)

avgWSerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWS[i])
  avgWSerror[i] = mean(errors$mape)
}

# 2. find avg knn mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-knn-insample/")
filesKNN = list.files(pattern="*.csv")
numFiles = length(filesKNN)

avgKNNerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesKNN[i])
  avgKNNerror[i] = mean(errors$mape)
}

# save results 
df = data.frame(building = substr(filesHM,15,17),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                mapeWS = avgWSerror,
                mapeKNN = avgKNNerror)
write.csv(df,"../avg-mapes-insample.csv",row.names=F)
