# compare and plot mapes from different models

rm(list=ls())

setwd("/Users/saima/Desktop/curtailment/MAPE/mape-histmean/")
files = list.files(pattern="*.csv")
numFiles = length(files)
numTestDays = numeric(numFiles)
numTrainDays = numeric(numFiles)

# find avg histmean mape for all files
avgError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(files[i])
  avgError[i] = mean(errors$mape)
  numTestDays[i] = dim(errors)[1]
  numTrainDays[i] = errors$daycounts[1]
}

# find avg wd mape for all files
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-wd/")
files = list.files(pattern="*.csv")
numFiles = length(files)

avgWDerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(files[i])
  avgWDerror[i] = mean(errors$mape)
}

# save results 
df = data.frame(building = substr(files,15,17),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                mapeHistmean = avgError,
                mapeWD = avgWDerror)
write.csv(df,"../avg-mapes.csv")
