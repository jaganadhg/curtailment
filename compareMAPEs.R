# compare and plot mapes from different models

setwd("/Users/saima/Desktop/curtailment/MAPE/mape-histmean/")
files = list.files(pattern="*.csv")
numFiles = length(files)

avgError = numeric(numFiles)
numTestDays = numeric(numFiles)
numTrainDays = numeric(numFiles)

# do for all files
for (i in 1:numFiles){
  errors = read.csv(files[i])
  avgError[i] = mean(errors$mape)
  numTestDays[i] = dim(errors)[1]
  numTrainDays[i] = errors$daycounts[1]
}

# save results 
df = data.frame(building = substr(files,15,17),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                mapeHistmean = avgError)
write.csv(df,"../avg-mapes.csv")
