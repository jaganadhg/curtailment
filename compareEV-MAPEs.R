# compare and plot mapes with different # of EVs

numEVs = 50
setwd("/Users/saima/Desktop/curtailment/MAPE/mape-evs/")

mapeEV = NULL
for (j in 1:nuMEVs){  
  files = list.files(pattern=paste("^",j,"ev",sep=""))
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
  mapeEV = cbind(mapeEV,avgError)
}


# save results 
df = data.frame(building = substr(files,15,17),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                mapeEV = mapeEV)
write.csv(df,"../avg-ev-mapes.csv")
