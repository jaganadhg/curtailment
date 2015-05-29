# compare and plot mapes from different models

setwd("~/Desktop/curtailment/MAPE/mape-histmean/")
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
setwd("~/Desktop/curtailment/MAPE/mape-wd/")
filesWD = list.files(pattern="*.csv")
numFiles = length(filesWD)

avgWDerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWD[i])
  avgWDerror[i] = mean(errors$mape)
}

# 3. find avg wm mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-wm/")
filesWM = list.files(pattern="*.csv")
numFiles = length(filesWM)

avgWMerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWM[i])
  avgWMerror[i] = mean(errors$mape)
}

# 4. find avg ws mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ws/")
filesWS = list.files(pattern="*.csv")
numFiles = length(filesWS)

avgWSerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWS[i])
  avgWSerror[i] = mean(errors$mape)
}

# 5. find avg knn mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-knn/")
filesKNN = list.files(pattern="*.csv")
numFiles = length(filesKNN)

avgKNNerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesKNN[i])
  avgKNNerror[i] = mean(errors$mape)
}

# 6. find avg knn mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-knn-global/")
filesKNNg = list.files(pattern="*.csv")
numFiles = length(filesKNNg)

avgKNNgError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesKNNg[i])
  avgKNNgError[i] = mean(errors$mape)
}

# 7. find avg ensLM mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ensLM/")
filesEnsLM = list.files(pattern="*.csv")
numFiles = length(filesEnsLM)

avgEnsLMerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesEnsLM[i])
  avgEnsLMerror[i] = mean(errors$x)
}

# 8. find avg ensLM mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ensLM2/")
filesEnsLM2 = list.files(pattern="*.csv")
numFiles = length(filesEnsLM2)

avgEnsLMerror2 = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesEnsLM2[i])
  avgEnsLMerror2[i] = mean(errors$x)
}

# save results 
df = data.frame(building = substr(filesWD,9,11),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                Histmean = avgError,
                WD = avgWDerror,
                WM = avgWMerror,
                WS = avgWSerror,
                KNN = avgKNNerror,
                KNNglobal = avgKNNgError,
                EnsLM = avgEnsLMerror,
                EnsLM2 = avgEnsLMerror2)

# leave out spurious data buildings
df = df[-c(16, 24, 25),] #SCC, SCB, LRC
# add a row of avg error values
df$building = as.character(df$building)
rowx = dim(df)[1]
df[rowx+1,] = c("Avg Error",sum(df$numTestDays),sum(df$numTrainDays),
            mean(df$Histmean),mean(df$WD),mean(df$WM),
            mean(df$WS),mean(df$KNN),mean(df$KNNglobal),
            mean(df$EnsLM),mean(df$EnsLM2))

write.csv(df,"../avg-mapes.csv",row.names=F)

#-------------------------
# plot cdf errors
plot(ecdf(df$Histmean),pch=20,cex=.75,xlim=c(0,1),col="red",
     xlab = "MAPE",ylab = "Fraction of buildings",main="")
lines(ecdf(df$WM),col="green",pch=20,cex=.75)
lines(ecdf(df$WS),col="yellow",pch=20,cex=.75)
lines(ecdf(df$KNN),col="blue",pch=20,cex=.75)
lines(ecdf(df$EnsLM),col="grey",pch=20,cex=.75)
#lines(ecdf(),col="magenta",pch=20,cex=.75)
#lines(ecdf(),col="orange",pch=20,cex=.75)

legend(0.55,0.5,c("Histmean","WM","WS","KNN","EnsLM"),
       col=c("red","green","yellow","blue","grey"),
       lty=c(1,1,1,1,1),lwd=c(3,3,3,3,3))

