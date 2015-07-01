# compare and plot mapes from different models

library(ggplot2)
library(reshape2)
library(plyr)

setwd("~/Desktop/curtailment/MAPE/mape-histmean/")
filesHM = list.files(pattern="*.csv")
numFiles = length(filesHM)
numTestDays = numeric(numFiles)
numTrainDays = numeric(numFiles)

--------------
# 1. find avg histmean mape for all files
avgError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesHM[i])
  avgError[i] = mean(errors$mape)
}

# 1a. find avg histmean-ew mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-histmean-ew/")
filesHMew = list.files(pattern="*.csv")
numFiles = length(filesHMew)

avgErrorEW = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesHMew[i])
  avgErrorEW[i] = mean(errors$mape)
}

# 1b. find avg histmean-mw mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-histmean-mw/")
filesHMmw = list.files(pattern="*.csv")
numFiles = length(filesHMmw)

avgErrorMW = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesHMmw[i])
  avgErrorMW[i] = mean(errors$mape)
}

#-------------------------
# 2. find avg wd mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-wd/")
filesWD = list.files(pattern="*.csv")
numFiles = length(filesWD)

avgWDerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWD[i])
  avgWDerror[i] = mean(errors$mape)
}

#-------------------------
# 3. find avg wm mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-wm/")
filesWM = list.files(pattern="*.csv")
numFiles = length(filesWM)

avgWMerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWM[i])
  avgWMerror[i] = mean(errors$mape)
}

#-------------------------
# 4. find avg ws mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ws/")
filesWS = list.files(pattern="*.csv")
numFiles = length(filesWS)

avgWSerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWS[i])
  avgWSerror[i] = mean(errors$mape)
  numTestDays[i] = dim(errors)[1]
  numTrainDays[i] = errors$daycounts[1]
}

# 4a. find avg ws mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ws-ew/")
filesWSew = list.files(pattern="*.csv")
numFiles = length(filesWSew)

avgWSewError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWSew[i])
  avgWSewError[i] = mean(errors$mape)
}

# 4b. find avg ws mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ws-mw/")
filesWSmw = list.files(pattern="*.csv")
numFiles = length(filesWSmw)

avgWSmwError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWSmw[i])
  avgWSmwError[i] = mean(errors$mape)
}

#----------------------------
# 5. find avg knn mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-knn/")
filesKNN = list.files(pattern="*.csv")
numFiles = length(filesKNN)

avgKNNerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesKNN[i])
  avgKNNerror[i] = mean(errors$mape)
}

# 6. find avg ensRF mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ensrf/")
filesEnsRF = list.files(pattern="*.csv")
numFiles = length(filesEnsRF)

avgEnsRFerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesEnsRF[i])
  avgEnsRFerror[i] = mean(errors$x)
}

# 7. find avg ensRFpb mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ensRFpb/")
filesEnsRFpb = list.files(pattern="*.csv")
numFiles = length(filesEnsRFpb)

avgEnsRFpberror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesEnsRFpb[i])
  avgEnsRFpberror[i] = mean(errors$x)
}

# 8. find avg ensRFglobal mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ensRFglobal/")
filesEnsRFglobal = list.files(pattern="*.csv")
numFiles = length(filesEnsRFglobal)

avgEnsRFglobalerror = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesEnsRFglobal[i])
  avgEnsRFglobalerror[i] = mean(errors$x)
}

#----------------------------
# save results
df = data.frame(building = substr(filesWD,9,11),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                IDS = avgError,
                IDSew = avgErrorEW,
                IDSmw = avgErrorMW,
                WD = avgWDerror,
                WM = avgWMerror,
                WS = avgWSerror,
                WSew = avgWSewError,
                WSmw = avgWSmwError,
                KNN = avgKNNerror,
                EnsRF = avgEnsRFerror,
                EnsRFpb = avgEnsRFpberror,
                EnsRFglobal = avgEnsRFglobalerror
                )

# leave out spurious data buildings
df = df[-c(16, 24, 25),] #SCC, SCB, LRC
rownames(df) = NULL

#-------------------------
#plot ecdf with ggplot
df1 = subset(df, select =
               c(building,Histmean,EnsRF,EnsRFpb,EnsRFglobal))
df2 = melt(df1,id="building")
cdfplot = ggplot(df2, aes(x=value)) + 
            stat_ecdf(aes(colour = variable), size=1)
g1 = cdfplot + theme_bw() + 
          xlab("MAPE") + 
          ylab("Fraction of Buildings") + 
          theme(legend.position="top")+
          theme(legend.title = element_blank()) +
          theme(legend.text = element_text(size = 16)) +
          theme(axis.title = element_text(size=14)) +
          theme(axis.text = element_text(size=14))
g1
#---------------------
# add a row of avg error values
df$building = as.character(df$building)
rowx = dim(df)[1]
df1 = df
df1[rowx+1,] = c("Avg Error",
                 sum(df$numTestDays),sum(df$numTrainDays),
                 mean(df$IDS),
                 mean(df$IDSew),
                 mean(df$IDSmw),
                 mean(df$WD),
                 mean(df$WM),
                 mean(df$WS),
                 mean(df$WSew),
                 mean(df$WSmw),
                 mean(df$KNN),
                 mean(df$EnsRF),
                 mean(df$EnsRFpb),
                 mean(df$EnsRFglobal))
df1[rowx+2,] = c("Std. dev Error",
                 sum(df$numTestDays),sum(df$numTrainDays),
                 sd(df$IDS),
                 sd(df$IDSew),
                 sd(df$IDSmw),
                 sd(df$WD),
                 sd(df$WM),
                 sd(df$WS),
                 sd(df$WSew),
                 sd(df$WSmw),
                 sd(df$KNN),
                 sd(df$EnsRF),
                 sd(df$EnsRFpb),
                 sd(df$EnsRFglobal))

write.csv(df1,"../avg-mapes-2.csv",row.names=F)

# remove the last row of averages
#df = df[-(rowx+1),]
#rownames(df) = NULL


