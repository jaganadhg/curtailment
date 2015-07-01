# compare and plot mapes from different models

library(ggplot2)
library(reshape2)
library(plyr)

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
}

# # 2. find avg wd mape for all files
# setwd("~/Desktop/curtailment/MAPE/mape-wd/")
# filesWD = list.files(pattern="*.csv")
# numFiles = length(filesWD)
# 
# avgWDerror = numeric(numFiles)
# for (i in 1:numFiles){
#   errors = read.csv(filesWD[i])
#   avgWDerror[i] = mean(errors$mape)
# }
# 
# # 3. find avg wm mape for all files
# setwd("~/Desktop/curtailment/MAPE/mape-wm/")
# filesWM = list.files(pattern="*.csv")
# numFiles = length(filesWM)
# 
# avgWMerror = numeric(numFiles)
# for (i in 1:numFiles){
#   errors = read.csv(filesWM[i])
#   avgWMerror[i] = mean(errors$mape)
# }
# 
# # 4. find avg ws mape for all files
# setwd("~/Desktop/curtailment/MAPE/mape-ws/")
# filesWS = list.files(pattern="*.csv")
# numFiles = length(filesWS)
# 
# avgWSerror = numeric(numFiles)
# for (i in 1:numFiles){
#   errors = read.csv(filesWS[i])
#   avgWSerror[i] = mean(errors$mape)
#   numTestDays[i] = dim(errors)[1]
#   numTrainDays[i] = errors$daycounts[1]
# }
# 
# #----------------------------
# # 5. find avg knn mape for all files
# setwd("~/Desktop/curtailment/MAPE/mape-knn/")
# filesKNN = list.files(pattern="*.csv")
# numFiles = length(filesKNN)
# 
# avgKNNerror = numeric(numFiles)
# for (i in 1:numFiles){
#   errors = read.csv(filesKNN[i])
#   avgKNNerror[i] = mean(errors$mape)
# }
#----------------------------

# 6. find avg ensRF mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ensRF/")
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

avgEnsRFpbError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesEnsRFpb[i])
  avgEnsRFpbError[i] = mean(errors$x)
}

# 8. find avg ensRFglobal mape for all files
setwd("~/Desktop/curtailment/MAPE/mape-ensRFglobal/")
filesEnsRFglobal = list.files(pattern="*.csv")
numFiles = length(filesEnsRFglobal)

avgEnsRFglobalError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesEnsRFglobal[i])
  avgEnsRFglobalError[i] = mean(errors$x)
}

#----------------------------
# save results 
df = data.frame(building = substr(filesWD,9,11),
                numTestDays = numTestDays,
                numTrainDays = numTrainDays,
                IDS = avgError,
                EnsRF = avgEnsRFerror,
                EnsRFpb = avgEnsRFpbError,
                EnsRFglobal = avgEnsRFglobalError)

# leave out spurious data buildings
df = df[-c(16, 24, 25),] #SCC, SCB, LRC
rownames(df) = NULL

#-------------------------
#plot ecdf with ggplot
df1 = subset(df, select =
               c(building,IDS,EnsRF,EnsRFpb,EnsRFglobal))
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
df1[rowx+1,] = c("Avg Error",sum(df$numTestDays),sum(df$numTrainDays),
                mean(df$Histmean),mean(df$WD),mean(df$WM),
                mean(df$WS),mean(df$KNN),mean(df$KNNglobal),
                mean(df$EnsLM),mean(df$EnsLM2),mean(df$EnsLM3),
                mean(df$EnsRT),mean(df$EnsRF))
df1[rowx+2,] = c("Std. dev Error",sum(df$numTestDays),sum(df$numTrainDays),
                sd(df$Histmean),sd(df$WD),sd(df$WM),
                sd(df$WS),sd(df$KNN),sd(df$KNNglobal),
                sd(df$EnsLM),sd(df$EnsLM2),sd(df$EnsLM3),
                sd(df$EnsRT),sd(df$EnsRF))

write.csv(df1,"../avg-mapes.csv",row.names=F)

# remove the last row of averages
#df = df[-(rowx+1),]
#rownames(df) = NULL


