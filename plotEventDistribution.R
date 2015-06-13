# plot histogram of events per building

library(ggplot2)
library(reshape2)
library(plyr)

newBD = c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10",
          "B11","B12","B13","B14","B15","B16","B17","B18","B19","B20",
          "B21","B22","B23","B24","B25","B26","B27","B28","B29","B30",
          "B31","B32")

setwd("~/Desktop/curtailment/MAPE/mape-ws/")
filesWS = list.files(pattern="*.csv")
numFiles = length(filesWS)
numTestDays = numeric(numFiles)
numTrainDays = numeric(numFiles)

# 1. find avg histmean mape for all files
avgError = numeric(numFiles)
for (i in 1:numFiles){
  errors = read.csv(filesWS[i])
  avgError[i] = mean(errors$mape)
  numTestDays[i] = dim(errors)[1]
  numTrainDays[i] = errors$daycounts[1]
}

#----------------------------
# save results 
df = data.frame(building = substr(filesWS,9,11),
                numTrainDays = numTrainDays,
                numTestDays = numTestDays)

# leave out spurious data buildings
df = df[-c(16, 24, 25),] #SCC, SCB, LRC
rownames(df) = NULL

df$building = newBD

# plot stacked barplots
df$building = factor(df$building, levels = df$building)
df1 = melt(df, id="building")

stack1 = ggplot(df1, aes(x = building, y = value, fill=variable)) +
          theme_bw() +
          geom_bar(stat="identity") + 
          xlab("Building") + ylab("Count") +
          theme(legend.position = "top") +
          theme(legend.title = element_blank()) +
          theme(axis.text.x = element_text(angle=90, vjust=1))

stack2 = stack1 +
          scale_fill_discrete(labels=c("Training Days  ", "Test Days")) + 
          theme(legend.text = element_text(size = 16)) +
          theme(axis.title = element_text(size=18)) +
          theme(axis.text = element_text(size=18))
stack2

#-----------------------
# plot histograms
# distTrain = ggplot(df, aes(x=numTrainDays)) + 
#               geom_histogram(binwidth = 1) 
# distTrain
# 
# distTest = ggplot(df, aes(x=numTestDays)) + 
#             geom_histogram(binwidth = 1) 
# distTest

