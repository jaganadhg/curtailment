# plot histogram of events per building

library(ggplot2)
library(reshape2)
library(plyr)

setwd("~/Desktop/curtailment/MAPE/mape-ws/")
filesWS = list.files(pattern="*.csv")
numFiles = length(filesWS)

setwd("~/Desktop/curtailment/MAPE/")
errors = read.csv("avg-mapes.csv", head = TRUE)
errors = errors[1:32,]

#----------------------------
# 1. single box plot per model

# frame the data
df = data.frame(building = as.character(errors$building),
                IDS = errors$Histmean,
                Ensemble = errors$EnsRF)

df1 = melt(df, id="building")
g1 = ggplot(df1, aes(variable, value, color = variable)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .3))

g2 = g1 + xlab("Building") + ylab("MAPE") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

g3 = g2 + theme(legend.text = element_text(size = 16)) +
  theme(axis.title = element_text(size=14)) +
  theme(axis.text = element_text(size=14))
g3
#-----------------------
#2. scatter plot: MAPE vs #events in training data

# frame the data
df = data.frame(building = as.character(errors$building),
                numTrain = errors$numTrainDays,
                IDS = errors$Histmean,
                Ensemble = errors$EnsRF)
df1 = melt(df, id = c("building","numTrain"))
g1 = ggplot(df1) + theme_bw() +
      geom_point(size=3,
                 aes(numTrain,value,
                     color = variable,
                     shape = variable)) +
      geom_smooth(method=lm, aes(x=numTrain, y=value, color = variable)) 
g2 = g1 + xlab("Number of Training Days") + ylab("MAPE") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

g3 = g2 + theme(legend.text = element_text(size = 16)) +
  theme(axis.title = element_text(size=14)) +
  theme(axis.text = element_text(size=14))
g3

#-----------------------
# 4. Paired boxplots
df1 = melt(df, id="building")
g1 = ggplot(df1) +
  geom_boxplot(aes(x=building, y=value,color=variable)) + 
  theme(axis.text.x = element_text(angle=90, vjust=1)) + 
  theme(legend.position = "top")
g1  
#---------------------------
#g1 = ggplot(df1, aes(x = building, y = value)) +
#  geom_boxplot() 
#g1
#geom_path(alpha = 0.9)
