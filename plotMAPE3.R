# box n whisker plots

library(ggplot2)
library(reshape2)
library(plyr)

# read obs and pred values
setwd("~/Desktop/curtailment/Obs/test/")
fList = list.files(pattern = paste("*.csv"))
fList = fList [-c(16,24,25,26)]
numFiles = length(fList)

dateList = NULL
dowList = NULL
bdList = NULL
mapeHM = NULL
mapeEns = NULL
for (j in 1:numFiles){
  bd = substr(fList[j],1,3)
    
  # read observed values
  setwd("~/Desktop/curtailment/Obs/test/")
  obs = read.csv(fList[j])  
  DRdates = obs$date
  DRdow = weekdays(as.Date(DRdates))
  obs = obs[,2:17]
  
  # read predicted values HM
  setwd("~/Desktop/curtailment/Predictions/histmean-test/")
  predHM = read.csv(paste(bd,"-preds.csv",sep=""))
  predHM = predHM[,2:17]
  errorsHM = abs(obs-predHM)/obs
  errorsHM = apply(errorsHM, 1, mean)
  mapeHM = c(mapeHM,errorsHM)
  
  # read predicted values Ens
  setwd("~/Desktop/curtailment/Predictions/ensrf-test/")
  predEns = read.csv(paste(bd,"-preds.csv",sep=""))
  predEns = predEns[,2:17]
  errorsEns = abs(obs-predEns)/obs
  errorsEns = apply(errorsEns, 1, mean)
  mapeEns = c(mapeEns,errorsEns)
  
  bdList = c(bdList, rep(bd,length(errorsHM)))
  dateList = c(dateList,DRdates)
  dowList = c(dowList,DRdow)
  cat(bd, ":", length(errorsHM), ",", length(errorsEns), "\n")
}

#------------------------------
# frame the data
df = data.frame(building = bdList,
                dowList = dowList,
                dateList = dateList,
                mapeHM = mapeHM,
                mapeEns = mapeEns)
df1 = melt(df, id=c("building","dowList","dateList"))

#------------------------------
# 1. paired boxplots
g1 = ggplot(df1) +
  geom_boxplot(aes(x=building, y=value,color=variable)) + 
  theme(axis.text.x = element_text(angle=90, vjust=1)) + 
  theme(legend.position = "top")

g2 = g1 + xlab("Building") + ylab("MAPE") +
      theme(legend.title = element_blank()) + 
      theme(legend.text = element_text(size = 16)) +
      theme(axis.title = element_text(size=14)) +
      theme(axis.text = element_text(size=14))
g2

#------------------------------
#2. DoW boxplots