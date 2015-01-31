# This script calculates a summary sheet of all DR events

# Sheet 2
setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")

myData = read.csv("curtailment-SCE-intervalwise.csv")
colnames(myData)[2] = "event"
uniqueDays = unique(myData$event)
num = length(uniqueDays)

campusCurtailment = NULL
selectedBuildings = NULL

for (i in 1:num){
  data.Slice = subset(myData, event == uniqueDays[i])
  col.sums = apply(data.Slice[3:18], 2, sum)
  campusCurtailment = rbind(campusCurtailment,col.sums)
  
  numBuildings = dim(data.Slice)[1]
  selectedBuildings = c(selectedBuildings, numBuildings)
}

# frame and save
myDF = data.frame(selectedBuildings,
                  uniqueDays,
                  campusCurtailment)
write.csv(myDF,"summary-sheet2.csv",row.names=FALSE)

###############

# Sheet 1
setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")

# read metered consumption
meteredConsumption = read.csv("metered-consumption.csv")

# read edison curtailment
curtailmentSCE = read.csv("curtailment-SCE-intervalwise.csv")
colnames(curtailmentSCE)[1] = "bld"
colnames(curtailmentSCE)[2] = "event"
consumptionSCE = read.csv("BLconsumption-SCE-intervalwise.csv")
consumptionSCE = consumptionSCE[,1:18] # drop redundant columns
colnames(consumptionSCE)[1] = "bld"
colnames(consumptionSCE)[2] = "event"

# read fixed baseline curtailment
curtailmentFB = read.csv("curtailment-FB-intervalwise.csv")
colnames(curtailmentFB)[1] = "bld"
colnames(curtailmentFB)[2] = "event"
consumptionFB = read.csv("BLconsumption-FB-intervalwise.csv")
colnames(consumptionFB)[1] = "bld"
colnames(consumptionFB)[2] = "event"

# read dicuf prediction
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/Predictions/IndiePreds-Wtd-TimeWise-MA/")
eventList = NULL
bldList = NULL
dicufList = matrix(0,length(all.files),16)
all.files = list.files()
for(i in 1:length(all.files)){
  
  # extract date and building name
  d = as.Date(substr(all.files[i],10,19))
  b = substr(all.files[i],6,8)
  eventList = c(eventList, d)
  bldList = c(bldList, b)
  
  # read dicuf data
  dicuf = read.csv(all.files[i])
  dicufList[i,]= t(dicuf)
}
dicufDF = data.frame(bld = bldList,
                     event = as.Date(eventList),
                     dicufList,
                     stringsAsFactors=F)

#----------------------------------------------
# initialize arrays
kwhDiffSCE = numeric(dim(meteredConsumption)[1])
kwhDiffFB = numeric(dim(meteredConsumption)[1])
BLconsSCE = numeric(dim(meteredConsumption)[1])
BLconsFB = numeric(dim(meteredConsumption)[1])
dicufCons = numeric(dim(meteredConsumption)[1])
  
# do for each line in meteredConsumption
for (i in 1: dim(meteredConsumption)[1]){
  
  # determine edison baseline data
  # curtailment
  data.Slice1 = subset(curtailmentSCE, bld == meteredConsumption$buildings[i])
  data.Slice2 = subset(data.Slice1, event == meteredConsumption$date[i])
  kwhDiffSCE[i] = sum(data.Slice2[3:18])
  
  # consumption
  data.Slice5 = subset(consumptionSCE, bld == meteredConsumption$buildings[i])
  data.Slice6 = subset(data.Slice5, event == meteredConsumption$date[i])
  BLconsSCE[i] = sum(data.Slice6[3:18])
  
  # determine fixed baseline data
  # curtailment 
  data.Slice3 = subset(curtailmentFB, bld == meteredConsumption$buildings[i])
  data.Slice4 = subset(data.Slice3, event == meteredConsumption$date[i])
  kwhDiffFB[i] = sum(data.Slice4[3:18])
 
  # consumption
  data.Slice7 = subset(consumptionFB, bld == meteredConsumption$buildings[i])
  data.Slice8 = subset(data.Slice7, event == meteredConsumption$date[i])
  BLconsFB[i] = sum(data.Slice8[3:18])
  
  # determine dicuf data
  data.Slice9 = subset(dicufDF, bld == meteredConsumption$buildings[i])
  data.Slice10 = subset(data.Slice9, event == as.character(meteredConsumption$date[i]))
  if(dim(data.Slice10)[1]==0){
    dicufCons[i] = NA
  }else{
    dicufCons[i] = sum(data.Slice10[3:18])    
  }
  
}

# frame and save
myDF = data.frame(buildinglist = meteredConsumption$buildings,
                  dates = meteredConsumption$date,
                  days = weekdays(as.Date(meteredConsumption$date)),
                  strategy = meteredConsumption$strategy,
                  ActualMeteredConsumption = meteredConsumption$consumption,
                  PredictedConsumption = dicufCons,
                  BaselineConsumptionEdison = BLconsSCE,
                  kwhDiffEdison = kwhDiffSCE,
                  BaselineConsumptionFixed = BLconsFB,
                  kwhDiffFixed = kwhDiffFB)

setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")
write.csv(myDF,"summary-sheet1.csv",row.names=FALSE)

