# This script calculates the following:

setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")

# Sheet 2
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

# read metered consumption
meteredConsumption = read.csv("metered-consumption.csv")

# read dicuf prediction

# read edison curtailment
curtailmentSCE = read.csv("curtailment-SCE-intervalwise.csv")
colnames(curtailmentSCE)[1] = "bld"
colnames(curtailmentSCE)[2] = "event"

# read fixed baseline curtailment
curtailmentFB = read.csv("curtailment-SCE-intervalwise.csv")

# do for each line in meteredConsumption
kwhDiffEdison = numeric(dim(meteredConsumption)[1])
for (i in 1: dim(meteredConsumption)[1]){
  data.Slice1 = subset(curtailmentSCE, bld == meteredConsumption$buildings[i])
  data.Slice2 = subset(data.Slice1, event == meteredConsumption$date[i])
  kwhDiffEdison[i] = sum(data.Slice2[3:18])
}

# frame and save
myDF = data.frame(buildinglist = meteredConsumption$buildings,
                  dates = meteredConsumption$date,
                  strategy = meteredConsumption$strategy,
                  ActualMeteredConsumption = meteredConsumption$consumption,
                  CurtailmentEdison = kwhDiffEdison)

write.csv(myDF,"summary-sheet1.csv",row.names=FALSE)

