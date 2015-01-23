# This script calculates curtailment summaries.

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
