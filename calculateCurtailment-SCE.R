# This file does the following:
# reads DR event dates
# reads kwh data for all DR events
# calculates total curtailment for each DR event
# (uses SCE: socal edison baseline)

# set parameters
year =2014

ithDay = "06/17/2014"
startDR = "13:00"
endDR = "17:00"
intStart = "14:00"

startString = paste(ithDay,"-",startDR,sep="")
intStartString = paste(ithDay,"-",intStart,sep="")
endString = paste(ithDay,"-",endDR,sep="")

adjustStart = as.numeric("16")
adjustDur = as.numeric("12")

# set date and timings of the DR day
startDate = strsplit(startString,"-")[[1]][1]
startTime = strsplit(startString,"-")[[1]][2]
endDate = strsplit(endString,"-")[[1]][1]
endTime = strsplit(endString,"-")[[1]][2]
targetDate = as.Date(startDate, format="%m/%d/%Y")

intStartDate = strsplit(intStartString,"-")[[1]][1]
intStartTime = strsplit(intStartString,"-")[[1]][2]

# findbuildings to process
setwd("/Users/saima/Desktop/Energy Experiments/gcode/nonDRdays/")
buildlist = list.files(pattern="*.csv")
buildlist = unlist(strsplit(buildlist,"_"))
buildlist = sort(buildlist[which((1:length(buildlist)) %% 2 != 0)])

# find list of holidays for each year
key.val=read.table("config.properties", sep="=", col.names=c("key","value"), as.is=c(1,2))
holidays = key.val$val[which(key.val$key == "consume.baseline.holidays")]
holidays = unlist(strsplit(holidays, split=";"))
holidays = as.Date(holidays, format="%m/%d/%Y")

#------------------------------

timelabels = seq(0,23.75,0.25)
mm = (timelabels - floor(timelabels))*60
mm = formatC(mm, width = 2, format = "d", flag = "0")
hh = floor(timelabels)
hh = formatC(hh, width = 2, format = "d", flag = "0")
timelabels = paste(hh, ":", mm, sep="")

startTimeIdx = which(timelabels == startTime) + 1
endTimeIdx = which(timelabels == endTime)
intStartTimeIdx = which(timelabels == intStartTime) + 1

DREventLength = endTimeIdx - startTimeIdx + 1
intEventLength = endTimeIdx - intStartTimeIdx + 1

morningIdxStart = startTimeIdx - adjustStart
morningIdxEnd = morningIdxStart + (adjustDur - 1)
morningLength = morningIdxEnd - morningIdxStart + 1    

output1 = array(dim=c(length(buildlist),intEventLength))
output2 = array(dim=c(length(buildlist),DREventLength))

#-----------------

past10dates = seq( as.Date("2013-01-01"), by=1, len=10)
past10daysDRdata = array(dim=c(10,DREventLength))
past10daysMRdata = array(dim=c(10,12))
buildings = array(dim=c(0,0))

for (indx in 1:length(buildlist)){
  
  cat("building#: ", indx, "\n")
  filename = paste(buildlist[indx],"_c.csv",sep="")
  readData = read.csv(filename,header = TRUE, skip = 1)
  
  currentPrevDate = targetDate - 1
  daysFound = 0
  
  morningStrLabel = paste(startDate,"-",timelabels[morningIdxStart],sep="")                                                                            
  morningStpLabel = paste(startDate,"-",timelabels[morningIdxEnd],sep="")                                                                            
  
  readStart = which(readData[,1] == morningStrLabel)
  readEnd = which(readData[,1] == morningStpLabel)
  
  validData = FALSE
  DRMorningData = 0
  buildings = c(buildings,buildlist[indx])
  
  # read data for the DR morning (9:15 am to 12 noon)
  if (length(readStart) != 0 && length(readEnd) != 0){
    DRMorningData = readData[readStart:readEnd,3]
    validData = TRUE  
  }

  while (daysFound < 10)
  {
    cat("days Found = ", daysFound)
    cat("current previous date = ", 
        format(currentPrevDate,format="%m-%d-%Y"), "-------\n")
    
    # check if the previous date is not a holiday or weekend
    if (!(currentPrevDate %in% holidays) &&
          !grepl("S+",weekdays(currentPrevDate))){
      
      kWhDRData = 0
      kWhMRData = 0
      
      readStart = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y"),"-",timelabels[startTimeIdx],sep=""))
      readEnd = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y"),"-",timelabels[endTimeIdx],sep=""))
      
      if (length(readStart) != 0 && length(readEnd) != 0){
        kWhDRData = readData[readStart:readEnd,3]
      }
      readStart = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y"),"-",timelabels[morningIdxStart],sep=""))
      readEnd = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y"),"-",timelabels[morningIdxEnd],sep=""))
      
      if (length(readStart) != 0 && length(readEnd) != 0){
        kWhMRData = readData[readStart:readEnd,3]
      }
      if (!(length(kWhDRData) != DREventLength) && !(length(kWhMRData) != morningLength)){
        if (!(-1 %in% kWhDRData) && !(-1 %in% kWhMRData)) {     
          daysFound = daysFound + 1
          past10dates[daysFound] = as.Date(currentPrevDate)
          past10daysDRdata[daysFound,] = kWhDRData
          past10daysMRdata[daysFound,] = kWhMRData
        }
      }
    }
    currentPrevDate =currentPrevDate - 1
  }
  
  if (validData == TRUE){
    prev_morning_avg = apply(past10daysMRdata, 2, mean, na.rm = TRUE)
    adjustfactor = sum(DRMorningData)/sum(prev_morning_avg)
  }else{
    adjustfactor <= 1
  }  
  unadj_baseline = apply(past10daysDRdata, 2, mean, na.rm = TRUE)
  output1[indx,] = adjustfactor * unadj_baseline[(length(unadj_baseline)-intEventLength+1):length(unadj_baseline)]
  output2[indx,] = adjustfactor * unadj_baseline
  
}

my1 = data.frame(buildings,output1)
write.table(my1, Outputfilepath, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
