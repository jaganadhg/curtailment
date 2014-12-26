# This file does the following:
# reads DR event dates 
# reads kwh data for all nonDR events
# calculates baseline for each DR event
# (uses SCE: socal edison baseline)

#year = 2014
#firstDayOfYear = paste("01/01/",year,sep="")
firstDayOfData = "01/01/2012"
startDR = "13:00"
intStart = "14:00" # same as DR
endDR = "17:00"
adjustStart = as.numeric("16")
adjustDur = as.numeric("12")

# define holidays
holidays = c("01/01/2012","01/02/2012","01/16/2012","02/20/2012",
                "05/28/2012","07/04/2012","09/03/2012","11/21/2012",
                "11/22/2012","11/23/2012","12/25/2012",
               "01/01/2013","01/21/2013","02/18/2013",
                "05/27/2013","07/04/2013","09/02/2013","11/27/2013",
                "11/28/2013","11/29/2013","12/25/2013",
               "01/01/2014","01/20/2014","02/17/2014",
                "05/26/2014","07/04/2014","09/01/2014","11/26/2014",
                "11/27/2014","11/28/2014","12/25/2014")
holidays = as.Date(holidays, format="%m/%d/%Y")

timelabels = seq(0,23.75,0.25)
mm = (timelabels - floor(timelabels))*60
mm = formatC(mm, width = 2, format = "d", flag = "0")
hh = floor(timelabels)
hh = formatC(hh, width = 2, format = "d", flag = "0")
timelabels = paste(hh, ":", mm, sep="")

# read event schedule 
setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")
DRschedule2012 = read.csv("DRevents2012.csv")
DRschedule2013 = read.csv("DRevents2013.csv")
DRschedule2014 = read.csv("DRevents2014.csv")
DRschedule = rbind(DRschedule2012,DRschedule2013,DRschedule2014)
eventDays = unique(DRschedule$Date)
#-----------------

# do for each DR event day
#for(id in 1:5){
for(id in 64:length(eventDays)){
  
  ithDay = eventDays[id]
  cat("\n", as.character(ithDay), "\n")
  year = substr(ithDay,nchar(as.character(ithDay))-3,nchar(as.character(ithDay)))
  ithDayBuildings = subset(DRschedule,Date == ithDay)
  buildlist = ithDayBuildings$Building
  
  # set date and timings of the DR day
  startString = paste(ithDay," ",startDR,sep="")
  intStartString = paste(ithDay," ",intStart,sep="")
  endString = paste(ithDay," ",endDR,sep="")  
  startDate = strsplit(startString," ")[[1]][1]
  startTime = strsplit(startString," ")[[1]][2]
  endDate = strsplit(endString," ")[[1]][1]
  endTime = strsplit(endString," ")[[1]][2]
  targetDate = as.Date(startDate, format="%m/%d/%Y")
  
  intStartDate = strsplit(intStartString," ")[[1]][1]
  intStartTime = strsplit(intStartString," ")[[1]][2]
    
  startTimeIdx = which(timelabels == startTime) + 1
  endTimeIdx = which(timelabels == endTime)
  intStartTimeIdx = which(timelabels == intStartTime) + 1
  
  DREventLength = endTimeIdx - startTimeIdx + 1
  intEventLength = endTimeIdx - intStartTimeIdx + 1
  
  morningIdxStart = startTimeIdx - adjustStart
  morningIdxEnd = morningIdxStart + (adjustDur - 1)
  morningLength = morningIdxEnd - morningIdxStart + 1    
  
  # initialize arrays for the ith event day
  output1 = array(dim=c(length(buildlist),intEventLength))
  output2 = array(dim=c(length(buildlist),DREventLength))
  past10dates = seq(as.Date("2013-01-01"), by=1, len=10)
  past10daysDRdata = array(dim=c(10,DREventLength))
  past10daysMRdata = array(dim=c(10,12))
  buildings = array(dim=c(0,0))
  
  # do for all buildings that had DR on the ith DR day
  for (indx in 1:length(buildlist)){
    
    cat(as.character(buildlist[indx]), ", ")
    buildings = c(buildings,as.character(buildlist[indx]))
    inFile = paste("../nonDRdays/",year,"/",buildlist[indx],".csv",sep="")
    if(file.exists(inFile)){
      readData = try(read.csv(inFile, header = TRUE, skip = 1))      
    }else{
      next
    }
    
    currentPrevDate = targetDate - 1
    daysFound = 0
    
    morningStrLabel = paste(startDate," ",timelabels[morningIdxStart],sep="")                                                                            
    morningStpLabel = paste(startDate," ",timelabels[morningIdxEnd],sep="")                                                                            
    
    readStart = which(readData[,1] == morningStrLabel)
    readEnd = which(readData[,1] == morningStpLabel)
    
    validData = FALSE
    DRMorningData = 0
    
    # read data for the DR morning (9:15 am to 12 noon)
    if (length(readStart) != 0 && length(readEnd) != 0){
      DRMorningData = readData[readStart:readEnd,2]
      validData = TRUE  
    }
    
    notFound = FALSE
    while (daysFound < 10)
    {   
      # check if the previous date is not a holiday or weekend
      if (!(currentPrevDate %in% holidays) &&
            !grepl("S+",weekdays(currentPrevDate))){
        
        kWhDRData = 0
        kWhMRData = 0
        
        # Determine DR duration data
        readStart = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y")," ",
                                                timelabels[startTimeIdx],sep=""))
        readEnd = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y")," ",
                                              timelabels[endTimeIdx],sep=""))
        if (length(readStart) != 0 && length(readEnd) != 0){
          kWhDRData = readData[readStart:readEnd,2]
        }
        
        # Determine DR morning data
        readStart = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y")," ",
                                                timelabels[morningIdxStart],sep=""))
        readEnd = which(readData[,1] == paste(format(currentPrevDate,"%m/%d/%Y")," ",
                                              timelabels[morningIdxEnd],sep=""))
        if (length(readStart) != 0 && length(readEnd) != 0){
          kWhMRData = readData[readStart:readEnd,2]
        }
        
        # determine if an appropriate previous date found
        if (!(length(kWhDRData) != DREventLength) && 
              !(length(kWhMRData) != morningLength)){
          if (!(-1 %in% kWhDRData) && !(-1 %in% kWhMRData)) {     
            daysFound = daysFound + 1
            past10dates[daysFound] = as.Date(currentPrevDate)
            past10daysDRdata[daysFound,] = kWhDRData
            past10daysMRdata[daysFound,] = kWhMRData
            #cat(as.character(currentPrevDate), ",")
          }
        }
      }
      
      currentPrevDate = currentPrevDate - 1
      if(as.Date(currentPrevDate) < as.Date(firstDayOfData,format="%m/%d/%Y"))
      {
        notFound = TRUE
        cat("not found in this year")
        break
      }      
    }# done for all previous days in the year
    
    if(notFound == TRUE){
      next
    }
    if (validData == TRUE){
      prev_morning_avg = apply(past10daysMRdata, 2, mean, na.rm = TRUE)
      adjustfactor = sum(DRMorningData)/sum(prev_morning_avg)
    }else{
      adjustfactor <= 1
    } 
    
    # calculate baselines
    unadj_baseline = apply(past10daysDRdata, 2, mean, na.rm = TRUE)
    output1[indx,] = adjustfactor * unadj_baseline[(length(unadj_baseline)-intEventLength+1):length(unadj_baseline)]
    output2[indx,] = adjustfactor * unadj_baseline
    
  }  # done for all buildings
  
  # save baseline data for the ith DR day
  opDF = data.frame(buildings,output2)
  opFile = paste("edison/",as.Date(ithDay, format="%m/%d/%Y"),".csv",sep="")
  write.csv(opDF, opFile, row.names = FALSE)
   
} # done for all eventdays
