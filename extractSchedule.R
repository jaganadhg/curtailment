# This file reads DR schedules from Excel file and 
# outputs event wise CSV file

library(rJava)
library(XLConnect)
library(Hmisc)

year = 2014 # 2013, 2014
maxCols = 100

datex = array(dim=c(0,0))
typex = array(dim=c(0,0))
typex2 = array(dim=c(0,0))
dayx = array(dim=c(0,0))
buildx = array(dim=c(0,0))

setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")
ipSchedule = paste("Schedule", year, ".xls", sep="")

wb <- loadWorkbook(ipSchedule, create = FALSE)
sheetsList <- getSheets(wb)
monthsList = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
strategyList = c("GTR","VFD","DUTY","GTR & VFD","GTR & DUTY","VFD & DUTY","GTR & VFD & DUTY")

for (indx in 1:length(sheetsList)) {
  
  #get month and year from sheet name
  currentMonth = unlist(strsplit(sheetsList[indx]," "))[1]
  currentYear = unlist(strsplit(sheetsList[indx]," "))[2]
  
  currentMonthIdx = which(toupper(monthsList) == 
                            toupper(substr(currentMonth,1,3)))
  if (length(currentMonthIdx) != 0){
    
    firstDate = paste(currentYear,"-",currentMonthIdx,"-1",sep="")
    daysInMonth = monthDays(as.Date(firstDate))
    
    schedule = readWorksheet(wb, sheet = sheetsList[indx],
                             header = FALSE,
                             startRow = 1, startCol = 1,
                             endRow = 5 + daysInMonth,
                             endCol = maxCols,
                             colTypes = c(XLC$DATA_TYPE.STRING))
    
    daysIndex = grep("^[0-9]+$",schedule[,1])
    buildingList = schedule[(daysIndex[1]-1),]
    buildingIndex = grep("^[A-Z]{3}$",buildingList)
    
    index1 = daysIndex[1]-1
    index2 = buildingIndex[length(buildingIndex)]
    
    schedule = schedule[index1:(index1+daysInMonth),
                        2:length(schedule[1,])]
    
    #find building list
    buildList = schedule[1,]
    
    for(jndx in 1:length(buildList))
    {
      DRschedule = schedule[2:length(schedule[,1]),jndx]
      inds = which(DRschedule != "NA" & DRschedule != " ")
      strategies = DRschedule[inds]
      if (length(strategies)!=0){
        buildx = c(buildx,
                    unlist(rep(buildList[jndx],
                               length(strategies))))
        datex = c(datex,
                   paste(currentMonthIdx,"/",inds,"/",
                         currentYear,sep=""))
        typex = c(typex,toupper(strategies))
        dayx = c(dayx,
                 weekdays(as.Date(paste(currentYear,"-",
                                        currentMonthIdx,"-",
                                        inds,sep="")))) 
      }
    }
  }  
}

typex = gsub("/"," & ", typex)
typex = gsub("_"," & ", typex)
typex = gsub(","," & ", typex)
typex = gsub("DTY","DUTY", typex)
typex = replace(typex, typex == "DUTY & GTR", "GTR & DUTY")
typex = replace(typex, typex == "VFD & GTR", "GTR & VFD")
typex = replace(typex, typex == "DUTY & VFD", "VFD & DUTY")

typex2 = typex

for (indx in 1:length(typex)){
  
  if( length(grep("^LVL",typex[indx])) == 1){
    typex2[indx] = strategyList[as.numeric(substr(typex[indx],
                                                  nchar(typex[indx]),
                                                  nchar(typex[indx])))]
  }else if(length(grep("^LEVEL",typex[indx])) == 1){
    typex2[indx] = strategyList[as.numeric(substr(typex[indx],
                                                  nchar(typex[indx]),
                                                  nchar(typex[indx])))]
  }else if (length(which(strategyList == typex[indx]))==0){
    typex2[indx] = sqrt(-1)
  }else if(length(gsub("[^&]","",typex[indx]))>1){
    typex2[indx] = strategyList[7]  
  }
}

my1 = data.frame(buildx,typex2,datex,dayx)
my1 = my1[order(buildx,typex,datex),]
my1a = my1[my1$typex2 %in% strategyList,]

colnames(my1a) = c("Building","Strategy","Date","Day")
write.csv(my1a,paste("DRevents",year,".csv",sep=""), row.names = FALSE)

# my3 <- my1a[c("Building","Date")]
# write.csv(my3,paste(schedule_output,"DRSchedule-Policy2.csv",sep=""), row.names = FALSE)
