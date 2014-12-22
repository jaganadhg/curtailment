# This script does the following
# reads kwh data from FTP for each day
# collects data for nonDR days
# stores nonDR data per building

library(zoo)

setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")
year = 2014 #2013, 2012
daysInAyear = 350 # or 365 or 366
  
# set FTP data
username = ""
pswd = ""
loc = ""
username = "fmsguest"
pswd = "gofms!"
loc = "@fmsdevwin.usc.edu/Files/Electrical_Dashboard/"
url = paste("ftp://",username,":",pswd,loc,sep="")

# read building codes
bcodes = read.csv("buildingCodes.csv",header=TRUE)

# read event schedule 
eventFile = paste("DRevents",year,".csv",sep="")
DRschedule = read.csv(eventFile)
buildings = unique(DRschedule$Building)

allDates = seq(as.Date(paste(year,"-01-01",sep="")), by=1, len=daysInAyear)

#----------------------------
# initialize data vectors
DRdates = vector("list",length(buildings))
DRkwh = vector("list",length(buildings))

nonDRdates = vector("list",length(buildings))
nonDRkwh = vector("list",length(buildings))

missed = NULL
# do for each day
for(i in 1:length(allDates)){
  
  ithDay = allDates[i]
  cat("\n",format(ithDay,format="%m-%d-%Y"),"\n")
  # read data for this day
  ipFile = paste(url,"export-",ithDay,".csv",sep="")
  myData = read.csv(ipFile,header=TRUE,sep=",",as.is=TRUE)
  
  # do for each building
  for (j in 1:length(buildings)){
    
    bld = buildings[j]
    key = bcodes$Building.Key[which(bcodes$Building.Code == as.character(bld))]
    kwhIndices = which(myData$szCity == key)
    
    if (length(kwhIndices) < 90){     # when kwh data is missing, ok until 6 values
      # don't add, skip this day's data
      missed = c(missed,paste(bld,",",ithDay,sep=""))
      next
    }
    cat(j, as.character(bld), ",")
    day = myData$timeLogged[kwhIndices]
    kwh = myData$Total[kwhIndices]
    # interpolate for missing data
    kwh = na.fill(kwh, "extend")
  
    # find all event days for this building
    bldEventDataSlice = subset(DRschedule, Building==bld)
    bldEventDays = as.Date(bldEventDataSlice$Date,"%m/%d/%Y")
  
   if(ithDay %in% bldEventDays){
     DRdates[[j]] = c(DRdates[[j]],day)
     DRkwh[[j]] = c(DRkwh[[j]],kwh) 
   } else{
     nonDRdates[[j]] = c(nonDRdates[[j]],day)
     nonDRkwh[[j]] = c(nonDRkwh[[j]],kwh) 
   }   
  } # done for all buildings
} # done for all days

# save building-wise
for (k in 1:length(buildings)){
  
  if(length(DRdates[[k]]) > 0){
    df = data.frame(timestamp = DRdates[[k]],
                    kwh = DRkwh[[k]])
    write.csv(df,paste("../DRdays/",year,"/",as.character(buildings[k]),".csv",sep=""),row.names=FALSE)  
  }
  
  if(length(nonDRdates[[k]]) > 0){
    df = data.frame(timestamp = nonDRdates[[k]],
                  kwh = nonDRkwh[[k]])
    write.csv(df,paste("../nonDRdays/",year,"/",as.character(buildings[k]),".csv",sep=""),row.names=FALSE)
  }
}
