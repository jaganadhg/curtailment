# This script does the following
# reads kwh data from FTP for each day
# collects data for nonDR days
# stores nonDR data per building

library(zoo)

setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")

# set FTP data
username = ""
pswd = ""
url = paste("ftp://",username,":",pswd,
            "@fmsdevwin.usc.edu/Files/Electrical_Dashboard/",
            sep="")

# read building codes
bcodes = read.csv("buildingCodes.csv",header=TRUE)

# read event schedule 
schedule12 = read.csv("DRevents2012.csv")
schedule13 = read.csv("DRevents2013.csv")
schedule14 = read.csv("DRevents2014.csv")
DRschedule = rbind(schedule12,schedule13,schedule14)
buildings = unique(DRschedule$Building)

allDates = seq(as.Date("2012-01-01"), by=1, len=sum(366,365,365))
numDays = length(allDates)

#----------------------------
nonDRdates = list()
nonDRkwh = list()

# do for each day
#for(i in 1:numDays){
for(i in 1:10){
  
  ithDay = allDates[i]
  cat("\n",format(ithDay,format="%m-%d-%Y"),"\n")
  # read data for this day
  ipFile = paste(url,"export-",ithDay,".csv",sep="")
  myData = read.csv(ipFile,header=TRUE,sep=",",as.is=TRUE)
  
  # do for each building
  for (j in 1:length(buildings)){
    
    bld = buildings[j]
    # find all event days for this building
    bldEventDataSlice = subset(DRschedule, Building==bld)
    bldEventDays = as.Date(bldEventDataSlice$Date,"%m/%d/%Y")
    
    # skip DR event days
    if(ithDay %in% bldEventDays){
      # don't add
    } else{
      
      cat(as.character(bld), ",")
      # read data for given building
      key = bcodes$Building.Key[which(bcodes$Building.Code == as.character(bld))]
      kwhIndices = which(myData$szCity == key)
      
      # check data for missing values
      if (length(kwhIndices) < 90){     # when kwh data is missing, ok until 6 values
        # don't add, skip this day's data             
      }else{
        d = myData$timeLogged[kwhIndices]
        k = myData$Total[kwhIndices]
        # interpolate for missing data
        k = na.fill(k, "extend")            
        
        # save in the global nonDRarrays
        if (i == 1){
          # first day
          nonDRdates[[j]] = d
          nonDRkwh[[j]] = k
        }else{
          nonDRdates[[j]] = c(nonDRdates[[j]],d)
          nonDRkwh[[j]] = c(nonDRkwh[[j]],k)          
        }
      }   
    }
  } # done for all buildings
} # done for all days

# save building-wise
for (k in 1:length(buildings)){
  df = data.frame(timestamp = nonDRdates[[k]],
                  kwh = nonDRkwh[[k]])
  write.csv(df,paste("../DRdata/",as.character(buildings[k]),".csv",sep=""),row.names=FALSE)
  
}
