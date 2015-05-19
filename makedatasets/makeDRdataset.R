# R script to prepare dataset for DR event days
# Data source: FMS FTP
# strategies: 1:GTR, 2:VFD, 3:Duty, 4:GTR&VFD, 5:GTR&Duty, 6:VFD&Duty, 7:GTR&VFD&Duty

library(zoo)
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/")
url = "ftp://fmsguest:gofms!@fmsdevwin.usc.edu/Files/Electrical_Dashboard/"
missing = NULL  # missing kwh data

# read building codes
bcodes = read.csv("Data/bcodes.csv",header=TRUE)

# read temp data
t12 = read.csv("Data/tmp-2012.csv",header=TRUE)
t13 = read.csv("Data/tmp-2013.csv",header=TRUE)
t14 = read.csv("Data/tmp-2014.csv",header=TRUE)

# read DR schedule
schedule = read.csv("DR-Schedule-all.csv",header=TRUE,as.is=TRUE)
numEvents = dim(schedule)[1]

buildings = unique(schedule$Building)
numBuildings = length(buildings)
strategies = unique(schedule$Strategy)
numStrategies = length(strategies)
days = unique(schedule$EventDate)
numDays = length(days)

for (i in 1:numEvents){
    
  cat("Event",i,",")
  # set metadata about the event
  bldng = schedule$Building[i]
  key = bcodes$Building.Key[which(bcodes$Building.Code == bldng)]
  
  eventDate = as.Date(schedule$EventDate[i],"%m/%d/%y")
  begin = as.POSIXct(strptime(paste(eventDate,schedule$Begin[i],sep=" "),"%Y-%m-%d %H:%M"))
  end = as.POSIXct(strptime(paste(eventDate,schedule$End[i],sep=" "),"%Y-%m-%d %H:%M"))
  strategy = schedule$Strategy[i]

  # read kwh data for the DR event day
  kwhIndices = NULL # reset
  missed = NULL # reset
  myData = read.csv(paste(url,"export-",eventDate,".csv",sep=""),
                 header=TRUE,sep=",",as.is=TRUE)
  kwhIndices = which(myData$szCity == key)
  if (length(kwhIndices) == 0){     # when kwh data for is missing
    missed = c(bldng,eventDate,strategy)
    missing = rbind(missing,missed) # save missed data info
    next   # skip this DR event; move to next             
  } 
  kwh = myData$Total[kwhIndices]
  kwh = na.fill(kwh, "extend")
  
  # determine temp vector for the DR event day
  tempIndices = NULL # reset
  yr = as.numeric(format(eventDate,"%Y"))
  if (yr == 2012){
    tempIndices = which(as.Date(t12$TimeStamp,"%m/%d/%y") == eventDate)
    temp = t12$Temperature[tempIndices]
  }else if (yr == 2013){
    tempIndices = which(as.Date(t13$TimeStamp,"%m/%d/%y") == eventDate)
    temp = t13$Temperature[tempIndices]
  }else if (yr == 2014){
    tempIndices = which(as.Date(t14$TimeStamp,"%m/%d/%y") == eventDate)
    temp = t14$Temperature[tempIndices]
  }

  # determine DoW vector for the DR event
  dayVector = numeric(7)
  day = as.POSIXlt(eventDate)$wday
  if (day == 0){ day = 7} #Sunday
  dayVector[day] = 1

  # make vector for the DR event day
  DRvector = c(kwh,temp,dayVector) # need to add holiday and strategy??
  
  # save results for this event
  opFile = paste("DRdataset/",bldng,"-",eventDate,"-s",strategy,".csv",sep="")
  write.csv(DRvector,opFile,row.names=F)  
}

write.csv(missing,"Data/DRmissing.csv",row.names=F)  
