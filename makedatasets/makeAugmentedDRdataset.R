# R script to prepare augmented dataset for DR event days
# Day vector: kwh,temp,humidity,dow,semester,curt-strategy
# vector length = 96+96+96+7+3+7
# Data source: FMS FTP
# strategies: 1:GTR, 2:VFD, 3:Duty, 4:GTR&VFD, 5:GTR&Duty, 6:VFD&Duty, 7:GTR&VFD&Duty

library(zoo)
SpringEnd = 136
SummerEnd = 229

setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/")
# read building codes
bcodes = read.csv("Data/bcodes.csv",header=TRUE)

# read temp data
t12 = read.csv("Data/tmp-2012.csv",header=TRUE)
t13 = read.csv("Data/tmp-2013.csv",header=TRUE)
t14 = read.csv("Data/tmp-2014.csv",header=TRUE)

# read humidity data
h12 = read.csv("Data/hmdt-2012.csv",header=TRUE)
h13 = read.csv("Data/hmdt-2013.csv",header=TRUE)
h14 = read.csv("Data/hmdt-2014.csv",header=TRUE)

# read DR schedule
schedule = read.csv("DR-Schedule-all.csv",header=TRUE,as.is=TRUE)
numEvents = dim(schedule)[1]

buildings = unique(schedule$Building)
numBuildings = length(buildings)
strategies = unique(schedule$Strategy)
numStrategies = length(strategies)
days = unique(schedule$EventDate)
numDays = length(days)

setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/DRdataset")
allDRFiles = list.files(pattern="*.csv")

for (i in 1:numEvents){
    
  # set metadata about the event
  bldng = schedule$Building[i]
  key = bcodes$Building.Key[which(bcodes$Building.Code == bldng)]
  
  eventDate = as.Date(schedule$EventDate[i],"%m/%d/%y")
  begin = as.POSIXct(strptime(paste(eventDate,schedule$Begin[i],sep=" "),"%Y-%m-%d %H:%M"))
  end = as.POSIXct(strptime(paste(eventDate,schedule$End[i],sep=" "),"%Y-%m-%d %H:%M"))
  strategy = schedule$Strategy[i]

  # read data for the DR event day from current dataset
  ipFile = paste(bldng,"-",eventDate,"-s",strategy,".csv",sep="")
  if(!ipFile %in% allDRFiles){
    next # no data for current event day; move to next event
  }
  cat(i,",")
  myData = read.csv(ipFile,header=TRUE,sep=",",as.is=TRUE)
  myData = myData[,1]
  kwh = myData[1:96]
  temp = myData[97:192]
  dayVector = myData[193:199]
  
  # determine hmdt vector for the DR event day
  hmdtIndices = NULL # reset
  yr = as.numeric(format(eventDate,"%Y"))
  if (yr == 2012){
    hmdtIndices = which(as.Date(h12$TimeStamp,"%m/%d/%y") == eventDate)
    hmdt = h12$Humidity[hmdtIndices]
  }else if (yr == 2013){
    hmdtIndices = which(as.Date(h13$TimeStamp,"%m/%d/%y") == eventDate)
    hmdt = h13$Humidity[hmdtIndices]
  }else if (yr == 2014){
    hmdtIndices = which(as.Date(h14$TimeStamp,"%m/%d/%y") == eventDate)
    hmdt = h14$Humidity[hmdtIndices]
  }
  
  # determine Semester vector for the DR event
  doy = as.numeric(strftime(eventDate, format = "%j"))
  semVector = numeric(3)
  if (doy <= SpringEnd){
    semVector[1] = 1
  } else if (doy > SpringEnd && doy <=SummerEnd){
    semVector[2] = 1
  } else{
    semVector[3] = 1
  }
  
  # determine Strategy vector for the DR event
  strategyVector = numeric(7)
  strategyVector[strategy] = 1

#   kwhIndices = NULL # reset
#   missed = NULL # reset
#   myData = read.csv(paste(url,"export-",eventDate,".csv",sep=""),
#                  header=TRUE,sep=",",as.is=TRUE)
#   kwhIndices = which(myData$szCity == key)
#   if (length(kwhIndices) == 0){     # when kwh data for is missing
#     missed = c(bldng,eventDate,strategy)
#     missing = rbind(missing,missed) # save missed data info
#     next   # skip this DR event; move to next             
#   } 
#   kwh = myData$Total[kwhIndices]
#   kwh = na.fill(kwh, "extend")
#   
#   # determine temp vector for the DR event day
#   tempIndices = NULL # reset
#   yr = as.numeric(format(eventDate,"%Y"))
#   if (yr == 2012){
#     tempIndices = which(as.Date(t12$TimeStamp,"%m/%d/%y") == eventDate)
#     temp = t12$Temperature[tempIndices]
#   }else if (yr == 2013){
#     tempIndices = which(as.Date(t13$TimeStamp,"%m/%d/%y") == eventDate)
#     temp = t13$Temperature[tempIndices]
#   }else if (yr == 2014){
#     tempIndices = which(as.Date(t14$TimeStamp,"%m/%d/%y") == eventDate)
#     temp = t14$Temperature[tempIndices]
#   }

#   # determine DoW vector for the DR event
#   dayVector = numeric(7)
#   day = as.POSIXlt(eventDate)$wday
#   if (day == 0){ day = 7} #Sunday
#   dayVector[day] = 1

  # make vector for the DR event day
#   DRvector = c(kwh,temp,dayVector) # need to add holiday and strategy??

  # make vector for the DR event day
  DRvector = c(kwh,temp,hmdt,dayVector,semVector,strategyVector)
  
  # save results for this event
  opFile = paste("../DRdataset-augmented/",bldng,"-",eventDate,"-s",strategy,".csv",sep="")
  write.csv(DRvector,opFile,row.names=F)  
}
