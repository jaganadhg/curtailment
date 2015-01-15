# This file does the following:
# reads DR event dates
# reads kwh data for all DR events
# calculates total curtailment for each DR event
# (uses SCE baseline)

# DR event parameters
beginDR = 54 # 1:15 PM
endDR = 69 # 5:00 PM
  
# set FTP data
username = ""
pswd = ""
url = paste("ftp://",username,":",pswd,
            "@fmsdevwin.usc.edu/Files/Electrical_Dashboard/",
            sep="")

# read building codes
setwd("/Users/saima/Desktop/Energy Experiments/gcode/reducedKWH/")
bcodes = read.csv("buildingCodes.csv",header=TRUE)

# read event data 
data12 = read.csv("DRevents2012.csv")
data13 = read.csv("DRevents2013.csv")
data14 = read.csv("DRevents2014.csv")
DRdata = rbind(data12,data13,data14)
eventDays = unique(DRdata$Date)
numDays = length(eventDays)

#-------------------------
# do for each DR event day
missing = NULL # DR days skipped
curtAll = NULL
eventsAll = NULL
for(i in 1:numDays){
#  for(i in 1:5){
    
  cat("*****day", i,"-", as.character(eventDays[i]),"\n")
  dataSlice = subset(DRdata, Date==eventDays[i])
  eventDate = as.Date(dataSlice$Date[1],"%m/%d/%Y")
  
  # 1. read observed data from FTP for the event day
  ipFile = paste(url,"export-",eventDate,".csv",sep="")
  myDataObs = read.csv(ipFile,header=TRUE,sep=",",as.is=TRUE)
  
  # 2. read baseline data from files for the event day
  ipFile = paste("edison/",eventDate,".csv",sep="")
  if(file.exists(ipFile)){
    #readData = try(read.csv(ipFile))
    myDataBL = read.csv(ipFile,header=TRUE,sep=",",as.is=TRUE)
  }else{
    next
  }
  
  # do for individual buildings on the DR day
  totalCurtailment = 0
  numMissed = 0
  cat("buildings ")
  for (j in 1:dim(dataSlice)[1]){
    bldng = as.character(dataSlice$Building[j])
    strategy = as.character(dataSlice$Strategy[j])
    key = bcodes$Building.Key[which(bcodes$Building.Code == bldng)]
    kwhIndices = which(myDataObs$szCity == key)
    
    cat(",", bldng)
    
    # check data for missing values
    if (length(kwhIndices) < 90){     # when kwh data is missing
      missed = c(bldng,as.character(eventDate),strategy)
      missing = rbind(missing,missed) # save missed data info
      numMissed = numMissed + 1
      cat("-skipped,")
      next   # skip for this building; move to next             
    } 
    kwh = myDataObs$Total[kwhIndices]
    # interpolate for missing data
    kwh = na.fill(kwh, "extend")  
    # extract kwh during DR
    kwhDR = kwh[beginDR:endDR]
    # find BL
    kwhBL = subset(myDataBL,buildings == bldng)
    kwhBL = kwhBL[2:17]
    
    # calculate curtailment
    curtailment = sum(kwhBL - kwhDR)
    #cat(bldng, ":", strategy, ", curt =", curtailment, "\n")
    totalCurtailment = totalCurtailment + curtailment
    #cat("total curt =", totalCurtailment, "\n")
    
  } # done for each building
  cat("\n Total curtailment = ", totalCurtailment, "\n")
  if(numMissed == dim(dataSlice)[1]){
    next    
  }
  # save this events data
  curtAll = rbind(curtAll,totalCurtailment)
  eventsAll = rbind(eventsAll,as.character(eventDate))
  
} # done for each DR event day
#---------------

# frame and save
myDF = data.frame(date = eventsAll,
                  curtailment = curtAll)
write.csv(myDF,"curtailment-SCE.csv")
write.csv(missing,"missingDRdata-SCE.csv",row.names=FALSE)
