# R script to prepare dataset for *non-DR* event days
# Data source: FMS FTP

library(zoo)
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/")
url = "ftp://fmsguest:gofms!@fmsdevwin.usc.edu/Files/Electrical_Dashboard/"

# read building codes
bcodes = read.csv("Data/bcodes.csv",header=TRUE)

# read temp data
t12 = read.csv("Data/tmp-2012.csv",header=TRUE)
t13 = read.csv("Data/tmp-2013.csv",header=TRUE)
t14 = read.csv("Data/tmp-2014.csv",header=TRUE)

# read names of DR eventdays
schedule = read.csv("DR-Schedule-all.csv",header=TRUE,as.is=TRUE)
buildings = unique(schedule$Building)
numBuildings = length(buildings)

# read DR eventdates
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/DRdataset")
eventDates = list()
for(i in 1:numBuildings){
  bldng = buildings[i]
  DRdays = list.files(pattern=paste(bldng,"-*",sep=""))
  DRdays = substr(DRdays,5,14)
  eventDates[[length(eventDates)+1]] = as.Date(DRdays)
}

# determine all days, DR days and non-DR days
dates12 = unique(as.Date(t12$TimeStamp,"%m/%d/%y"))
dates13 = unique(as.Date(t13$TimeStamp,"%m/%d/%y"))
dates14 = unique(as.Date(t14$TimeStamp,"%m/%d/%y"))
#allDays = c(dates12,dates13,dates14)
allDays = c(dates13)
numAllDays = length(allDays)

# read kwh data serially for all days
missing = NULL
matrixList=list()
for (i in 1:numAllDays){

  cat("\n Day",i,", Bldngs:")
  today = allDays[i]
  # read kwh data for today
  myData = read.csv(paste(url,"export-",today,".csv",sep=""),
                    header=TRUE,sep=",",as.is=TRUE)
  
  # extract data for all DR buildings
  for (j in 1:numBuildings){
    cat(j,",")
    bldng = buildings[j]
    key = bcodes$Building.Key[which(bcodes$Building.Code == bldng)]
    
    # check if DR day for this building
    if(today %in% eventDates[[j]]){
      # do nothing
    }else{
      # create vector for today and add to nonDRdataset
      
      # determine kwh
      kwhIndices = NULL #reset for each iteration
      kwh = NULL #reset for each iteration
      kwhIndices = which(myData$szCity == key)
      if (length(kwhIndices) < 45){     # when kwh data for is missing
        missed = c(bldng,today)
        missing = rbind(missing,missed) # save missed data info
        next   # skip this building; move to next             
      } 
      kwh = myData$Total[kwhIndices]
      kwh = na.fill(kwh, "extend")
      
      # determine temp 
      temp = NULL #reset for each iteration
      yr = as.numeric(format(today,"%Y"))
      if (yr == 2012){
        tempIndices = which(as.Date(t12$TimeStamp,"%m/%d/%y") == today)
        temp = t12$Temperature[tempIndices]
      }else if (yr == 2013){
        tempIndices = which(as.Date(t13$TimeStamp,"%m/%d/%y") == today)
        temp = t13$Temperature[tempIndices]
      }else if (yr == 2014){
        tempIndices = which(as.Date(t14$TimeStamp,"%m/%d/%y") == today)
        temp = t14$Temperature[tempIndices]
      }
      
      # determine DoW vector for the nonDR event
      dayVector = numeric(7)
      day = as.POSIXlt(today)$wday
      if (day == 0){ day = 7} #Sunday
      dayVector[day] = 1
      
      # make vector for the nonDR event day
      nonDRvector = c(kwh,temp,dayVector) # need to add holiday and strategy??
      #nonDRvector = data.frame(today,kwh,temp,dayVector)
      if(length(matrixList) < j){ 
        # first row to be added to the matrix
        matrixList[[j]] = nonDRvector
      }else{
        # row to be appended to the existing matrix
        matrixList[[j]] = rbind(matrixList[[j]],nonDRvector)
      }
      
    }    
  } # done for all buildings
} # done for all days
  
# save results individually for all building
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/")
for (i in 1:length(matrixList)){
  bldng = buildings[i]
  opFile = paste("NonDRdataset/",bldng,"-2012.csv",sep="")  
  write.csv(matrixList[[i]],opFile,row.names=F)  
}

#################
# url<-c("ftp://fmsguest:gofms!@fmsdevwin.usc.edu/Files/Electrical_Dashboard/export-2014-07-28.csv")
# kwh = read.csv(url,
#                header=TRUE,sep=",",as.is=TRUE)# 
# kwh = na.fill(kwh, "extend")
#   begin = as.POSIXct(strptime(paste(eventDate,schedule$Begin[i],sep=" "),"%Y-%m-%d %H:%M"))
#   end = as.POSIXct(strptime(paste(eventDate,schedule$End[i],sep=" "),"%Y-%m-%d %H:%M"))

# matrixList=list()
# #matrixList[[1]]=c(2,3,4,5)
# #matrixList[[2]]=c(2,2)
# matrixList[[1]] = rbind(matrixList[[1]],c(1,2,3,4))
