# This file reads an Excel fil containing DR schedules and outputs it into a csv

args <- commandArgs(trailingOnly = TRUE)
 
library(rJava)
library(XLConnect)
library(Hmisc)

datex = array(dim=c(0,0))
typex = array(dim=c(0,0))
typex2 = array(dim=c(0,0))
dayx = array(dim=c(0,0))
buildx <- array(dim=c(0,0))

schedule_input = as.character(args[1])
schedule_output = as.character(args[2])

 
# schedule_input = "C:/Smart Grid/DR_Testing_Schedule3.xls"
# schedule_output = "C:/Smart Grid/"

wb <- loadWorkbook(schedule_input, create = FALSE)
sheetsList <- getSheets(wb)
monthsList = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
strategyList = c("GTR","VFD","DUTY","GTR & VFD","GTR & DUTY","VFD & DUTY","GTR & VFD & DUTY")


for (indx in 1:length(sheetsList)) {
  #  indx = 1
  
  #get month and year from sheet name
  current_month <- unlist(strsplit(sheetsList[indx]," "))[1]
  current_year <- unlist(strsplit(sheetsList[indx]," "))[2]
  
  current_month_idx = which(toupper(monthsList) == toupper(substr(current_month,1,3)))
  if (length(current_month_idx) != 0){
    
    firstdate <- paste(current_year,"-",current_month_idx,"-1",sep="")
    daysinmonth <- monthDays(as.Date(firstdate))
    
    schedule_data = readWorksheet(wb, sheet = sheetsList[indx],  header = FALSE, startRow = 1, startCol = 1, endRow = 5 + daysinmonth ,endCol = 100, colTypes = c(XLC$DATA_TYPE.STRING))
    
    days_index = grep("^[0-9]+$",schedule_data[,1])
    building_list = schedule_data[(days_index[1]-1),]
    building_index = grep("^[A-Z]{3}$",building_list)
    
    index1 <- days_index[1]-1
    index2 <- building_index[length(building_index)]
    
    #schedule_data = readWorksheet(wb, sheet = sheetsList[indx],  header = FALSE, startRow = index1, startCol = 2, endRow = (index1 + daysinmonth) ,endCol = index2, colTypes = c(XLC$DATA_TYPE.STRING))
    
    schedule_data <- schedule_data[index1:(index1+daysinmonth),2:length(schedule_data[1,])]
    
    #finding building list
    
    
    #end finding building list
    
    
    buildlist <- schedule_data[1,]
    
    for(jndx in 1:length(buildlist))
    {
      DR_Schedule <- schedule_data[2:length(schedule_data[,1]),jndx]
      inds <- which(DR_Schedule != "NA" & DR_Schedule != " ")
      strategies <- DR_Schedule[inds]
      if (length(strategies)!=0){
        buildx <- c(buildx,unlist(rep(buildlist[jndx],length(strategies))))
        datex <- c(datex,paste(current_month_idx,"/",inds,"/",current_year,sep=""))
        typex <- c(typex,toupper(strategies))
        dayx <- c(dayx,weekdays(as.Date(paste(current_year,"-",current_month_idx,"-",inds,sep="")))) 
      }
    }
  }  
}

typex <- gsub("/"," & ", typex)
typex <- gsub("_"," & ", typex)
typex <- gsub(","," & ", typex)
typex <- gsub("DTY","DUTY", typex)
typex <- replace(typex, typex == "DUTY & GTR", "GTR & DUTY")
typex <- replace(typex, typex == "VFD & GTR", "GTR & VFD")
typex <- replace(typex, typex == "DUTY & VFD", "VFD & DUTY")

typex2 <- typex


for (indx in 1:length(typex)){
  
  if( length(grep("^LVL",typex[indx])) == 1){
    typex2[indx] = strategyList[as.numeric(substr(typex[indx],nchar(typex[indx]),nchar(typex[indx])))]
  }else if( length(grep("^LEVEL",typex[indx])) == 1){
    typex2[indx] = strategyList[as.numeric(substr(typex[indx],nchar(typex[indx]),nchar(typex[indx])))]
  }else if (length(which(strategyList == typex[indx]))==0){
    typex2[indx] = sqrt(-1)
  }else if(length(gsub("[^&]","",typex[indx]))>1){
    typex2[indx] <- strategyList[7]  
  }
}

my1 <- data.frame(buildx,typex2,datex,dayx)
my1 <- my1[order(buildx,typex,datex),]

my1a<-my1[my1$typex2 %in% strategyList,]

colnames(my1a) <- c("Building","Strategy","Date","Day")

write.csv(my1a,paste(schedule_output,"DRSchedule-Policy.csv",sep=""), row.names = FALSE)

my3 <- my1a[c("Building","Date")]
write.csv(my3,paste(schedule_output,"DRSchedule-Policy2.csv",sep=""), row.names = FALSE)


#options(echo=TRUE)
#cat("Done")
#options(echo=FALSE)
