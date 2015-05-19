# This script reads data from DR event days
# Makes predictions for each DR day

#read DR vectors
setwd("/Users/saima/Desktop/curtailment/")

# Find buildings from the DR schedule
schedule12 = read.csv("data/DRevents2012.csv",header=TRUE)
schedule13 = read.csv("data/DRevents2013.csv",header=TRUE)
schedule14 = read.csv("data/DRevents2014.csv",header=TRUE)

buildings12 = as.character(unique(schedule12$Building))
buildings13 = as.character(unique(schedule13$Building))
buildings14 = as.character(unique(schedule14$Building))

testBuildings = unique(c(buildings13,buildings14))
allBuildings = unique(c(buildings12,buildings13,buildings14))
numBuildings = length(buildings)

# read DR vectors
bd = "BKS"
setwd("/Users/saima/Desktop/curtailment/makedatasets/DRdataset/")
fList = list.files(pattern = paste("^",bd,sep=""))
allFiles = lapply(fList,function(i){
         read.csv(i)
        })



# list13 = list.files("2013/")
# list14 = list.files("2014/")
# 
# all12 = lapply(list12,function(i){
#         read.csv(paste("2012/",i,sep=""))
#       })
# all13 = lapply(list13,function(i){
#   read.csv(paste("2013/",i,sep=""))
# })
# all14 = lapply(list14,function(i){
#   read.csv(paste("2014/",i,sep=""))
# })
# 
# 
# # read temperature data
# setwd("/Users/saima/Desktop/Energy Experiments/gcode/weather")
# t12 = read.csv("tmp-2012.csv")
# t13 = read.csv("tmp-2013.csv")
# t14 = read.csv("tmp-2014.csv")
# 
