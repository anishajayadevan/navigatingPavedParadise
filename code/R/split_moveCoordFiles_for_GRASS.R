#To speed up computation time, you can split iterations between instances of Netlogo (e.g., 20 iterations per NetLogo instance)
#and run these NetLogo instances concurrently. 
#The code below assumes iterations are split between 5 instances. Each instance has a separate output folder,
#named '/Instance<instanceNumber>'

library(raster)
library(rgdal)
library(rgeos)

instanceFiles="path/to/Sloth bear/" #path to output folder containing output files of 5 instances
Species="Sloth bear"
instanceNums=  c(1,2,3,4,5) 
outputDir="path/to/Sloth bear/moveCoords/"
uniqueFileNameID="SB123"

#read movement coordinates in each instance and bind to one csv per instance
bindCSVs=function(instanceDir,instanceNum){
  print(instanceNum)
  dir <- list.files(instanceDir, full.names = T, recursive=T,pattern= '*movementCoordinates*')
  datalist = lapply(dir, function(x)data.table::fread(x, header=F)) 
  fil1 = do.call("rbind", datalist)
  fil1$V3=as.factor(fil1$V3)
  
  rm(dir)
  rm(datalist)
  
  uq_id <- unique(fil1$V3)
  fil1$uqid <- fil1$V3
  levels(fil1$uqid) <- 1:length(unique(fil1$V3))
  
  fil2 <- fil1[, c(1, 2, 8)]
  fil2$uqid=as.numeric(levels(fil2$uqid))[fil2$uqid]
  setwd(outputDir)
  write.csv(fil2,paste0(Species,"_moveCoord_instance",instanceNum,".csv"))
  rm(fil1)
}

mapply(bindCSVs,instanceFiles,instanceNums)

setwd("path/to/Sloth bear/")


#### Split moveCoord files for GRASS####
#Increase computation time in GRASS by splitting moveCoord csvs into smaller shapefiles

fileNames=c(
  "path/to/Sloth_bear_moveCoord_instance1.csv",
  "path/to/Sloth_bear_moveCoord_instance2.csv",
  "path/to/Sloth_bear_moveCoord_instance3.csv",
  "path/to/Sloth_bear_moveCoord_instance4.csv")

instanceNum=c(1, 2, 3, 4, 5)
splitNum=c(1,2,3,4,5) #the number of smaller moveCoord shapefiles per instance

Species="Sloth_bear"
uniqueID="SB123"
outputDir="path/to/split moveCoord/"

#readMCFiles iterates through the moveCoord csvs of each instance and calls splitMoveCoord to split these CSVs
#into smaller shapefiles which can then be read into GRASS
readMCFiles=function(filename,instanceNum){
  moveCoord1=data.table::fread(filename, header=T)
  moveCoord1=as.data.frame(moveCoord1)
  colnames(moveCoord1)[4]="V4"
  moveCoord1=moveCoord1[,-1]
  colnames(moveCoord1)=c("V2","V3","V4")
  moveCoord1$V4=as.numeric(moveCoord1$V4)
  moveCoord1$V2=as.numeric(moveCoord1$V2)
  moveCoord1$V3=as.numeric(moveCoord1$V3)
  
  #specify the range of unique IDs of individuals in each shapefile (split1 - split2)
  #e.g.: the first shapefile will have the movement coordinates of 0-20000 unique indiviudals
  split1=c(0,20000,40000,60000,80000) 
  split2=c(20000,40000,60000,80000,100000,max(moveCoord1$V4)) 
 
  mapply(splitMoveCoord, split1=split1,split2=split2,splitNumber=splitNum,instanceNum=instanceNum,MoreArgs = list(moveCoord1=moveCoord1))
}

mapply(readMCFiles,fileNames,instanceNum)            

#function to split movement coordinate files into shapefiles
splitMoveCoord=function(moveCoord1,split1,split2,splitNumber,instanceNum){
  print("Splitting")
  print(paste("Instance ",instanceNum,"Split ",splitNumber))
  moveCoord1.1=moveCoord1[moveCoord1$V4 >= split1 & moveCoord1$V4 < split2 ,]
  
  coordinates(moveCoord1.1) <- ~V2 + V3
  
  proj4string(moveCoord1.1)="+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs"
  setwd(outputDir)
  writeOGR(moveCoord1.1,".",paste0(uniqueID,"_",Species,"_inst",instanceNum,"_split",splitNumber,"_shapefile"),"ESRI Shapefile",overwrite=T)
  print(paste0("Split",splitNumber," written"))
}




