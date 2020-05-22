#Code to:
#1. Split rasters into tiles
#2. Split vectors into tiles
#3. Split starting points into tiles
#All tiles have an area of overlap (see comments below SplitRas function)

#######1. RUN SPLITRAS FUNCTION#####
#http://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples 
#The function spatially aggregates the original raster
# it turns each aggregated cell into a polygon
# then the extent of each polygon is used to crop
# the original raster.
# The function returns a list with all the pieces
# in case you want to keep them in the memory. 
# it saves and plots each piece
# The arguments are:
# raster = raster to be chopped            (raster object)
# number of splits in horizonatl (h) and vertical (v) axis (integer)
# e = Buffer width to add to the cropped raster in meters (integer) 
#(for a MW analysis with width of the square=1km, use buffer=1100)
# save   = write raster                    (TRUE or FALSE)
# plot   = do you want to plot the output? (TRUE or FALSE)
SplitRas <- function(raster,h,v,e,save,plot, layerName){
  rasterTileNum=c()
  h        <- ceiling(ncol(raster)/h)#x
  v        <- ceiling(nrow(raster)/v)#y
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extend(extent(agg_poly[agg_poly$polis==i,]),e/2)
    r_list[[i]] <- crop(raster,e1)
  }
  if(save==T){
    for(i in 1:length(r_list)){
      if (length(which(is.na(r_list[[i]][1:length(r_list[[i]])])))!=length(r_list[[i]])){
        crs(r_list[[i]])="+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs"
        writeRaster(r_list[[i]],filename=paste("SplitRas_",layerName,i,sep=""),
                    format="ascii",datatype="FLT4S",overwrite=TRUE)
        rasterTileNum=c(rasterTileNum,i)
        write(i,file=paste("tileNum",layerName,".csv"),append=T)
      }
    }
    
  }
  if(plot==T){
    par(mfrow=c(h,v))
    for(i in 1:length(r_list)){
      plot(r_list[[i]],axes=F,legend=F,bty="n",box=FALSE)  
    }
  }
  return(r_list)
}



######2. SPLIT RASTERS INTO TILES#####
#The number of splits will depend on the size of your raster. The buffer width can be left as 1100
#SET DIRECTORY BEFORE RUNNING SPLITRAS FN
#CREATE RASTER TILES FOLDER IF IT DOESNT EXIST
setwd("path/to/folder")

#read the raster layer you want to split into tiles
lulc = raster("path/to/raster")
SplitRas(lulc,10,48,1100,T,F,"lulc")

#####################################################################################################

######3. SPLIT VECTORS INTO TILES#####
#A csv containing the tile number of each raster tile will be stored as part of the SplitRas function. 
#This csv, along with the extent of each raster tile is used in the SplitVec function

#read the vector layer you want to split into tiles
roads=readOGR("path/to/shapefile")

SplitVec=function(RasterFileID,RastFormat,VectFileID, vectorLayer,rasterDir,outputDir){
  for (i in tileNum[1:nrow(tileNum),]){
    tile=raster(paste(rasterTilesLoc,RasterFileID,i,RastFormat,sep=""))
    projection(tile)= projection
    ext=extent(tile)
    extPolygon=as(ext,'SpatialPolygons')
    projection(extPolygon)=projection(tile)
    cropVector=crop(vectorLayer,extPolygon)
    if(!is.null(cropVector)){ 
      print(paste("Printing file",i))
      setwd(outputDir)
      writeOGR(cropVector,".",paste(VectFileID,i,sep=""),driver="ESRI Shapefile",overwrite=T)
    }
  }
}

#set the location of the folder containing raster tiles
rasterTilesLoc="path/to/rasterTiles"

#load LULC tile num file
tileNum=read.csv("path/to/rastertiles/tileNum lulc .csv",header=F)

#Create output folders if they don't already exist
#CREATE NEW FOLDER VECTOR TILES IF IT DOESN"T EXIST
setwd("path/to/vectortiles folder/")
dir.create("Roads")

#Roads
vectorTilesOutputLoc=paste(getwd(),"/Roads",sep="")
SplitVec("SplitRas_lulc",".asc","Roads",roads,rasterTilesLoc,vectorTilesOutputLoc)

######################################################################################################

######4. SPLIT STARTING POINTS INTO TILES#####

#CREATE SPECIES FOLDERS IN STARTING POINT FILES IF THEY DONT EXIST
setwd("path/to/species/folder")
dir.create("Gaur")

#REMEMBER TO PUT A / AFTER FILE NAMES FOR SPECIESSTPTDIR, TILEDIR,STPTTILES

SpeciesStPtDir="path/to/folder containing starting points/"
tileDir="path/to/RasterTiles/"
stPtTiles="path/to/StartingPointTiles/Gaur/"
tileNum=read.csv("path/to/RasterTiles/tileNum lulc .csv",header=F)


#SETWD and projection before running SplitStartingPt function
setwd(SpeciesStPtDir)
projection = "set projection"

#Run SplitStartingPt function
SplitStartingPt=function(tileDir,RasterFileID,RasterFormat,stPtFile,iterationNum){
  setwd(stPtTiles)
  unlink(paste("Iteration_",iterationNum,sep=""), recursive=TRUE)
  dir.create(paste("Iteration_",iterationNum,sep=""))
  setwd(paste(stPtTiles,"Iteration_",iterationNum,sep=""))
  for (i in tileNum[1:nrow(tileNum),]){
    tile=raster(paste(tileDir,RasterFileID,i,RasterFormat,sep=""))
    projection(tile)=projection
    ext=extent(tile)
    stPts=read.csv(stPtFile)
    
    for (j in 1:nrow(stPts)){
      x=stPts$x[j]
      y=stPts$y[j]
      if (x > ext[1] && x < ext[2] && y > ext[3] && y < ext[4]){
        agent=stPts$status[j]
        id=stPts$id[j]
        row = data.frame(x,y,agent,id)
        
        write.table(row,file=paste("startPoint_Tile_",i,".csv",sep=""),append=T,sep=",",row.names=F,col.names=F)
        write(i,file=paste("stPttileNum.csv"),append=T)
      }
      
    }
  }
}

setwd(SpeciesStPtDir)
for (i in 1:100){
  print(paste("Writing to file number ", i))
  if (i <10) suffix="00"
  if(i>= 10 && i< 100 ) suffix ="0"
  if(i==100) suffix =""
  SplitStartingPt(tileDir,"SplitRas_lulc",".asc",paste(SpeciesStPtDir,"start_pointb",suffix,i,".csv",sep=""),i)
}

