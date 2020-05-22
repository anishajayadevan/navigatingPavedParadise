#Generate random starting points for transients and residents, depending on home range area and forest patch size

library(sp)
library(rgdal)
library(maptools)
library(dplyr)
library(raster)
library(rgeos)
library(doMC)


dir = "path/to/directory"
fold = list.dirs(dir,full.names=T,recursive = F)

## This file has min and max homerange listed for each species of our interest. 
#It also has the threshold value of homerange, below which the forest patch is not considered for seeding agents
homerange<-read.csv("path/to/Inputs/homeRange.csv") 


setwd("path/to/folder/containing/species/range/shapefile/") 
s<-list.files(getwd(),"*.shp") ## list the distribution shapefile
shp<-shapefile(s) #read the shapefile

#To avoid placing the points in waterbodies etc,
#we have given a negative buffer which is slightly
#larger than one pixel value (220m) of the raster LULC layer we have used in the analysis
shp<-buffer(shp,-225,dissolve=F)
  
#### check polygons for holes ####
#extract polygons from spatialpolygondataframe
pls<- slot(shp, "polygons")  
# check for holes
pls_new <- lapply(pls, checkPolygonsHoles)  
# fix holes
poly.new <- SpatialPolygonsDataFrame(SpatialPolygons(pls,proj4string=CRS(proj4string(shp))), 
                                       data=as(shp,"data.frame"))
  
a<-substr(s,1,nchar(s)-13)
a<-subset(homerange, homerange$Species == a) ## extract the rows which represent homerange of GSQ in homerange.csv file
mn=a[1,2] #extracting minumum homerange size obtained from the literature
mx=a[1,3] #extracting maximum homerange size obtained from the literature
th=a[1,4] #patch size threshold should be stored along with min and max home ranges in the csv
  
poly.new$area<-area(poly.new)/10000 ## converting areas from sq.m to ha
poly.new <- subset(poly.new, area >th) ##rejecting forest patches smaller than the threshold value
  
#100 random sets of starting points
for(j in 1:100){
    
    print(paste("Generating set", j, "of 100", "on",date()))
    filename=paste0(getwd(), '/start_pointb',formatC(j, width = 3, format = "d", flag = "0"),'.csv') ##output file name
    stpt<-as.data.frame(NULL)
    ## run through each polygon and generate starting points within the patch
    for (i in 1:length(poly.new)) {
        print(paste("Processing polygon", i, 'of', length(poly.new)))
        area<-(poly.new[i,]$areaHa)
        # if patch size is less than the minimum homerange (but more than the threshold)
        #only one point is generated
        if(area<mn){ 
          n=round(runif(1,min=0,max=1))
          if(n==1)
            s1<-spsample(poly.new[i,], n=n,type='random')
          else{next}
        }
        else {
          cx<-(runif(1, min=mn, max=mx)/runif(1,min=0.7,max=0.9))
          cx<-round(2 * sqrt((cx*10000)/((3*sqrt(3)/2))) * sqrt(3)/2)
          cc<-try(spsample(SpatialPolygons(poly.new[i,]),
                           cellsize=cx,type="hexagonal", iter=1000),silent=T)
          if(is(cc,"try-error"))
          {s1<-spsample(poly.new[i,], n=ceiling((area/runif(1, min=mn, max=mx))*runif(1,min=0.7,max=0.9)),type='random')}
          else{
            s1<-cc}
        }
        proj4string(s1) <- CRS("+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs")
        t1 = over(s1, poly.new)
        t1<-cbind(t1,s1@coords)
        
        for (k in 1:nrow(t1)){
          t1[k,6]<-ifelse(t1$areaHa>=th && t1$areaHa<mn, "T", ## 30% of the random points are considered as 'T', transients
                          ifelse(runif(1)<=0.3,"T","R"))
          
        }
        names(t1)<-c("cat","label","areaHa","x","y","status") ## status represents whether an individual point is T (transient) or R (resident)
        stpt<-bind_rows(stpt,t1)
        stpt$id<-paste(stpt$status,sprintf("%06d",as.numeric(row.names(stpt))),"_",j, sep="")
        write.csv(stpt, file=filename, row.names = F)
    }
}
