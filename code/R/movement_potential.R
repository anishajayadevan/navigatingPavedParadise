#Code to 1) derive movement potential map and 
# 2) reclassify the result into the 5 movement categories (reduced, impeded, unrestricted, increased and channelled)

library(raster)

nl = raster("path/to/mosaic_numIndiv_SB123_Sloth_bear_null.tif") #mosaicked output of null model
ac = raster("path/to/mosaic_numIndiv_SB123_Sloth_bear.tif") #mosaciked output of actual model

stk = stack(nl,ac)

#create a mask layer
msk = calc(stk, fun = sum, na.rm=T)
msk[msk > 0] = 1
msk[msk == 0] = NA

nl[is.na(nl)] = 0
ac[is.na(ac)] = 0

#calculate movement potential
move_potential = (ac+1)/(nl+1)
move_potential = mask(move_potential, msk)

Species="Sloth bear"
UniqueFilName = "SB123"
outputDir="path/to/outputDirectory/"
writeRaster(move_potential,filename=paste0(outputDir, Species,UniqueFilName,"_effect_numIndiv.tif"),overwrite=T)

#### reclassify movement potential map ####

makeQuantMatrix=function(rasterLayer){
  quant=quantile(rasterLayer, probs=c(0,0.1,0.5,0.9,0.95,1))
  m=vector()
  m=c(m,quant[[1]],quant[[2]],-1) #Impeded
  m=c(m,quant[[2]],quant[[3]],-0.5) #Reduced
  m=c(m,quant[[3]],quant[[4]],0) #Unrestricted
  m=c(m,quant[[4]],quant[[5]],0.5) #Increased
  m=c(m,quant[[5]],quant[[6]],1) #Channeled
  reclassMatrix=matrix(m,ncol=3, byrow=TRUE)
  return (reclassMatrix)
}

#function to call makeQuantMatrix for multiple rasters
reclassRaster=function(rasterLayer,filename){
  contig.reclass=reclassify(rasterLayer,makeQuantMatrix(rasterLayer))
  return(contig.reclass)
}

slothBearReclass = reclassRaster(move_potential, "")
writeRaster(slothBearReclass, "path/to/movement potential maps/slothBearReclass.tif")
