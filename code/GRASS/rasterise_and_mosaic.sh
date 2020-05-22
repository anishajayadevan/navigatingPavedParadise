#!/bin/bash
#rasterise shapefiles for each shapefile created as an output of split_moveCoordFiles_for_GRASS.R

#create a grid to store the final output, using one of the input rasters as a reference
#(resolution of output grid should be the same as that of the input grid)
gridName="grid";
species="Sloth_bear";
uniqueFileID="SB123";
instances=("inst1" "inst2" "inst3" "inst4" "inst5");
splitNum=("split1" "split2" "split3" "split4" "split5");

for i in "${instances[@]}"; do
for j in "${splitNum[@]}"; do
echo "$i" echo "$j"
#grassMapset is the name of the mapset you are using
v.vect.stats points="$uniqueFileID""_""$species""_"$i"_"$j"_shapefile@grassMapset" areas=$gridName@grassMapset method=diversity points_column=V4 count_column=count stats_column=div;
v.to.rast --overwrite input=$gridName@grassMapset type=area
#numIndiv: unique individuals; numCross: total number of crossings across all individuals in a cell
output="gridNumIndiv_""$uniqueFileID""_""$species"$i"_"$j use=attr attribute_column=div label_column=div; 
v.to.rast --overwrite input=$gridName@grassMapset type=area output="gridNumCross_""$uniqueFileID""_""$species"$i"_"$j use=attr attribute_column=count label_column=count; done; done;

#mosaic rasters
g.list --q type=rast pattern="gridNumIndiv_""$uniqueFileID""_""$species""*" >> "path/to/gridNumIndiv_""$uniqueFileID""_""$species";
g.list --q type=rast pattern="gridNumCross_""$uniqueFileID""_""$species""*" >> "path/to/gridNumCross_""$uniqueFileID""_""$species";
r.series -z --overwrite file="path/to/gridNumIndiv_""$uniqueFileID""_""$species" output="mosaic_numIndiv_""$uniqueFileID""_""$species" method=sum range=1,100000000;
r.series -z --overwrite file="path/to/gridNumCross_""$uniqueFileID""_""$species" output="mosaic_numCross_""$uniqueFileID""_""$species" method=sum range=1,100000000;

