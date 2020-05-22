;the code for the null model is the same as the actual model.
;the only difference is the absence of LULC, infrastructure and human population when calculating movement probabilities

breed [transients transient]
breed [residents resident]
breed [settledTransients settledTransient]

extensions [gis rnd cf table csv]

;attributes of each patch (in netlogo, a patch is a cell)
patches-own[lulc numIndividualsList numCrossing dem tpi   riverBridge movementProbability numIndividualsCrossed
    treeCover  areaGtHR border]

;attributes of each individual
transients-own[timeInUnsuitableHabitat  stepCount unsuitableHabitatStep dispDist transientHomeRangeRadius id]
residents-own[timeInUnsuitableHabitat stepCount homeX homeY unsuitableHabitatStep homeRangeRadius id]
settledTransients-own[stepCount]

;global variables
globals[
  moveProbTable
  numTiles
  tileNum
  iterationNum
  lulcLayer
  agentsDiscarded
  runNum
  runsAfterIterations
]

to setup
  set-current-directory (word outputDirectory Species "/")
  ifelse file-exists? "modelComplete.txt" [
    show "model has completed running. Delete modelCompelte.txt to run again"
  ]
  [
    start
  ]
end

to start
  clear-all ;clear landscape
  file-close-all
  initTable
  initGlobals
  loadMovementProb
  loadNumTiles
  iterate
  handleSpilledOverAgents
  deleteOutputCSVs
  cratemodelCompleteTxt
  stop
end

to initTable
  set moveProbTable table:make
end

to loadMovementProb
  file-open movementProbFile
  let headers csv:from-row file-read-line
  let row csv:from-row file-read-line
  foreach headers[
    x -> table:put moveProbTable x (item (position x headers) row)
  ]
  file-close
end

to loadNumTiles

  set numTiles []
  file-open numTilesFile
  while [not file-at-end?][
    let x csv:from-row file-read-line
    set numTiles lput (item 0 x) numTiles
  ]

  file-close
end

to initFiles
  set-current-directory (word outputDirectory Species "/")
  file-open "movementCoordinates.csv"
  file-print "x,y,ID,TileNum,IterationNum,runNum"
  file-close
  file-open "transientStatus.csv"
  file-print "x,y,ID,iterationNum,runNum,tileNum,Status,stepCount,dispDist"
  file-close
end

to initGlobals
  set runsAfterIterations false
  gis:load-coordinate-system projectionFile
end

to iterate
  let iterNums []
  let nextIterAndTile []
  set iterationNum 1
  let i 0; counter for tiles
  let runComplete true
  let iterationsComplete false

  set runComplete checkIfPreviousRunIsComplete
  if runComplete = false[
    set nextIterAndTile setIterAndTileNum
    if item 0 nextIterAndTile = (numIterations + 1) [ set runComplete true set iterationsComplete true set iterationNum item 0 nextIterAndTile ]
  ]

  if iterationsComplete = false[
    ifelse runComplete = true [
      show "previous model runs completed without aborting"
      set i position (reportStartingTile ) numTiles
      set tileNum item i numTiles
    ]
    [
      let lastIterNum 0
      show "RESUMING. previous model runs stopped before completion, adding tile for the iteration last completed"
      set nextIterAndTile setIterAndTileNum
      set iterationNum ( item 0 nextIterAndTile)
      set i position (item 1 nextIterAndTile) numTiles
      set-current-directory (word outputDirectory Species "/")
    ]

    set tileNum item i numTiles
    deleteIncompleteFiles

    while [iterationNum <= numIterations][

      ifelse runComplete = true [
        set i position (reportStartingTile ) numTiles
      ]
      [
        set i position (item 1 setIterAndTileNum) numTiles
        if i = false [set i (length(numTiles) + 1)]
        set runComplete true
      ]

      while [i < length(numTiles)] [
        set tileNum item i numTiles
        clear-patches
        killOldAnimals
        loadExtent tileNum
        readStartingLocations

        if count transients > 0 or count residents > 0 [
          readNewTiles tileNum
          setupLandscape
          setupSpecies ; set time in unsuitable habitat and initial heading
          move
          findNumIndividualsInEachCell
        ]
        storeCompletedIterAndTileNums
        set i (i + 1)

      ]
      set iterationNum (iterationNum + 1)
    ]
  ]

end

to deleteIncompleteFiles
  let iterationNumber 0
  let tileNumInFile 0
  let result []
  let rowsCopied 0

  set-current-directory (word outputDirectory Species "/")
  if file-exists? "agentLocations.csv"
  [
    file-open "agentLocations.csv"
    while [not file-at-end?][
      set result csv:from-row file-read-line
      set iterationNumber item 13 result
      set tileNumInFile item 4 result
      if (iterationNumber < iterationNum) and (tileNumInFile >= first numTiles) and (tileNumInFile <= last numTiles)[
        file-open "tempAL.csv" ;temporary agent locations
        file-print (word item 0 result "," item 1 result "," item 2 result "," item 3 result "," item 4 result "," item 5 result "," item 6 result "," item 7 result "," item 8 result "," item 9 result "," item 10 result "," item 11 result "," item 12 result "," item 13 result)
        file-close
        set rowsCopied (rowsCopied + 1)
      ]
      if (iterationNumber = iterationNum and tileNumInFile < tileNum)[
         file-open "tempAL.csv"
        file-print (word item 0 result "," item 1 result "," item 2 result "," item 3 result "," item 4 result "," item 5 result "," item 6 result "," item 7 result "," item 8 result "," item 9 result "," item 10 result "," item 11 result "," item 12 result "," item 13 result)
        file-close
        set rowsCopied (rowsCopied + 1)
      ]
      file-open "agentLocations.csv"
    ]
    file-close-all

    file-delete "agentLocations.csv"
    set rowsCopied 0

    if file-exists? "tempAL.csv"[
      file-open "tempAL.csv"
      while [not file-at-end?][
        set result csv:from-row file-read-line
        file-open "agentLocations.csv"
        file-print (word item 0 result "," item 1 result "," item 2 result "," item 3 result "," item 4 result "," item 5 result "," item 6 result "," item 7 result "," item 8 result "," item 9 result "," item 10 result "," item 11 result "," item 12 result "," item 13 result)
          set rowsCopied (rowsCopied + 1)
        file-close
        file-open "tempAL.csv"
      ]
      file-close
      file-delete "tempAL.csv"
    ]
  ]

  if file-exists? (word "transientStatus_" iterationNum "_" tileNum ".csv") [file-delete (word "transientStatus_" iterationNum "_" tileNum ".csv")]
  if file-exists? (word "residentStatus_" iterationNum "_" tileNum ".csv") [file-delete (word "residentStatus_" iterationNum "_" tileNum ".csv")]
  if file-exists? (word "movementCoordinates_" iterationNum "_" tileNum ".csv") [
    show (word "deleting file: movementCoordinates_" iterationNum "_" tileNum ".csv")
    file-delete (word "movementCoordinates_" iterationNum "_" tileNum ".csv")
  ]

  set-current-directory (word outputDirectory Species "/" "Iteration_" iterationNum "/")
  if file-exists? (word Species "output_numCrossing_Itn" iterationNum "_Tile" tileNum) [
      show (word "deleting file: output_numCrossing_Itn" iterationNum "_" tileNum ".csv")
    file-delete (word Species "output_numCrossing_Itn" iterationNum "_Tile" tileNum)
  ]
  if file-exists? (word Species "output_numIndividuals_Itn" iterationNum "_Tile" tileNum)[
    show (word "deleting file: output_numIndividuals_Itn" iterationNum "_" tileNum ".csv")
    file-delete (word Species "output_numIndividuals_Itn" iterationNum "_Tile" tileNum)
  ]
  set-current-directory (word outputDirectory Species "/")
end

to-report checkIfPreviousRunIsComplete
  set-current-directory (word outputDirectory Species "/")
  ifelse file-exists? (word completedIterAndTileNumFileName ".csv")
  [report false][report true] ;false indicates previous runs are not complete
end

to-report reportStartingTile []
  let stPtLocations []
  file-open (word startingPtTilesLocation Species "/" "Iteration_" iterationNum "/" "stPttileNum.csv")
  while[not file-at-end?][
    set stPtLocations lput (item 0 csv:from-row file-read-line) stPtLocations
  ]

  let startingTileNum item 0 stPtLocations
  file-close
  report startingTileNum
end

to-report setIterAndTileNum
  set-current-directory (word outputDirectory Species "/")
  file-open (word completedIterAndTileNumFileName ".csv")
  let nextTileNum 0
  let nextIteration 0
  let x []

   while[not file-at-end?][
    set x csv:from-row file-read-line
  ]

  let  lastIterationNum (item 0 x)
  let lastTileNum (item 1 x)
  file-close
  ifelse lastTileNum = (last numTiles)[set nextTileNum first numTiles set nextIteration (lastIterationNum + 1)]
  [set nextTileNum item ((position lastTileNum numTiles) + 1) numTiles set nextIteration lastIterationNum]
  if nextTileNum = false [set nextTileNum lastTileNum]
  report (list nextIteration nextTileNum)
end

to reInitGlobals
  ask patches [set numIndividualsList [] set numCrossing 0 set numIndividualsCrossed [] ]
end

to killOldAnimals
  if count residents > 0 [ask residents[ die] ]
  if count transients > 0 [ask transients[ die] ]
  if count settledTransients > 0 [ask settledTransients[ die]]
end

to loadExtent [i]
   set-current-directory rasterTilesLocation
  set lulcLayer gis:load-dataset (word "SplitRas_nullLulc" i ".asc") ;--asc file is loaded
  resize-world 0 gis:width-of lulcLayer - 1 0 gis:height-of lulcLayer - 1
  gis:set-world-envelope gis:envelope-of lulcLayer
end

to readNewTiles [i]
  set-current-directory rasterTilesLocation
  gis:apply-raster lulcLayer lulc

   if file-exists? (word "SplitRas_slope" i ".asc") [
    let slopeLayer gis:load-dataset (word "SplitRas_slope" i ".asc")
    gis:apply-raster slopeLayer tpi
  ]

   if file-exists? (word "SplitRas_dem" i ".asc") [
    let demLayer gis:load-dataset (word "SplitRas_dem" i ".asc")
    gis:apply-raster demLayer dem
  ]

  if file-exists? (word "SplitRas_boundaryCells" i ".asc")[
    ;show "adding boundary cells"
    let boundaryCellsLayer gis:load-dataset (word "SplitRas_boundaryCells" i ".asc")
    gis:apply-raster boundaryCellsLayer border
  ]

  set-current-directory vectorTilesLocation

  if file-exists? (word "RiverBridges/" "RiverBridgesDiff" i ".shp")[
    let riverBridges gis:load-dataset (word "RiverBridges/" "RiverBridgesDiff" i ".shp")
    foreach gis:feature-list-of riverBridges[ x ->
      ask patches gis:intersecting x[
        set riverBridge 1
      ]
    ]
  ]
end

to readStartingLocations
  set-current-directory (word startingPtTilesLocation Species "/" "Iteration_" iterationNum "/")
  carefully [
    file-open (word "startPoint_Tile_" tileNum ".csv")
    let location []
    let netlogo-x 0
    let netlogo-y 0
    let fileId ""
    while[not file-at-end?][
      let row csv:from-row file-read-line
      let x item 0 row
      let y item 1 row

      set location (checkIfWithinTile x y)

      ifelse not empty? location[
        set netlogo-x item 0 location
        set netlogo-y item 1 location
        let transientOrAdult item 2 row
        set fileId item 3 row
        ifelse transientOrAdult = "T"[
          let temp patch netlogo-x netlogo-y
          let newPatchFound true
          ask patch netlogo-x netlogo-y[
            sprout-transients 1[
              set size 2.5 set color black
              set id (word fileId "_" tileNum)
            ]
          ]
        ]
        [
          let newPatchFound true
          ask patch netlogo-x netlogo-y[
            sprout-residents 1[
              set size 2.5 set color black
              set id (word fileId "_" tileNum)
            ]
          ]
        ]
      ]
      [
        show "no starting locations in this tile"
      ]
    ]
    file-close
  ]
  [
   show (word startingPtTilesLocation "Iteration_" iterationNum "/" "startPoint_Tile_" tileNum ".csv" "does not exist")
  ]
end

to-report checkIfWithinTile [x y]
  let envelope gis:world-envelope
  let xscale (max-pxcor - min-pxcor) / (item 1 envelope - item 0 envelope)
  let yscale (max-pycor - min-pycor) / (item 3 envelope - item 2 envelope)

  ifelse x >= item 0 envelope and x <= item 1 envelope and y >= item 2 envelope and y <= item 3 envelope[
    let netlogo-x (x - item 0 envelope) * xscale  + min-pxcor
    let netlogo-y (y - item 2 envelope) * yscale + min-pycor
    report list netlogo-x netlogo-y
  ]
  [
    report []
  ]
end


to setupLandscape
ask patches[
    set numIndividualsList []

    cf:when
    cf:case [lulc = 1][set pcolor red] ;builtup
    cf:case [lulc = 2][set pcolor yellow];agriculture
    cf:case [lulc = 3][set pcolor green];plantation
    cf:case [lulc = 4][set pcolor 62];forest
    cf:case [lulc = 5][set pcolor 42];scrubland
    cf:case [lulc = 6][set pcolor cyan];littoral
    cf:case [lulc = 7][set pcolor 34];grassland
    cf:case[lulc = 8][set pcolor grey];barren
    cf:case [lulc < 0][set pcolor brown set lulc 10]; outside WG
    cf:case [not((lulc <= 0) or (lulc >= 0))] [set pcolor black] ; outside boundary
    cf:else [set pcolor blue];waterbody, lulc = 9
  ]
end

to setupSpecies
  ask transients[
    set heading (random 8) * 45
    set dispDist randExpDist
    set transientHomeRangeRadius (precision sqrt ((((random-float (maxHomeRange - minHomeRange)) + minHomeRange) * 10000) / 3.14) 2)
    set timeInUnsuitableHabitat percDispDist dispDist
  ]

  ask residents[
    set homeX xcor set homeY ycor
    set homeRangeRadius (precision sqrt ((((random-float (maxHomeRange - minHomeRange)) + minHomeRange) * 10000) / 3.14) 2)
    set heading (random 8) * 45
    set timeInUnsuitableHabitat (((.1 * homeRangeRadius) * 1000) / resolution)
  ]
end

to-report randExpDist
  let result abs (precision  (random-exponential ((((random-float (maxDispDist - minDispDist)) + minDispDist )* 1000) / resolution )) 2)
  if result < ((minDispDist * 1000) / resolution) or result > (((maxDispDist + 15) * 1000) / resolution)
  [report randExpDist]
  report result
end

to-report percDispDist[dispDistance]
  let result abs(precision random-exponential (0.2 * dispDistance) 2)
  if result < (0.2 * dispDistance) or result > (0.7 * dispDistance)
  [report percDispDist dispDistance]
  report result
end

to move
  repeat TimeSteps[
   moveTransients
   moveResidents
]
end

to moveTransients
  let foundCell false
  let storeTransientLocation false
  let transientAtBorder false
  let nextPatch nobody
  let movementProb 0
  let completeNeighborPool no-patches
  let noBarrierPool no-patches

  ask transients
    [
      let transId ([id] of self)

      ifelse (stepCount <= dispDist) and (unsuitableHabitatStep < timeInUnsuitableHabitat) [
        let neighborPool no-patches
        set nextPatch nobody
        set foundCell false
        set storeTransientLocation boundaryCheck
        set transientAtBorder borderCheck

        ifelse transientAtBorder = true[

          set stepCount dispDist + 1
          set unsuitableHabitatStep timeInUnsuitableHabitat + 1
          set agentsDiscarded (agentsDiscarded + 1)
          file-open "discardedAgents.csv"
          let env gis:envelope-of self
          file-print (word first env "," last env "," "Transient" tileNum)
          file-close
          die
        ]
        [
          ifelse storeTransientLocation = true[
            storeAgentLocationFn self "T"
            set stepCount (dispDist + 1) ;Kill transient
            set unsuitableHabitatStep (timeInUnsuitableHabitat + 1) ;Kill transient
            die
          ]
          [
            set completeNeighborPool neighbors
            set neighborPool neighbors in-cone 1.8 135 ; initially neighborPool constitutes neighbors in 180 deg cone
            set noBarrierPool findCellsWithNoBarriers neighborPool
            if noBarrierPool = no-patches [
              set noBarrierPool findCellsWithNoBarriers completeNeighborPool
              if noBarrierPool = no-patches [
                show "can't find suitabe cell, turning back" set heading 180
              ]
            ]
            moveForward noBarrierPool "T" self
           ]
        ]
      ]
      ;else block for stepCount and unsuitableHabitatStep check
      [
        ;DIE
        ifelse unsuitableHabitatStep >= timeInUnsuitableHabitat[
          set-current-directory (word outputDirectory Species "/")
          ifelse runsAfterIterations = false[
            file-open (word "transientStatus_" iterationNum "_" tileNum  ".csv")
            let env gis:envelope-of self
            file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," dispDist)
            file-close
          ]
          [
            file-open (word "transientStatus_afterN_" runNum "_" tileNum  ".csv")
            let env gis:envelope-of self
            file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," dispDist)
            file-close
          ]
          die
        ]
        [
          ;SETTLE
          ifelse ([lulc] of patch-here = 4) and (count other residents in-radius transientHomeRangeRadius = 0) and
          (count other settledTransients in-radius transienthomeRangeRadius = 0)
          [
            set-current-directory (word outputDirectory Species "/")

            ifelse runsAfterIterations = false[
              file-open (word "transientStatus_" iterationNum "_" tileNum  ".csv")
              let env gis:envelope-of self
              file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Settled" "," stepCount "," dispDist)
              file-close
            ]
            [
              file-open (word "transientStatus_afterN_" runNum "_" tileNum  ".csv")
              let env gis:envelope-of self
              file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Settled" "," stepCount "," dispDist)
              file-close
            ]

            let patchHere patch-here
            ask patchHere [sprout-settledTransients 1]
            die
          ]
          [
            ;DIE if no suitable habitat to settle
            set-current-directory (word outputDirectory Species "/")
            ifelse runsAfterIterations = false[
              file-open (word "transientStatus_" iterationNum "_" tileNum  ".csv")
              let env gis:envelope-of self
              file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," dispDist)
              file-close
            ]
            [
              file-open (word "transientStatus_afterN_" runNum "_" tileNum  ".csv")
              let env gis:envelope-of self
              file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," dispDist)
              file-close
            ]
            die
          ]
        ]
      ]
  ]

end

to-report moveProb [linearInfrastructure linearInfrastructureValue]
  if linearInfrastructure = "RiverBridge"[
    cf:when
    cf:case [linearInfrastructureValue = 1][report 0]
    cf:else [report 1]; bridge exists
  ]
  if linearInfrastructure = "TreeCover"[
    cf:when
    cf:case [linearInfrastructureValue < table:get moveProbTable "TreeCoverMin"][
      ifelse linearInfrastructureValue <= 0 [
        report table:get moveProbTable 8
      ]
      [
        let prob abs(0.3 - ((table:get moveProbTable "TreeCoverMin") - linearInfrastructureValue) / 100 )

        if prob >= 1 [set prob 0.9]
        report prob
      ]
    ]
    cf:else [report 1]
  ]

  if linearInfrastructure = "slope"[
    cf:when
    cf:case [linearInfrastructureValue <= table:get moveProbTable "SlopeMax"][report 1]
    cf:else [
      let prob (1 / ((linearInfrastructureValue - table:get moveProbTable "SlopeMax")))
      if prob >= 1 [set prob 0.9]
      report prob
    ]
  ]
  if linearInfrastructure = "dem" [
    cf:when
    cf:case [linearInfrastructureValue <= table:get moveProbTable "ElevMax"][report 1]
    cf:else [
      let prob (1 / ((linearInfrastructureValue - table:get moveProbTable "ElevMax")))
      if prob >= 1 [set prob 0.9]
      report prob
    ]
  ]

end


to-report findCellsWithNoBarriers [neighborPool]
  let noBarrierPool neighborPool
  let sampledPatch nobody
  let removeCell false

  ask neighborPool[
    set removeCell false
    set sampledPatch self

    ifelse (not((lulc <= 0) or (lulc >= 0 ))) [
      set removeCell true
    ]

    [
      ifelse lulc = 10 [
        set removeCell true
      ]
      [
        if member? 0 (list (moveProb "RiverBridge" riverBridge) )  or (not((lulc <= 0) or (lulc >= 0 )))[
          set removeCell true
        ]
      ]
    ]
    if removeCell = true [ask sampledPatch [set noBarrierPool other noBarrierPool]]
  ]
  report noBarrierPool
end

to-report boundaryCheck []
  let yCoord [pycor] of patch-here let xCoord [pxcor] of patch-here
  ifelse (any? neighbors with [ not((lulc <= 0) or (lulc >= 0)) or (lulc = 0 )] ) or (yCoord = max-pycor or yCoord = min-pycor or xCoord = max-pxcor or xCoord = min-pxcor)
  [
    report true
  ]
  [
    report false
  ]
end

to-report borderCheck
  ifelse (any? neighbors with [lulc = 10 or border = 1])[report true][report false]
end

to moveForward [noBarrierPool transientOrAdult agent]
  let movementProb 0
  let movedToNextCell true
  let agentId [id] of agent
  let agentStepCount [stepCount] of agent
  let fullProduct false

  ask noBarrierPool[ set movementProbability  ((precision (moveProb "slope" tpi) 2 )  *  (precision (moveProb "dem" dem) 2 ))];show word "movementProb" movementProbability]

  let nextPatch rnd:weighted-one-of noBarrierPool [ movementProbability ]
  if nextPatch = nobody[
    set nextPatch one-of noBarrierPool with-max [movementProbability]
  ]

  carefully [face nextPatch][show (word "did not find a next cell" self " " error-message)]
  carefully[
    move-to nextPatch
    ask agent[set stepCount (stepCount + 1) set agentStepCount stepCount]
    ask nextPatch [
      set numCrossing (numCrossing + 1)
      if not member? agentId numIndividualsList[
        set numIndividualsList lput agentId numIndividualsList
      ]
      set-current-directory (word outputDirectory Species "/")
      ifelse runsAfterIterations = false[
        file-open (word "movementCoordinates_" iterationNum "_" tileNum ".csv")
        let env gis:envelope-of agent
        file-print (word first env "," last env "," agentId "," tileNum "," iterationNum "," runNum "," agentStepCount)
        file-close
      ]
      [
        file-open (word "movementCoordinates_afterN_" runNum "_" tileNum ".csv")
        let env gis:envelope-of agent
        file-print (word first env "," last env "," agentId "," tileNum "," iterationNum "," runNum "," agentStepCount)
        file-close
      ]
    ]
    ifelse transientOrAdult = "T"[
      ask agent[
        setTimeInUnsuitableHabitat "T"
      ]
    ]
    [
      setTimeInUnsuitableHabitat "A"
    ]
  ]
  [
    show (word "did not find next cell" self " " error-message)
  ]
end

to storeAgentLocationFn [agentToStore transientOrAdult]
  set-current-directory (word outputDirectory Species "/")
  file-open "agentLocations.csv"
  let env gis:envelope-of agentToStore

  ifelse transientOrAdult = "T" [
    file-print (word id "," first env "," last env "," runNum "," tileNum "," unsuitableHabitatStep "," stepCount "," transientOrAdult "," heading "," 0 "," 0 "," 0 "," dispDist "," iterationNum)
  ]
  [
    file-print (word id "," first env "," last env "," runNum "," tileNum "," unsuitableHabitatStep "," stepCount "," transientOrAdult "," heading "," homeX "," homeY "," 0 "," homeRangeRadius "," iterationNum)
  ]
  file-close
end

to setTimeInUnsuitableHabitat [transientOrAdult]
  let movementProb [movementProbability] of patch-here
   ifelse movementProb > 0.5 [
     set unsuitableHabitatStep unsuitableHabitatStep + 0
   ]
  [
    ifelse movementProb != 0[
      let increment ceiling ((1 / movementProb))
      if increment > 15 [set increment 20]
      set unsuitableHabitatStep unsuitableHabitatStep + increment
    ]
    [
      set unsuitableHabitatStep unsuitableHabitatStep + 20
    ]
  ]
end

to moveResidents
  let nextPatch nobody
  let movementProb 0
  let noBarrierPool no-patches
  let neighborPool no-patches
  let adultAtBorder false
  let storeAdultLocation false
  let agentAtBorderNotFacingIt false

   ask residents
   [
    ifelse stepCount < TimeSteps[
      set nextPatch nobody
      set neighborPool no-patches
      let ResId [id] of self
      set storeAdultLocation boundaryCheck ; boundary of tiles
      set adultAtBorder borderCheck

      ifelse adultAtBorder = true[
        set agentsDiscarded (agentsDiscarded + 1)
        file-open "discardedAgents.csv"
        let env gis:envelope-of self
        file-print (word first env "," last env "," "Adult" tileNum)
        file-close
        die
      ]
      [
        ifelse storeAdultLocation = true
        [
          storeAgentLocationFn self "A"
          die
        ]
        [
          set neighborPool neighbors
          set noBarrierPool findCellsWithNoBarriers neighborPool
          if noBarrierPool = no-patches[
            show "can't find suitabe cell"
          ]
          moveForward noBarrierPool "A" self

          if unsuitableHabitatStep > timeInUnsuitableHabitat [
            ifelse random-float 1 < 0.7 [
              set heading 180 set unsuitableHabitatStep 0
            ]
            [
              set-current-directory (word outputDirectory Species "/")
              ifelse runsAfterIterations = false[
                file-open (word "residentStatus_" iterationNum "_" tileNum  ".csv")
                let env gis:envelope-of self
                file-print (word first env "," last env "," ResId "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," homeRangeRadius)
                file-close
              ]
              [
                file-open (word "residentStatus__afterN_" runNum "_" tileNum  ".csv")
                let env gis:envelope-of self
                file-print (word first env "," last env "," ResId "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," homeRangeRadius)
                file-close
              ]

              set stepCount TimeSteps + 1 die
            ]
          ]
          if distancexy homeX homeY > homeRangeRadius[facexy homeX homeY]
        ]
      ]
    ]
    ;STEPCOUNT > TIMESTEPS
    [
      set-current-directory (word outputDirectory Species "/")
      ifelse runsAfterIterations = false[
        file-open (word "residentStatus_" iterationNum "_" tileNum  ".csv")
        let env gis:envelope-of self
        file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," homeRangeRadius)
        file-close
      ]
      [
        file-open (word "residentStatus__afterN_" runNum "_" tileNum  ".csv")
        let env gis:envelope-of self
        file-print (word first env "," last env "," id "," iterationNum  "," runNum "," tileNum "," "Dead" "," stepCount "," homeRangeRadius)
        file-close
      ]
      die
    ]

  ]
end

to-report checkIfAgentIsAtBorderButNotFacingIt
  let xCoord [xcor] of self let yCoord [ycor] of self
  ifelse ycor = max-pycor or ycor = min-pycor or xcor = max-pxcor or xcor = min-pxcor[
   ifelse any? neighbors with [(abs(pycor - yCoord) > 1) or (abs(pxcor - xCoord) > 1)][
      report true
    ]
    [
      report false
    ]
  ]
  [
    report false
  ]

end

to handleSpilledOverAgents

  let runsComplete true
  let previousTileNum 0
  let previousRunNum 0
  let i 0
  set runsAfterIterations true

  show "ITERATIONS COMPLETED, handling agents that spilled out of tiles"

  set-current-directory (word outputDirectory Species "/")

  if file-exists? "completedTileNums.csv" [set runsComplete false show "RESUMING model runs after n iterations" ]


  ifelse runsComplete = false[
    let lastRunAndTileNum readLastTileNum
    let lastTileNum item 1 lastRunAndTileNum
    set runNum item 0 lastRunAndTileNum
    set i position lastTileNum numTiles
    set tileNum item i numTiles
  ]
  [
    set i 0
    set runNum 1
  ]

  set tileNum item i numTiles

  if file-exists? "agentLocations.csv" = true [copytoTempFile]

  if runsComplete = true [deleteFirstTileFiles runsComplete]

  deleteIncompleteAgentLocAndCSVFiles

  clear-patches
  killOldAnimals
  show (word " starting at runNumber " runNum " tileNumber " tileNum)

  if file-exists? "agentLocations.csv" = true [copyToFileAndDelete "agentLocations.csv" (word "agentLocations" runNum "_" tileNum ".csv") runsComplete]

  set runsComplete true

  while [(file-exists? "agentLocations.csv" = true  or file-exists? (word "agentLocations" runNum "_" tileNum ".csv") = true)][
    while [i < length(numTiles)][
      clear-patches
      killOldAnimals
      set tileNum item i numTiles
      show (word "RUNNUM " runNum " TILENUM " tileNum)
      if file-exists? "agentLocations.csv" = true [copyToFileAndDelete "agentLocations.csv" (word "agentLocations" runNum "_" tileNum ".csv") runsComplete]
      loadExtent tileNum
      loadLocationsFromPreviousTile

      if count transients > 0 or count residents > 0 [
        readNewTiles tileNum
        setupLandscape
        moveAfterIterations
        findNumIndividualsInEachCell
      ]
      set i (i + 1)
      storeCompletedTileNums
    ]
    set-current-directory (word outputDirectory Species "/")
    set runNum (runNum + 1)
    set tileNum first numTiles
    set i 0
  ]; end of file-exists
  show "model complete"
end

to deleteFirstTileFiles [runsComplete]
  show "in delete first tile files"
  set-current-directory (word outputDirectory Species "/")
  ifelse runsComplete = true and file-exists? (word "agentLocations" runNum "_" tileNum ".csv") [
    show (word "deleting file agentLocations" runNum "_" tileNum ".csv")
    file-delete (word "agentLocations" runNum "_" tileNum ".csv")
    if file-exists? "agentLocations.csv"[
      show "deleting file agentLocations.csv"
      file-delete "agentLocations.csv"
    ]

    ;copy agentLocationsCOPY to agentLocations runNum tileNum
    ifelse file-exists? (word "agentLocationsCOPY" runNum "_" tileNum ".csv")[
      file-open (word "agentLocationsCOPY" runNum "_" tileNum ".csv")
      let result []

      while [not file-at-end?][
        set result csv:from-row file-read-line
        file-open (word "agentLocations" runNum "_" tileNum ".csv")
        file-print (word item 0 result "," item 1 result "," item 2 result "," item 3 result "," item 4 result "," item 5 result "," item 6 result "," item 7 result "," item 8 result "," item 9 result "," item 10 result "," item 11 result "," item 12 result "," item 13 result)
        file-close
        file-open (word "agentLocationsCOPY" runNum "_" tileNum ".csv")
      ]
      file-close
    ]
    [
      show "agentLocationsCOPY does not exist!"
    ]
  ]
  [
    show (word "agentLocations" runNum "_" tileNum ".csv does not exist")
  ]
end

to deleteIncompleteAgentLocAndCSVFiles
  let nextTileNum 0
  let nextRunNum 0
  let nextNextTileNum 0
  let nextNextRunNum 0

  set-current-directory (word outputDirectory Species "/")
  if file-exists? (word "movementCoordinates_afterN_" runNum "_" tileNum ".csv")[
    show (word "deleting movementCoordinates_afterN_" runNum "_" tileNum ".csv")
    file-delete (word "movementCoordinates_afterN_" runNum "_" tileNum ".csv")
  ]
  if file-exists? (word "transientStatus_afterN_" runNum "_" nextTileNum ".csv")[file-delete (word "transientStatus_afterN_" runNum "_" nextTileNum ".csv")]
  if file-exists? (word "residentStatus_afterN_" runNum "_" nextTileNum ".csv")[file-delete (word "residentStatus_afterN_" runNum "_" nextTileNum ".csv")]

  set-current-directory (word outputDirectory Species "/" "Iteration_" iterationNum "/")
  if file-exists? (word Species "output_numCrossing_Itn" iterationNum "_" runNum "_Tile" tileNum) [
      show (word "deleting output_numCrossing_Itn" iterationNum "_" runNum "_Tile" tileNum)
    file-delete (word Species "output_numCrossing_Itn" iterationNum "_" runNum "_Tile" tileNum)
  ]
  if file-exists? (word Species "output_numIndividuals_Itn" iterationNum "_" runNum "_Tile" tileNum) [
      show (word "deleting output_numIndividuals_Itn" iterationNum "_" runNum "_Tile" tileNum)
    file-delete (word Species "output_numIndividuals_Itn" iterationNum "_" runNum "_Tile" tileNum)
  ]

  set-current-directory (word outputDirectory Species "/")

  ifelse tileNum = last numTiles [
    set nextTileNum first numTiles
    set nextRunNum (runNum + 1)

    set nextNextTileNum item ((position nextTileNum numTiles) + 1) numTiles
    set nextNextRunNum nextRunNum

  ]
  [
    set nextTileNum item ((position tileNum numTiles) + 1) numTiles
    set nextRunNum runNum

    ifelse nextTileNum = last numTiles[
      set nextNextTileNum first numTiles set nextNextRunNum (nextRunNum + 1)
    ]
    [
      set nextNextTileNum item ((position nextTileNum numTiles) + 1) numTiles
      set nextNextRunNum nextRunNum
    ]
  ]

  if file-exists? (word "agentLocations" nextRunNum "_" nextTileNum ".csv") [
    show (word "deleteing agentLocations" nextRunNum "_" nextTileNum ".csv")
    file-delete (word "agentLocations" nextRunNum "_" nextTileNum ".csv")
  ]
  if file-exists? (word "agentLocations" nextNextRunNum "_" nextNextTileNum ".csv") [
      show (word "deleteing agentLocations" nextNextRunNum "_" nextNextTileNum ".csv")
    file-delete (word "agentLocations" nextNextRunNum "_" nextNextTileNum ".csv")
  ]
  if file-exists? (word "movementCoordinates_afterN_" nextRunNum "_" nextTileNum ".csv")[
      show (word "deleteing movementCoordinates after N" nextRunNum "_" nextTileNum ".csv")
    file-delete (word "movementCoordinates_afterN_" nextRunNum "_" nextTileNum ".csv")
  ]
  if file-exists? (word "movementCoordinates_afterN_" nextNextRunNum "_" nextNextTileNum ".csv")[
      show (word "deleteing movementCoordinates after N" nextNextRunNum "_" nextNextTileNum ".csv")
    file-delete (word "movementCoordinates_afterN_" nextNextRunNum "_" nextNextTileNum ".csv")
  ]

  if file-exists? (word "transientStatus_afterN_" nextRunNum "_" nextTileNum ".csv")[
    file-delete (word "transientStatus_afterN_" nextRunNum "_" nextTileNum ".csv")
  ]
  if file-exists? (word "transientStatus_afterN_" nextNextRunNum "_" nextNextTileNum ".csv")[
    file-delete (word "transientStatus_afterN_" nextNextRunNum "_" nextNextTileNum ".csv")
  ]
  if file-exists? (word "residentStatus_afterN_" nextRunNum "_" nextTileNum ".csv")[
    file-delete (word "residentStatus_afterN_" nextRunNum "_" nextTileNum ".csv")
  ]
  if file-exists? (word "residentStatus_afterN_" nextNextRunNum "_" nextNextTileNum ".csv")[
    file-delete (word "residentStatus_afterN_" nextNextRunNum "_" nextNextTileNum ".csv")
  ]

end

to copyToTempFile []
  set-current-directory (word outputDirectory Species "/")
  if not file-exists? (word "agentLocationsCOPY" runNum "_" tileNum ".csv")[
    file-open "agentLocations.csv"
    let result []


    while [not file-at-end?][
      set result csv:from-row file-read-line
      file-open (word "agentLocationsCOPY" runNum "_" tileNum ".csv")
      file-print (word item 0 result "," item 1 result "," item 2 result "," item 3 result "," item 4 result "," item 5 result "," item 6 result "," item 7 result "," item 8 result "," item 9 result "," item 10 result "," item 11 result "," item 12 result "," item 13 result)
      file-close
      file-open "agentLocations.csv"
    ]
    file-close
  ]
end

to copyToFileAndDelete [fromFile toFile runsComplete]
  show (word "agentLocations.csv exists, copying to agentLocations" runNum "_"tileNum ".csv")
  set-current-directory (word outputDirectory Species "/")
  file-open fromFile
  let result []
  let runNumInFile 0
  let tileNumInFile 0
  let rowsCopied  0

  while [not file-at-end?][
    set result csv:from-row file-read-line
    set runNumInFile item 3 result
      set tileNumInFile item 4 result
      if (runNumInFile < runNum) and (tileNumInFile >= first numTiles) and (tileNumInFile <= last numTiles)[
        file-open toFile
        file-print (word item 0 result "," item 1 result "," item 2 result "," item 3 result "," item 4 result "," item 5 result "," item 6 result "," item 7 result "," item 8 result "," item 9 result "," item 10 result "," item 11 result "," item 12 result "," item 13 result)
        file-close
        set rowsCopied (rowsCopied + 1)
      ]
      if (runNumInFile = runNum and tileNumInFile < tileNum)[
         file-open toFile
        file-print (word item 0 result "," item 1 result "," item 2 result "," item 3 result "," item 4 result "," item 5 result "," item 6 result "," item 7 result "," item 8 result "," item 9 result "," item 10 result "," item 11 result "," item 12 result "," item 13 result)
        file-close
        set rowsCopied (rowsCopied + 1)
      ]
      file-open fromFile
  ]
  file-close
  show (word "rows moved from agentLocations to agentLocations " runNum " " tileNum " : " rowsCopied)
  if file-exists? fromFile [file-delete fromFile]
  if file-exists? "agentLocations.csv" [show "in copyToFileAndDelete fn, agentLocations still exists!"]
end

to-report readLastTileNum
  file-open "completedTileNums.csv"
  let x []

  while [not file-at-end?][
    set x csv:from-row file-read-line
  ]

  let  lastRunNum (item 0 x)
  let lastTileNum (item 1 x)
  file-close
  report list lastRunNum lastTileNum
end

to loadLocationsFromPreviousTile []
  let location []
  let netlogo-x 0
  let netlogo-y 0
  let transientOrAdult "T"
  ;let previousLocationsExist false
  let tile 0
  let numPasses 0
  let numRowsStored 0

 set-current-directory (word outputDirectory Species "/")
  if file-exists? (word "agentLocations" runNum "_" tileNum ".csv") = true [
    file-open (word "agentLocations" runNum "_" tileNum ".csv")

    while [not file-at-end? ][
      let row csv:from-row file-read-line
      let x item 1 row
      let y item 2 row
      set tile item 4 row
      set numPasses item 11 row
      let agentAtBoundary false

      set location checkIfWithinTile x y
      ifelse (not empty? location) and (tile != tileNum) [
        set netlogo-x (item 0 location)
        set netlogo-y (item 1 location)

        let temp patch netlogo-x netlogo-y
        ask patch netlogo-x netlogo-y
          [
            set transientOrAdult item 7 row
            ifelse transientOrAdult = "T"[
              sprout-transients 1[
              set size 2.5 set color black
              set id item 0 row
              set heading item 8 row
              set unsuitableHabitatStep item 5 row
              set stepCount item 6 row
              set dispDist item 12 row
              set timeInUnsuitableHabitat percDispDist dispDist
              set transientHomeRangeRadius (precision sqrt ((((random-float (maxHomeRange - minHomeRange)) + minHomeRange) * 10000) / 3.14) 2)
            ]
          ]
          [
            sprout-residents 1[
              set size 2.5 set color black
              set id item 0 row
              set heading item 8 row
              set homeX item 9 row
              set homeY item 10 row
              set unsuitableHabitatStep item 5 row
              set stepCount item 6 row
              set homeRangeRadius item 12 row
              set timeInUnsuitableHabitat (((.1 * homeRangeRadius) * 1000) / resolution)
            ]
          ]
        ]
      ]
      [
        if tile = tileNum [set numPasses (numPasses + 1)]
        ifelse numPasses < 2 [
          storeInNextFile row numPasses
          set numRowsStored (numRowsStored + 1)
        ]
        [
          show (word "looping over tile " tile " more than once, discarding this location " (ceiling item 0 location) (floor item 1 location))
          set agentsDiscarded (agentsDiscarded + 1)
          file-open "discardedAgents.csv"

          file-print (word x "," y "," "Transient" tileNum "," "looped more than once")
          file-close
        ]
      ]

      file-open (word "agentLocations" runNum "_" tileNum ".csv")
    ]

    file-close
  ]

  file-close-all
  show (word "rows for next agentLocations file: " numRowsStored)
end

to storeInNextFile [row numPasses]
  let nextTileNum 0
  let runNumber 0

  ifelse tileNum = last numTiles [
    set nextTileNum first numTiles set runNumber runNum + 1
  ]
  [
    set nextTileNum item ((position tileNum numTiles) + 1) numTiles set runNumber runNum
  ]

  set-current-directory (word outputDirectory Species "/")
  file-open (word "agentLocations" runNumber "_" nextTileNum ".csv")
  file-print (word item 0 row "," item 1 row "," item 2 row "," item 3 row "," item 4 row "," item 5 row "," item 6 row "," item 7 row "," item 8 row "," item 9 row "," item 10 row "," numPasses "," item 12 row "," item 13 row)
  file-close
end

to moveAfterIterations
  ask residents[ while [stepCount < TimeSteps] [
    moveResidents
    ]
  ]
  ask transients[
    while [stepCount < TimeSteps][
      moveTransients
    ]
  ]

end

to storeCompletedIterAndTileNums []
  set-current-directory (word outputDirectory  Species "/")
  file-open (word completedIterAndTileNumFileName ".csv")
  file-print (word iterationNum "," tileNum)
  file-close
end

to findNumIndividualsInEachCell
  ask patches[set numIndividualsCrossed length numIndividualsList]
end

to deleteOutputCSVs
  set-current-directory (word outputDirectory  Species "/")
  if file-exists? "completedTileNums.csv" [file-delete "completedTileNums.csv"]
end

to storeCompletedTileNums
  set-current-directory (word outputDirectory  Species "/")
  file-open "completedTileNums.csv"
  file-print (word runNum "," tileNum)
  file-close
end

to cratemodelCompleteTxt
  set-current-directory (word outputDirectory  Species "/")
  file-open "modelComplete.txt"
  file-print "done"
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
3
30
1369
1155
-1
-1
6.724
1
10
1
1
1
0
0
0
1
0
201
0
165
0
0
1
ticks
30.0

INPUTBOX
1390
150
1472
210
TimeSteps
300.0
1
0
Number

BUTTON
1394
100
1461
133
Start
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
2109
165
2397
225
movementProbFile
path/to/data/Sloth bear_moveprob.csv
1
0
String

INPUTBOX
1482
151
1569
211
numIterations
100.0
1
0
Number

INPUTBOX
1803
36
2088
96
numTilesFile
path/to/tileNum lulc .csv
1
0
String (reporter)

INPUTBOX
1801
100
2089
160
rasterTilesLocation
path/to/RasterTiles/
1
0
String

INPUTBOX
1799
168
2090
228
outputDirectory
path/to/outputDirectory/
1
0
String

INPUTBOX
2110
36
2390
96
startingPtTilesLocation
path/to/StartingPointTiles/Sloth bear/
1
0
String

INPUTBOX
2111
98
2393
158
vectorTilesLocation
path/to/VectorTiles/
1
0
String

INPUTBOX
1391
457
1582
517
Species
Sloth bear
1
0
String

MONITOR
1392
35
1494
80
currentIteration
iterationNum
17
1
11

MONITOR
1508
34
1582
79
TileNumber
tileNum
17
1
11

INPUTBOX
1799
242
1883
302
minDispDist
4.95
1
0
Number

INPUTBOX
1896
242
1981
302
maxDispDist
33.98
1
0
Number

INPUTBOX
1796
325
1884
385
minHomeRange
2500.0
1
0
Number

INPUTBOX
1897
323
1981
383
maxHomeRange
10000.0
1
0
Number

INPUTBOX
1390
221
1653
281
completedIterAndTileNumFileName
CompletedIterAndTileNum
1
0
String

MONITOR
1591
34
1756
79
runNum (after n iterations)
runNum
17
1
11

INPUTBOX
1389
298
1654
358
projectionFile
path/to/projectionfile.prj
1
0
String

INPUTBOX
1392
379
1486
439
hpopM
-0.15002
1
0
Number

INPUTBOX
1495
378
1579
438
hpopC
1.69549
1
0
Number

INPUTBOX
1579
152
1653
212
resolution
220.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <enumeratedValueSet variable="numIterations">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minHomeRange">
      <value value="1240"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxHomeRange">
      <value value="8500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="outputDirectory">
      <value value="&quot;/media/data/Netlogo model/Western Ghats/Outputs/Sloth bear/1to25/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movementProbFile">
      <value value="&quot;/home/feral/Documents/Netlogo model Dec 17/MovementProbabilities/New_June/Sloth bear_moveprob.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="projectionFile">
      <value value="&quot;/home/feral/Documents/Netlogo model Dec 17/Western Ghats/Inputs/VectorTiles/NewTilesWithBuffer/Roads/Roads2.prj&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minDispDist">
      <value value="4.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rasterTilesLocation">
      <value value="&quot;/home/feral/Documents/Netlogo model Dec 17/Western Ghats/Inputs/RasterTiles/NewTilesWithBuffer/RasterTiles/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="completedIterAndTileNumFileName">
      <value value="&quot;CompletedIterAndTileNum&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingPtTilesLocation">
      <value value="&quot;/media/data/Netlogo model/Western Ghats/Inputs/StartingPointTiles/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TimeSteps">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species">
      <value value="&quot;Sloth bear&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxDispDist">
      <value value="33.98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numTilesFile">
      <value value="&quot;/home/feral/Documents/Netlogo model Dec 17/Western Ghats/Inputs/RasterTiles/NewTilesWithBuffer/RasterTiles/tileNum lulc .csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vectorTilesLocation">
      <value value="&quot;/home/feral/Documents/Netlogo model Dec 17/Western Ghats/Inputs/VectorTiles/NewTilesWithBuffer/&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
