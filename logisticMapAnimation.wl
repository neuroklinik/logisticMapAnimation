logisticMapImage[xLower_, xUpper_, yLower_, yUpper_, xResolution_, 
  yResolution_, dotCount_, maxIterations_, options___] :=
 Block[{lambda},
  ListPlot[
   Flatten[
    Table[
     Transpose[
      {Table[lambda, {dotCount + 1}],
       NestList[lambda # (1 - #) &, 
        Nest[lambda # (1 - #) &, 0.5, maxIterations], dotCount]}
      ],
     {lambda, xLower, xUpper, (xUpper - xLower)/xResolution}
     ], 1],
   ImageSize -> {xResolution, yResolution},
   AspectRatio -> yResolution/xResolution,
   PlotRange -> {yLower, yUpper},
   options]
  ]
  
  logisticMapAnimation[fileName_, xLS_, xUS_, xLE_, xUE_, yLS_, yUS_, 
  yLE_, yUE_, frameCount_,
  xResolution_, yResolution_, dotCount_, maxIterations_, options___] :=
  Block[{xLI, xUI, yLI, yUI, frameCoordinateList},
  If[! DirectoryQ[FileNameJoin[{$HomeDirectory, fileName}]], 
   CreateDirectory[FileNameJoin[{$HomeDirectory, fileName}]]];
  xLI = If[xLS == xLE, 0, (xLE - xLS)/frameCount];
  xUI = If[xUS == xUE, 0, (xUS - xUE)/frameCount];
  yLI = If[yLS == yLE, 0, (yLE - yLS)/frameCount];
  yUI = If[yUS == yUE, 0, (yUS - yUE)/frameCount];
  frameCoordinateList = 
   NestList[{#[[1]] + xLI, #[[2]] - xUI, #[[3]] + yLI, #[[4]] - 
       yUI} &, {xLS, xUS, yLS, yUS}, frameCount - 1];
  ParallelDo[
   Export[
    FileNameJoin[{$HomeDirectory, fileName, 
      fileName <> "_" <> ToString[i] <> ".png"}],
    logisticMapImage[frameCoordinateList[[i, 1]], 
     frameCoordinateList[[i, 2]], frameCoordinateList[[i, 3]], 
     frameCoordinateList[[i, 4]],
     xResolution, yResolution, dotCount, maxIterations, options]],
   {i, 1, frameCount}];
  Return["Render Complete!"]
  ]