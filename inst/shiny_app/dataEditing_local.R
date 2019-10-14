# --- set ID
assignID <- function(panelID,
                     imageName,
                     targetEnvir,
                     input,
                     rankTable)
{
  newid <- input[[paste0("textID",panelID)]]
  if(newid %in% c(""," "))
  {
    print("ignoring change")
  }else{
    targetEnvir$idData[imageName] <- newid
    names(newid) <- imageName
    if(length(newid)>1)stop("no multiname update")
    setData(directory=input$queryDirectory,column="id",image_val=newid)
    
    rankTable$editCount <- rankTable$editCount+1
  }
}


# --- set window to retrace
prepRetrace <- function(panelEnv,
                        panelID,
                        imageName,
                        rowName,
                        targetDir,
                        readyToRetrace)
{
  panelEnv[[panelID]]$mode <- "fix"
  
  readyToRetrace$imgName <- imageName
  readyToRetrace$rowName <- rowName
  readyToRetrace$panelID <- panelID
  readyToRetrace$directory <- targetDir
}

# --- restore defaults upon cancel
cancelRetrace <- function(panelEnv,
                          readyToRetrace,
                          targetEnvir,
                          traceGuideCounter)
{
  if(length(readyToRetrace$panelID)>0)
  {
    panelEnv[[readyToRetrace$panelID]]$mode <- "default"
    
    traceGuideCounter$count <- (-1)
    
    readyToRetrace$imgName <- NULL
    readyToRetrace$rowName <- NULL
    readyToRetrace$panelID <- NULL
    readyToRetrace$directory <- NULL
    readyToRetrace$traceResults <- list()
  }else{
    print("no changes")
    #panelEnv[[readyToRetrace$panelID]]$mode <- "default"
  }
}

# --- save trace edit and update tables
saveRetrace <- function(panelEnv,
                        readyToRetrace,
                        targetEnvir,
                        sessionReference,
                        mxnetModel,
                        rankTable,
                        traceGuideCounter)
{
  if(length(readyToRetrace$traceResults)>0)
  {
    print("prep trace")
    trace <- list( readyToRetrace$traceResults$coordinates )
    names(trace) <- readyToRetrace$imgName
    hash <- traceToHash( list(readyToRetrace$traceResults$annulus ), mxnetModel )
    names(hash) <- readyToRetrace$imgName
    print("retrace hash calculated")
    targetEnvir$settraceData(image_val=trace)
    targetEnvir$sethashData(image_val=hash)
    print("saving to db")
    
    # --- recalculate rank table ------------------
    if(length(sessionReference$idData)>0)
    {
      # newDistances <- distanceToRef( (unlist(targetEnvir$hashData[readyToRetrace$imgName])),
      #                                data.frame(sessionReference$hashData))
      newDistances <- distanceToRef( unlist(targetEnvir$gethashData(readyToRetrace$imgName)),
                                     data.frame(sessionReference$gethashData(names(sessionReference$idData))))
      # make sure to clip to correct number of columns
      newSortingIndex <- order(newDistances)[1:ncol(rankTable$Name)]
      
      rankTable$Name[readyToRetrace$rowName,] <- names(sessionReference$idData[newSortingIndex])
      rankTable$NameSimple[readyToRetrace$rowName,] <- basename(names(sessionReference$idData[newSortingIndex]))
      rankTable$ID[readyToRetrace$rowName,] <- sessionReference$idData[newSortingIndex]
      rankTable$Unique[readyToRetrace$rowName,] <- !duplicated(sessionReference$idData[newSortingIndex])
      rankTable$Distance[readyToRetrace$rowName,] <- newDistances[newSortingIndex]
      
      rankTable$editCount <- rankTable$editCount+1
    }
    # ---------------------------------------------
    
    panelEnv[[readyToRetrace$panelID]]$mode <- "default"
    
    traceGuideCounter$count <- (-1)
    
    readyToRetrace$imgName <- NULL
    readyToRetrace$rowName <- NULL
    readyToRetrace$panelID <- NULL
    readyToRetrace$directory <- NULL
    readyToRetrace$traceResults <- list()
  }else{
    print("skip len-0 trace")
  }
}