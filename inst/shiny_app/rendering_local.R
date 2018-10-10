

# ---------- Cluster Display windows --------------------------------------------------

windowObserver <- function(imageName,targetDir,panelID,selection,targetEnvir)
{
  # --- close window
  observeEvent(input[[paste0("close",panelID)]],ignoreInit=T,{
    print("<-><-><-><-><-><-><-><-><->")
    print(paste("close:",panelID,plotsPanel[[panelID]]$locked))
    print("<-><-><-><-><-><-><-><-><->")
    
    #NOT CLEAN SOLUTION
    #CLOSE IS BEING CALLED AFTER CLOSING
    if(exists(paste(panelID),envir = plotsPanel))
    {
      if(!plotsPanel[[panelID]]$locked)
      {
        removeIndex <- which(displayActive$activeSelections==selection)
        if(length(removeIndex)>0)
        {
          print(c(removeIndex,displayActive$activeSelections[removeIndex]))
          displayActive$activeSelections <- displayActive$activeSelections[-removeIndex]
          #displayActive$activeSelections[removeIndex] <- NULL
          
          print(paste("ppcontB4:",ls(plotsPanel)))
          remove(list=paste(panelID),envir = plotsPanel)
          print(paste("ppcontAFTR:",ls(plotsPanel)))
        }
        #remove(list=as.character(paste(panelID)),envir = plotsPanel)
      }
    }
  })
  
  
  # --- remove from session memory
  observeEvent(input[[paste0("remove",panelID)]],ignoreInit=T,{
    if(!plotsPanel[[panelID]]$locked)
    {
      removeIndex <- which(displayActive$activeSelections==selection)
      if(length(removeIndex)>0)
      {
        prepRemoval(imageName,readyToRemove,selection)
      }
    }
  })
  # --- set window to retrace
  observeEvent(input[[paste0("retrace",panelID)]],ignoreInit=T,{
    prepRetrace(panelID=panelID,
                imageName=imageName,
                targetDir = input$queryDirectory,
                readyToRetrace=readyToRetrace)
  })
  
  # --- restore defaults upon cancel
  observeEvent(input[[paste0("cancelRetrace",panelID)]],ignoreInit=T,{
    cancelRetrace(readyToRetrace=readyToRetrace,
                  targetEnvir=sessionQuery)
    plotsPanel[[panelID]]$coord <- targetEnvir$traceData[imageName]
  })
  
  # --- save trace edit
  observeEvent(input[[paste0("saveRetrace",panelID)]],ignoreInit=T,{
    saveRetrace(readyToRetrac=readyToRetrace,
                targetEnvir=sessionQuery)
    print("save complete")
    # reset outputs
    plotsPanel[[panelID]]$coord <- targetEnvir$traceData[imageName]
    print("outputs reset")
  })
  
  # --- lock window in place
  observeEvent(input[[paste0("lock",panelID)]],ignoreInit=T,{
    plotsPanel[[panelID]]$locked <- input[[paste0("lock",panelID)]]
    if(input[[paste0("lock",panelID)]])
    {
      displayActive$lockedSelections <- unique(append(displayActive$lockedSelections,selection))
    }else{
      removeIndex <- which(displayActive$lockedSelections == selection)
      if(length(removeIndex)>0)
      {
        displayActive$lockedSelections <- displayActive$lockedSelections[-removeIndex]
      }
    }
  })
  
  
  
  # --- set id
  observeEvent(input[[paste0("saveID",panelID)]],{
    assignID(panelID=panelID,
             imageName=imageName,
             targetEnvir=sessionQuery)
    plotsPanel[[panelID]]$mode <- "default"
    
    #neded to update FIX LATER
    output[[paste0("imageID",panelID)]] <- renderText(sessionQuery$idData[imageName])
  })
  observeEvent(input[[paste0("changeID",panelID)]],{
    plotsPanel[[panelID]]$mode <- "setID"
  })
}

  
windowGenerator <- function(selection,plotsPanel)
{
  hashMapLabel <- strsplit(selection,": ")[[1]]
  imageRegister <- hashMapLabel[2]
  panelID <- gsub("[[:punct:]]", "", hashMapLabel[2])
  panelID <- gsub("[[:space:]]", "", panelID)
  
  if(!exists(paste(panelID),envir = plotsPanel))
  {
    plotsPanel[[panelID]] <- reactiveValues(fin=NULL,
                                            coord=NULL,
                                            locked=FALSE,
                                            mode="default")
  }
  
  sourceType <- hashMapLabel[1]
  
  output[[paste0("imageName",panelID)]] <- renderText(imageRegister)
  
  if(substr(sourceType,nchar(sourceType)-4,nchar(sourceType)) == "Query")
  {
    targetEnvir <- sessionQuery
    allowEdit <- T
    targetDir <- normalizePath(file.path(input$queryDirectory,imageRegister))
  }else{
    targetEnvir <- sessionReference
    allowEdit <- F
    targetDir <- imageRegister
  }
  
  plotsPanel[[panelID]]$coord <- targetEnvir$traceData[imageRegister]
  
  output[[paste0("header",panelID)]] <- renderUI({
    generateDisplayHeader(panelID,
                          mode= plotsPanel[[panelID]]$mode,
                          closeOption = T,
                          fixOption=allowEdit)
  })
  
  output[[paste0("imageID",panelID)]] <- renderText(targetEnvir$idData[imageRegister])
  
  # --- image displays
  output[[paste0("image",panelID)]] <- renderPlot({
    print(paste('plot:',targetDir))
    plotFinTrace(load.image(targetDir),
                 plotsPanel[[panelID]]$coord,
                 input[[paste0("trace",panelID)]])#includeTrace
  })
  
  
  windowObserver(imageRegister,targetDir,panelID,selection,targetEnvir)
  
  return(
    column(width = 12,class = "well",
           uiOutput(paste0("header",panelID)),
           plotOutput(paste0("image",panelID),click = clickOpts(id = paste("clickPointSet"),clip = TRUE))
    )
  )
}