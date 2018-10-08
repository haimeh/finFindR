
plotFinTrace <- function(fin,coordinates,trace)
{
  if(length(fin)>0)
  {
    par(mar = c(0,0,0,0))
    if(length(coordinates)>0)
    {
      #dont forget to unlist coordinates
      coordinates <- coordinates[[1]]
      if(nrow(coordinates) > 0 && trace)
      {
        plot(fin, ann=FALSE, asp = 1, axes = FALSE)
        par(new=TRUE)
        points(coordinates[,1],coordinates[,2],pch=".",col='red', ann=FALSE, asp = 0)
      }else{
        plot(fin, ann=FALSE, axes = FALSE)
      }
    }else{
      plot(fin, ann=FALSE, axes = FALSE)
    }
  }else{
    print("not an image")
  }
}

# --- set image header pannel
generateDisplayHeader <- function(instance,
                                  mode="default",
                                  closeOption=T,
                                  fixOption=T){
  
  if(fixOption){
    modSelection <- tagList(
      actionButton(paste0("retrace",instance),"Fix"),
      actionButton(paste0("remove",instance),"Remove"),
      
      column(width = 3,checkboxInput(
        inputId = paste0("trace",instance),
        label = "Trace",
        value = FALSE
      ))
    )
  }else{
    modSelection <- tagList(
      column(width = 4,
             checkboxInput(
               inputId = paste0("trace",instance),
               label = "Trace",
               value = TRUE
             ))
    )
  }
  
  if(mode=="fix")
  {
    return(tagList(
      h4("Click start point then Click end point"),
      actionButton(paste0("cancelRetrace",instance),"Cancel"),
      actionButton(paste0("saveRetrace",instance),"Save")
    ))
    
  }else{
    
    return(tagList(
      fluidRow(
        column(width = if(closeOption){9}else{12}, 
               verbatimTextOutput(paste0("imageName",instance)),
               tags$head(tags$style(paste0("#imageName",instance,"{overflow-x:hidden}"))),
               
               if(mode=="setID")
               {
                 fluidRow(
                   column(width=9,
                          textInput(paste0("textID",instance),label = NULL)),
                   column(width=3,
                          actionButton(paste0("saveID",instance),label = "Set"))
                 )
               }else{
                 fluidRow(
                   column(width=if(fixOption){8}else{12},
                          tags$head(tags$style(paste0("#imageID",instance,"{overflow-x:hidden}"))),
                          verbatimTextOutput(paste0("imageID",instance))),
                   if(fixOption)
                   {
                     column(width=3,
                            actionButton(paste0("changeID",instance),"Change"))
                   }
                 )
               }),
        if(closeOption)
        {
          column(width = 3,
                 actionButton(paste0("close",instance),"close"),
                 # FINISH LOCK
                 checkboxInput(
                   inputId = paste0("lock",instance),
                   label = "Lock",
                   value = ifelse(plotsPanel[[instance]]$locked,TRUE,FALSE)
                 )
          )
        }
      ),modSelection
    ))
  }
}

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