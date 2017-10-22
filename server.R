library("shiny")
library("DT")
library("mxnet")

#
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }")
options(shiny.maxRequestSize=10*1024^2)

# --- helper functions

randify <- function(angleVector)
{
  
  shift <- floor(runif(n=1,min=-75,max=75))
  if (shift > 0)
  {
    newVec <- angleVector[shift:length(angleVector)]
  }else{
    newVec <- angleVector[1:(length(angleVector)+shift)]
  }
 
  dyVec <- sin(angleVector)+runif(n=1,min = -.2,max = .2)
  dxVec <- cos(angleVector)+runif(n=1,min = -.2,max = .2)
  
  dyVec <- dyVec[sort(sample(seq_len(length(newVec)),200 ),decreasing = T) ]
  dxVec <- dxVec[sort(sample(seq_len(length(newVec)),200 ),decreasing = T) ]
  
  dyVec <- filter(dyVec,rep(1,3),sides=1)/3
  dxVec <- filter(dxVec,rep(1,3),sides=1)/3
  
  dyVec[1:3] <- dyVec[5:7]
  dxVec[1:3] <- dxVec[5:7]
  
  newAngles <- rev(atan2(dyVec,dxVec))
  
  return(newAngles)
}


plotAngleVector <- function(angles)
{
  par(mar = c(0,0,0,0))
  plot(angles,ylim=c(-pi/2,pi/2), pch=".", xaxt='n', ann=FALSE)
  lines(angles)
}
plotFinTrace <- function(fin,path,trace)
{
  if(!is.null(fin))
  {
    if(nrow(fin) > 1800)
    {
      fin <- resize(fin, size_x = 1800, size_y = 1800*(ncol(fin)/nrow(fin)))
    }
    if(trace && nrow(path) > 0)
    {
      plot(fin, ann=FALSE, asp = 0)
      par(new=TRUE)
      points(path[,1],path[,2],pch=".",col='red', xaxt='n',yaxt='n', ann=FALSE, asp = 1)
    }else{
      plot(fin, ann=FALSE)
    }
  }
}

getImgNames <- function(directory,saveEnvir)
{
  print("searching")
  imgs <- try(list.files(directory,full.names = T,pattern = "\\.JPG$"))
  if(typeof(imgs) == "try-error" || length(imgs)==0)
  {
    showModal(modalDialog(
      title = paste("No JPG file found in:",directory),
      size = "s",
      easyClose = TRUE
    ))
  }else{
    return(imgs)
  }
}

mahalOutliers <- function(queryMatrix,finMean,finCov,threshold=2300)
{
  #finMeanDiff <- apply(queryMatrix,1,function(x,finMean){x-(finMean)},finMean=finMean)
  mahalDist <- mahalanobis(queryMatrix, finMean, finCov)
  return(mahalDist>threshold)
}

distanceToRef <- function(queryHash,referenceHash)
{
  if(length(referenceHash)>0 && !is.null(queryHash))
  {
    return(apply(referenceHash,2,
      function(x,queryHash)
      {
        return(acos(as.numeric(x) %*% as.numeric(queryHash) ))
      },queryHash=queryHash)
    )
  }
}

traceToHash <- function(trace,edge=1,reps)
{
  if(sum(!is.na(as.numeric(trace[[edge]][[1]])))==0){return(NULL)}
  
  traceReps <- replicate(reps,spline(as.numeric(trace[[edge]][[1]]),n=400)$y)
  processedArray <- as.array(apply(traceReps,2,function(x)randify(x)))
  dim(processedArray) <- c(200,1,reps)
  
  netEmbedding <- predict(mxnetModel, 
                          processedArray, 
                          array.layout = "colmajor",
                          ctx=mx.cpu(), 
                          allow.extra.params = TRUE)
  print("NeuralNet embedding complete")
  hash <- rowMeans(netEmbedding)
  print(hash)
  return(unlist(hash/norm(hash,type="2")))
}

processImageData <- function(directory,saveEnvir,appendNew)
{
  imgPaths <- getImgNames(directory)
  remove <- NULL
  
  hashData <- list()
  traceData <- list()
  idData <- NULL
  
  progressTicker <- 0
  for(img in imgPaths)
  {
    print(img)
    traceResults <- try(isolateCurve(load.image(img)))
    if(class(traceResults)!="try-error" && 
       length(unlist(traceResults)[[1]])>0 &&
       !is.null(unlist(traceResults)[[1]]))
    {
      hashData <- append(hashData,data.frame(traceToHash(traceResults,1,100)))
      traceData <- append(traceData,traceResults)
      idData <- append(idData,"unlabeled")
    }else{
      
      print("removed..")
      print(traceResults)
      remove <- append(remove,which(imgPaths==img))
    }
    progressTicker <- progressTicker+1
    incProgress(1/length(imgPaths), detail = paste(basename(img)," -- ",progressTicker,"of",length(imgPaths)))
  }
  print(remove)
  if(length(remove)>0){print("removing");imgPaths <- imgPaths[-remove]}
  
  
  print("hashData");print(length(imgPaths));print(length(hashData))
  names(hashData) <- basename(imgPaths)
  print("traceData");print(length(imgPaths));print(length(traceData))
  names(traceData) <- basename(imgPaths)
  print("idData");print(length(imgPaths));print(length(idData))
  names(idData) <- basename(imgPaths)
  
  if(!appendNew)
  {
    saveEnvir$hashData <- list()
    saveEnvir$traceData <- list()
    saveEnvir$idData <- NULL
  }
    
  saveEnvir$hashData <- append(hashData,saveEnvir$hashData)
  saveEnvir$traceData <- append(traceData,saveEnvir$traceData)
  saveEnvir$idData <- append(idData,saveEnvir$idData)
}

#only works for query type atm
replaceImageData <- function(directory,imageName,startStopPoints,saveEnvir)
{
  print(imageName)
  traceResults <- try(isolateCurve(load.image(file.path(directory,imageName)),startStopPoints))
  
  if(class(traceResults)!="try-error" && 
     length(unlist(traceResults)[[1]])>0 &&
     !is.null(unlist(traceResults)[[1]]))
  {
    saveEnvir$hashData[imageName] <- data.frame(traceToHash(traceResults,1,50))
    saveEnvir$traceData[imageName] <- traceResults
  }else{
    print("removed..")
  }
}



loadRdata <- function(directory,saveEnvir,appendNew,isRef)
{
  print("searching")
  RdataFiles <- try(list.files(directory, full.names=TRUE, pattern="\\.Rdata$", recursive=appendNew))
  if(typeof(RdataFiles) != "try-error" && length(RdataFiles)>0)
  {
    tempEnvir <- new.env()
    tempEnvir$hashData <- list()
    tempEnvir$traceData <- list()
    tempEnvir$idData <- NULL
    
    loopEnvir <- new.env()
    
    for(RdataFile in dirname(RdataFiles))
    {
      if(file.exists(file.path(RdataFile,"finFindR.Rdata")))
      {
        print(file.path(RdataFile,"finFindR.Rdata"))
        load(file.path(RdataFile,"finFindR.Rdata"),loopEnvir)
        
        if(isRef)
        {
          names(loopEnvir$hashData) <- file.path(RdataFile,names(loopEnvir$hashData))
          names(loopEnvir$traceData) <- file.path(RdataFile,names(loopEnvir$traceData))
          names(loopEnvir$idData) <- file.path(RdataFile,names(loopEnvir$idData))
        }
        tempEnvir$hashData <- append(tempEnvir$hashData,loopEnvir$hashData)
        tempEnvir$traceData <- append(tempEnvir$traceData,loopEnvir$traceData)
        tempEnvir$idData <- append(tempEnvir$idData,loopEnvir$idData)
      }
    }
    rm(loopEnvir)
    
    if(!appendNew)
    {
      saveEnvir$hashData <- list()
      saveEnvir$traceData <- list()
      saveEnvir$idData <- NULL
    }
    saveEnvir$hashData <- append(saveEnvir$hashData,tempEnvir$hashData)
    saveEnvir$traceData <- append(saveEnvir$traceData,tempEnvir$traceData)
    saveEnvir$idData <- append(saveEnvir$idData,tempEnvir$idData)
    
    rm(tempEnvir)
    
  }else{
    showModal(modalDialog(
      title = paste("No Rdata file found in:",directory),
      size = "s",
      easyClose = TRUE
    ))
  }
}


extractMetadata <- function(directory,saveEnvir)
{
  print("searching")
  imgs <- try(list.files(directory,full.names = T,pattern = "\\.JPG$"))
  if(typeof(imgs) != "try-error" && length(imgs)>0)
  {
    metadata <- easyEXIF(list.files(directory,full.names = T,pattern = "\\.JPG$"))
    metadata <- as.data.frame(do.call(rbind, metadata))
    colnames(metadata) <- c("ID","Hash","Lat","Lon","Image")
    assign('metadata',metadata,envir = saveEnvir)
  }else{
    showModal(modalDialog(
      title = paste("No JPG found in:",directory),
      size = "s",
      easyClose = TRUE
    ))
  }
}



#####################################################################################
### Server Logic
#####################################################################################

function(input, output, session) {
  
  appendRecursive <- TRUE
  
  
  # --- stop r from app ui
  session$onSessionEnded(function(){
    stopApp()
  })
  
  sessionReference <- new.env()
  sessionQuery <- new.env()
  sessionStorage <- new.env()
  
  observeEvent(input$clearQuery,{
    sessionQuery$hashData <- list()
    sessionQuery$traceData <- list()
    sessionQuery$idData <- NULL
    
    output$querySelector <- renderUI(
      fluidRow(
        column(width = 4,
          actionButton("removeSingle","Remove")
        ),
        column(width = 8,
          selectInput(
            inputId = "imgSelected",
            label = NULL,
            choices = names(sessionQuery$idData)
          )
        )
      )
    )
    output$anglesQuery <- renderPlot(NULL)
    output$queryImage <- renderPlot(NULL)
    
    selectedInstance$photo <- NULL
  })
  observeEvent(input$clearRef,{
    sessionReference$hashData <- list()
    sessionReference$traceData <- list()
    sessionReference$idData <- NULL
    
    selectedInstance$photo <- NULL
  })
  
  # --- establish reference
  observeEvent(input$inputSize,{
    if(input$inputSize == 'Batch')
    {
      updateRadioButtons(session,"refView",choices =c("Match","Cluster"))
    }else if(input$inputSize == 'Single'){
      updateRadioButtons(session,"refView",choices =c("Match"))
    }
  })
  
  # --- save Rdata
  observeEvent(input$saveRdata,{
    
    if(dir.exists(input$queryDirectory) &&
       !is.null(input$queryDirectory) && 
       input$queryDirectory != "" && 
       length(input$queryDirectory)>0)
    {
      save(list = as.character(c("hashData","traceData","idData")),
           file=file.path(input$queryDirectory,"finFindR.Rdata"),
           envir = sessionQuery)
      showModal(modalDialog(
        title = paste("Save Successful"),
        size = "s",
        easyClose = TRUE
      ))
    }else{
      showModal(modalDialog(
        title = paste("No File Selected"),
        size = "s",
        easyClose = TRUE
      ))
    }
  })
  
  # --- Remove entry
  readyToRemove <-  reactiveValues(imgName=NULL)
  modifiedQueryDone <- reactiveValues(status=1)
  observeEvent(input$removeQuery, {

    sessionQuery$idData <- sessionQuery$idData[which(names(sessionQuery$idData)!=readyToRemove$imgName)]
    sessionQuery$hashData[readyToRemove$imgName] <- NULL
    sessionQuery$traceData[readyToRemove$imgName] <- NULL
    
    #reactive switch indicates change complete
    modifiedQueryDone$status <- modifiedQueryDone$status*-1
    
    removeModal(session = getDefaultReactiveDomain())
    print(paste("removed",readyToRemove$imgName))
  })
  observeEvent(input$removeSingle,{
    if(!is.null(input$imgSelected))
    {
      readyToRemove$imgName <- input$imgSelected
      showModal(modalDialog(
        title = paste("Are you sure you want to remove",input$imgSelected,"?"),
        footer = tagList(actionButton("removeQuery", "Remove"),modalButton("Cancel")),
        size = "s",
        easyClose = TRUE
      ))
    }
  })
  observeEvent(input$removeLeft,{
    if(!is.null(leftPhoto$imgName))
    {
      readyToRemove$imgName <- leftPhoto$imgName
      showModal(modalDialog(
        title = paste("Are you sure you want to remove",leftPhoto$imgName,"?"),
        footer = tagList(actionButton("removeQuery", "Remove"),modalButton("Cancel")),
        size = "s",
        easyClose = TRUE
      ))
    }
    observeEvent(input$removeQuery,{
      output$leftPhoto <- renderText(NULL)
      output$leftID <- renderText(NULL)
      output$refImageLeft <- renderPlot(NULL)
      output$anglesLeft <- renderPlot(NULL)
    })
  })
  observeEvent(input$removeRight,{
    if(!is.null(rightPhoto$imgName))
    {
      readyToRemove$imgName <- rightPhoto$imgName
      showModal(modalDialog(
        title = paste("Are you sure you want to remove",rightPhoto$imgName,"?"),
        footer = tagList(actionButton("removeQuery", "Remove"),modalButton("Cancel")),
        size = "s",
        easyClose = TRUE
      ))
    }
    observeEvent(input$removeQuery,{
      output$rightPhoto <- renderText(NULL)
      output$rightID <- renderText(NULL)
      output$refImageRight <- renderPlot(NULL)
      output$anglesRight <- renderPlot(NULL)
    })
  })
  
  # --- trace with human input
  readyToRetrace <-  reactiveValues(imgName=NULL)
  modifiedQueryDone <- reactiveValues(status=1)
  observeEvent(input$retraceQuery, {
    if(!is.null(readyToRetrace$imgName))
    {
      replaceImageData(readyToRetrace$imgName,input$queryDirectory)
      #reactive switch indicates change complete
      modifiedQueryDone$status <- modifiedQueryDone$status*-1
      removeModal(session = getDefaultReactiveDomain())
    }
  })
  observeEvent(input$retraceSingle,{
    if(!is.null(input$imgSelected))
    {
      readyToRetrace$imgName <- imgSelected
      if(!is.null(rightPhoto$imgName))
      {
        readyToRetrace$imgName <- rightPhoto$imgName
        showModal(modalDialog(
          title = paste("Click the Tip of fin, then the End of the trailing edge, then the Front of the leading edge"),
          footer = tagList(actionButton("retraceQuery", "Done"),modalButton("Cancel")),
          size = "s",
          easyClose = TRUE
        ))
      }
    }
    # observeEvent(input$retraceQuery,{
    # 
    # })
  })
  observeEvent(input$retraceLeft,{
    if(!is.null(leftPhoto$imgName))
    {
      readyToRetrace$imgName <- leftPhoto$imgName
      if(!is.null(rightPhoto$imgName))
      {
        readyToRetrace$imgName <- rightPhoto$imgName
        showModal(modalDialog(
          title = paste("Click the Tip of fin, then the End of the trailing edge, then the Front of the leading edge"),
          footer = tagList(actionButton("retraceQuery", "Done"),modalButton("Cancel")),
          size = "s",
          easyClose = TRUE
        ))
      }
    }
    # observeEvent(input$retraceQuery,{
    # 
    # })
  })
  observeEvent(input$retraceRight,{
    if(!is.null(rightPhoto$imgName))
    {
      readyToRetrace$imgName <- rightPhoto$imgName
      showModal(modalDialog(
        title = paste("Click the Tip of fin, then the End of the trailing edge, then the Front of the leading edge"),
        footer = tagList(actionButton("retraceQuery", "Done"),modalButton("Cancel")),
        size = "s",
        easyClose = TRUE
      ))
    }
    # observeEvent(input$removeQuery,{
    #   
    # })
  })
  
  
  
  # --- get data into r
  observeEvent(input$loadRdataRef,{
    loadRdata(input$referenceDirectory,sessionReference,appendRecursive,TRUE)
  })
  observeEvent(input$loadRdataQuery,{
    loadRdata(input$queryDirectory,sessionQuery,FALSE,FALSE)
  })
  #for batches, establish query
  #for a batch that goes from image to hash
  traceBatchDone <- reactiveValues(status=1)
  observeEvent(input$traceBatchQuery,{
    withProgress(message = 'Processing Images', value = 0, processImageData(input$queryDirectory,sessionQuery,FALSE))
    traceBatchDone$status <- traceBatchDone$status*-1
  })

  
  observeEvent(c(input$loadRdataQuery,
                 modifiedQueryDone$status,
                 traceBatchDone$status),{
    output$querySelector <- renderUI(
      fluidRow(
        column(width = 2,
          actionButton("retraceSingle","Trace")
        ),
        column(width = 3,
          actionButton("removeSingle","Remove")
        ),
        
        column(width = 7,
          selectInput(
            inputId = "imgSelected",
            label = NULL,
            choices = names(sessionQuery$idData)
          )
        )
      )
    )
    print(input$imgSelected)
  })
  
  includeTrace <- reactiveValues(bool=TRUE)
  observeEvent(input$trace,{includeTrace$bool <- input$trace})
  
  # --- display for the trace features 
  # --- fin image and overlay of traced path
  observeEvent(input$imgSelected,{
    output$anglesQuery <- renderPlot(
      plotAngleVector(sessionQuery$traceData[input$imgSelected][[1]][[1]])
    )
    output$queryImage <- renderPlot({
      fin <- load.image(file.path(input$queryDirectory, input$imgSelected))#input$finImage$datapath
      path <- matrix(sessionQuery$traceData[input$imgSelected][[1]][[2]] ,ncol=2)
      plotFinTrace(fin,path,includeTrace$bool)
    })
  })
  
  # --- display specific image if comparing a batch
  distanceTbl <- reactiveValues(val=NULL)
  observeEvent(c(input$loadRdataRef,
                 input$clearRef,
                 input$clearQuery,
                 input$imgSelected,
                 traceBatchDone$status), {
    if(!is.null(sessionReference$hashData) && !is.null(input$imgSelected))
    {
      Distances <- round(as.numeric(distanceToRef(sessionQuery$hashData[[input$imgSelected]], data.frame(sessionReference$hashData))),5 )
      if(length(Distances)>0)
      {
        names(Distances) <- names(sessionReference$hashData)
        orderedIndex <- order(Distances,decreasing = FALSE)
        allignByPhotoName <- match(names(Distances),names(sessionReference$idData))
        
        distanceTbl$val <- cbind.data.frame(Distances,ID=sessionReference$idData[allignByPhotoName])[orderedIndex,]
      
        output$singleID <- renderText({
          if(length(sessionQuery$idData)>0)
          {
            sessionQuery$idData[input$imgSelected]
          }else{
            NULL
          }
        })
        
        output$confidence <- DT::renderDataTable({
          if(length(distanceTbl$val)>0)
          {
            distanceTbl$val
          }else{
            NULL
          }
        },selection = 'single')
      }
    }
  })
  
  # --- fin image and overlay of traced path
  observeEvent(c(input$confidence_rows_selected,
                 input$imgSelected, 
                 input$loadRdataRef,
                 input$clearRef,
                 input$clearQuery),{
    if(!is.null(input$confidence_rows_selected)){
      output$refImage <- renderPlot({
        if(length(sessionReference$traceData)>0)
        {
          fin <- load.image(rownames(distanceTbl$val)[input$confidence_rows_selected])
          path <- matrix(sessionReference$traceData[rownames(distanceTbl$val)[input$confidence_rows_selected]][[1]][[2]] ,ncol=2)
          plotFinTrace(fin,path,includeTrace$bool)
        }else{
          NULL
        }
      })
      output$anglesRef <- renderPlot(
        if(length(sessionReference$traceData)>0)
        {
          plotAngleVector(sessionReference$traceData[rownames(distanceTbl$val)[input$confidence_rows_selected]][[1]][[1]])
        }else{
          NULL
        }
      )
    }
  })
  
  observeEvent(input$assignIDSingle,{
    if(input$textIDSingle == "")
    {
      sessionQuery$idData[input$imgSelected] <- "unlabeled"
    }else{
      sessionQuery$idData[input$imgSelected] <- input$textIDSingle
    }
    output$singleID <- renderText(sessionQuery$idData[input$imgSelected])
  })

  
  # --- Batch Self Reference Hierarchical Clustering
  
  selectedRange <- reactiveValues(y = NULL)
  selectedInstance <- reactiveValues(photo = NULL)
  
  observeEvent(c(input$dblclick), {
    brush <- input$brush
    
    if (!is.null(brush)) {
      selectedRange$y <- c(brush$ymin, brush$ymax)
    } else {
      #zoom out
      selectedRange$y <- c(0, 1)
      sessionStorage$permutation <- NULL
    }
  })
  
  hashRow <- reactiveValues(names = NULL)
  
  # --- Hash Reference Table
  observeEvent(c(input$loadRdataQuery,
                 input$loadRdataRef,
                 input$clearRef,
                 input$clearQuery,
                 traceBatchDone$status,
                 modifiedQueryDone$status), {
    selectedRange$y <- c(0, 1)
    sessionStorage$permutation <- NULL
                   
    output$hashComparison <- renderPlot(width = "auto",
                                        height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)),
    {
      if(length(sessionReference$hashData)>0 || length(sessionQuery$hashData)>0)
      {
        allHashData <- append(sessionQuery$hashData,sessionReference$hashData)
        
        hashRow$names <- append(if(length(sessionQuery$hashData)>0){paste("Qu:",names(sessionQuery$hashData))},
                                if(length(sessionReference$hashData)>0){paste("Ref:",names(sessionReference$hashData))})
        
        hashData <- t(data.matrix(data.frame(allHashData)))
        rownames(hashData) <- hashRow$names
        
        if(length(sessionStorage$permutation) == 0 || is.null(sessionStorage$permutation))
        {
          par(mar = c(.3,0,0,0))
          dendroParams <- heatmap(hashData, 
                                  margins = c(1,8), 
                                  col = gray.colors(100), 
                                  Colv = NA)
          sessionStorage$permutation <- dendroParams$rowInd
        }
        
        indRange <- ceiling(selectedRange$y*length(sessionStorage$permutation))
        indRange[1] <- max(1,indRange[1],na.rm = TRUE)
        indRange[2] <- min(length(sessionStorage$permutation),indRange[2],na.rm = TRUE)
        
        par(mar = c(.3,0,0,0))
        dendroParams <- heatmap(hashData[sessionStorage$permutation[indRange[1]:indRange[2]],],
                                margins = c(1,8),
                                col = gray.colors(100),
                                Colv = NA)
        
        sessionStorage$permutation <- sessionStorage$permutation[c(indRange[1]:indRange[2])[dendroParams$rowInd]]
      }else{
        NULL
      }
    })
  })
  
  # --- instance selection
  observeEvent(input$click, {
    ind <- ceiling(input$click$y*length(sessionStorage$permutation))
    ind <- max(ind,na.rm = TRUE)
    ind <- min(length(sessionStorage$permutation),ind,na.rm = TRUE)
    
    selectedInstance$photo <- hashRow$names[sessionStorage$permutation[ind]]
    
    output$selectedPhoto <- renderText({
      if(length(hashRow$names)>0)
      {
        selectedInstance$photo
      }else{
        NULL
      }
    })
  })
  
  
  
  # --- set image comparison panels
  leftPhoto <- reactiveValues(
    imgName = NULL,
    sourceType = NULL
  )
  
  observeEvent(input$assignIDLeft,{
    if(length(sessionQuery$idData)>0)
    {
      sessionQuery$idData[leftPhoto$imgName] <- input$textIDLeft
      output$leftID <- renderText(sessionQuery$idData[leftPhoto$imgName])
    }
  })
  observeEvent(input$setLeft,{
    if(!is.null(selectedInstance$photo) &&
       length(selectedInstance$photo)>0)
    {
      leftPhoto$imgName <- strsplit(selectedInstance$photo,": ")[[1]][2]
      leftPhoto$sourceType <- strsplit(selectedInstance$photo,": ")[[1]][1]
  
      if(leftPhoto$sourceType == "Qu")
      {
        targetEnvir <- sessionQuery
        targetDir <- file.path(input$queryDirectory,leftPhoto$imgName)
        output$removeLeft <- renderUI(actionButton("removeLeft","Remove"))
        output$retraceLeft <- renderUI(actionButton("retraceLeft","Trace"))
      }
      if(leftPhoto$sourceType == "Ref")
      {
        targetEnvir <- sessionReference
        targetDir <- leftPhoto$imgName
        output$removeLeft <- renderUI(NULL)
      }
      output$leftPhoto <- renderText(leftPhoto$imgName)
      output$leftID <- renderText(targetEnvir$idData[leftPhoto$imgName])
      
      # --- image displays
      output$refImageLeft <- renderPlot({
        plotFinTrace(load.image(targetDir),
                     targetEnvir$traceData[leftPhoto$imgName][[1]][[2]],
                     includeTrace$bool)
      })
      output$anglesLeft <- renderPlot({
        plotAngleVector(targetEnvir$traceData[leftPhoto$imgName][[1]][[1]])
      })
      
      output$leftAssignButton <- renderUI({
        verbatimTextOutput("leftID")
        if(leftPhoto$sourceType != "Ref")
        {
          column(width = 12,
            fluidRow(
              column(width = 6,
                textInput(
                  inputId = "textIDLeft",
                  label = NULL
                )
              ),
              column(width = 6,
                actionButton("assignIDLeft","Assign ID")
              )
            )
          )
        }
      })
    }
  })
  
  
  rightPhoto <- reactiveValues(
    imgName = NULL,
    sourceType = NULL
  )
  observeEvent(input$assignIDRight,{
    if(length(sessionQuery$idData)>0)
    {
      sessionQuery$idData[rightPhoto$imgName] <- input$textIDRight
      output$rightID <- renderText(sessionQuery$idData[rightPhoto$imgName])
    }
  })
  observeEvent(input$setRight,{
    if(!is.null(selectedInstance$photo) &&
       length(selectedInstance$photo)>0)
    {
      rightPhoto$imgName <- strsplit(selectedInstance$photo,": ")[[1]][2]
      rightPhoto$sourceType <- strsplit(selectedInstance$photo,": ")[[1]][1]
      
      if(rightPhoto$sourceType == "Qu")
      {
        targetEnvir <- sessionQuery
        targetDir <- file.path(input$queryDirectory,rightPhoto$imgName)
        output$removeRight <- renderUI(actionButton("removeRight","Remove"))
        output$retraceRight <- renderUI(actionButton("retraceRight","Trace"))
      }
      if(rightPhoto$sourceType == "Ref")
      {
        targetEnvir <- sessionReference
        targetDir <- rightPhoto$imgName
        output$removeRight <- renderUI(NULL)
      }
      output$rightPhoto <- renderText(rightPhoto$imgName)
      output$rightID <- renderText(targetEnvir$idData[rightPhoto$imgName])
      
      output$refImageRight <- renderPlot({
        plotFinTrace(load.image(targetDir),
                     targetEnvir$traceData[rightPhoto$imgName][[1]][[2]],
                     includeTrace$bool)
      })
      output$anglesRight <- renderPlot({
        plotAngleVector(targetEnvir$traceData[rightPhoto$imgName][[1]][[1]])
      })
      
      output$rightAssignButton <- renderUI({
        verbatimTextOutput("rightID")
        if(rightPhoto$sourceType != "Ref")
        {
          column(width = 12,
            fluidRow(
              column(width = 6,
                textInput(
                  inputId = "textIDRight",
                  label = NULL
                )
              ),
              column(width = 6,
                actionButton("assignIDRight","Assign ID")
              )
            )
          )
        }
      })
    }
  })
}