library("shiny")
library("DT")
library("mxnet")
library("imager")
library("finFindR")


options(shiny.maxRequestSize=10*1024^2)
options(stringsAsFactors=FALSE)

appendRecursive <- TRUE
plotLim <- 4

networks <- system.file("extdata", package="finFindR")

pathNet <- mxnet::mx.model.load(file.path(networks,'tracePath_256'), 60)
mxnetModel <- mxnet::mx.model.load(file.path(networks,'fin_triplet_4096_out'), 2800)
cropNet <- mxnet::mx.model.load(file.path(networks,'finLocate_256'), 200)


# --- Helper functions -----------------------------------------------------------------------------------------
# ==================================================================================================================

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

getImgNames <- function(directory,saveEnvir)
{
  print("searching")
  imgs <- try(list.files(directory,full.names = T,pattern = "\\.JPG$|\\.jpg$"))
  if(typeof(imgs) == "try-error" || length(imgs)==0)
  {
    showModal(modalDialog(
      title = "Search error",
      HTML(paste("No JPG file found in:<br>",directory)),
      size = "m",
      easyClose = TRUE
    ))
  }
  return(imgs)
}

distanceToRef <- function(queryHash,referenceHash,counterEnvir=NULL)
{
  if(!is.null(counterEnvir))
  {
    curVal <- get("progressTicker", envir = counterEnvir)
    incProgress(1/counterEnvir$length, 
                detail = paste(counterEnvir$progressTicker,"of",counterEnvir$length))
    assign("progressTicker", curVal +1 ,envir= counterEnvir)
  }
  if(length(referenceHash)>0 && !is.null(queryHash))
  {
    return(apply(referenceHash,2,
                 function(x,queryHash)
                 {
                   distance <- sqrt(sum((as.numeric(x)-as.numeric(queryHash))^2))
                   return(if(!is.nan(distance)){distance}else{0})
                 },queryHash=queryHash)
    )
  }
}


finIter <- setRefClass("finIter",
                       
                       fields=c("data",
                                "iter",
                                "data.shape"),
                       
                       contains = "Rcpp_MXArrayDataIter",
                       
                       methods=list(
                         initialize=function(iter=NULL,
                                             data,
                                             data.shape){
                           
                           data_len <- prod(data.shape)
                           print(dim(data))
                           array_iter <- mx.io.arrayiter(data,
                                                         label=rep(0,ncol(data)),
                                                         batch.size=ncol(data))
                           
                           .self$iter <- array_iter
                           
                           .self$data.shape <- data.shape
                           
                           .self
                         },
                         
                         value=function(){
                           val.x <- as.array(.self$iter$value()$data)
                           
                           val.x[is.na(val.x) | is.nan(val.x) | is.infinite(val.x)]<-(.5)
  
                           dim(val.x) <- c(data.shape,16,3,ncol(val.x))
                           val.x <- mx.nd.array(val.x)
                           
                           list(data=val.x)
                         },
                         
                         iter.next=function(){
                           .self$iter$iter.next()
                         },
                         reset=function(){
                           .self$iter$reset()
                         },
                         num.pad=function(){
                           .self$iter$num.pad()
                         },
                         finalize=function(){
                           .self$iter$finalize()
                         }
                       )
)

traceToHash <- function(traceData)
{
  iterInputFormat <- sapply(traceData,function(x){as.array(resize(x,size_x = 200,interpolation_type = 5))})
  dataIter <- finIter$new(data = iterInputFormat,
                          data.shape = 200)
  print("embed")
  netEmbedding <- mxnet:::predict.MXFeedForwardModel(mxnetModel,
                                                     dataIter,
                                                     array.layout = "colmajor",
                                                     ctx= mx.cpu(),
                                                     allow.extra.params=T)
  rm(dataIter)
  
  #dim(netEmbedding) <- c(32,length(traceData),2)
  #netEmbedding <- apply(netEmbedding, 1:2, mean)
  
  print("NeuralNet embedding complete")
  hashList <- lapply(seq_len(ncol(netEmbedding)), function(i) netEmbedding[,i])
  print("listified")
  print(names(traceData))
  names(hashList) <- names(traceData)
  print("labeled")
  return(hashList)
}

cropDirectory <- function(searchDirectory,
                          saveDirectory,
                          cropNet,
                          workingImage,
                          minXY=200,
                          includeSubDir=T,
                          mimicDirStructure=T)
{
  # dirStruct <- strsplit(imgName,searchDirectory)
  queryImgs <- list.files(searchDirectory, 
                          pattern = "\\.JPG$|\\.jpg$", 
                          full.names = T, 
                          recursive = includeSubDir)
  completeImgs <- list.files(saveDirectory, 
                             pattern = "\\.JPG$|\\.jpg$", 
                             full.names = F, 
                             recursive = T)
  
  mainImgName <- lapply(queryImgs, function(imageName){strsplit(basename(imageName),"/.")[[1]][1]})
  mainImgName <- lapply(mainImgName, function(imageName){strsplit(basename(imageName),"\\.JPG$|\\.jpg$")[[1]]})
  cropedImgNames <- lapply(completeImgs, function(imageName){strsplit(basename(imageName),"_")[[1]][1]})
  
  index <- !(mainImgName %in% cropedImgNames)
  withProgress(message = 'Cropping', value = 0,
               {
                 progressTicker <- 0
                 for(imgName in queryImgs[index])
                 {
                   progressTicker <- progressTicker+1
                   if(mimicDirStructure)
                   {
                     dirStruct <- gsub(searchDirectory,"",imgName)
                     
                     folderNames <- unlist(strsplit(dirStruct,"/"))
                     folderNames <- folderNames[-length(folderNames)]
                     folderNames <- folderNames[-which(folderNames %in% c("","\\") )]
                     
                     newDirStruct <- ""
                     for(folder in folderNames)
                     {
                       newDirStruct <- file.path(newDirStruct,folder)
                       dir.create(file.path(saveDirectory,newDirStruct), showWarnings = FALSE)
                     }
                   }else{
                     newDirStruct <- ""
                   }
                   print("==== + ====")
                   print(basename(imgName))
                   #print(file.path(sub('/$','',saveDirectory),sub('^/','',newDirStruct)))
                   try(cropFins(imgName,cropNet,workingImage, file.path(sub('/$','',saveDirectory),sub('^/','',newDirStruct)),minXY ))
                   incProgress(1/sum(index), detail = paste(basename(imgName)," -- ",progressTicker,"of",sum(index)))
                   
                 }
               })
}

processImageData <- function(directory,saveEnvir,appendNew,pathNet)
{
  imgPaths <- getImgNames(directory)
  
  if(typeof(imgPaths) != "try-error" && length(imgPaths)!=0)
  {
    remove <- NULL
    
    hashData <- list()
    traceImg <- list()
    traceCoord <- list()
    idData <- NULL
    
    progressTicker <- 0
    for(img in imgPaths)
    {

      print(paste("loading",basename(img)))
      progressTicker <- progressTicker+1
      incProgress(1/length(imgPaths), detail = paste(basename(img)," -- ",progressTicker,"of",length(imgPaths)))
      traceResults <- try(traceFromImage(load.image(img),NULL,pathNet))
      if(class(traceResults)!="try-error" && 
         length(unlist(traceResults)[[1]])>0 &&
         !is.null(unlist(traceResults)[[1]]))
      {
        traceImg <- append(traceImg,list(traceResults$annulus))
        traceCoord <- append(traceCoord,list(traceResults$coordinates))
        
        idData <- append(idData,"unlabeled")
      }else{
        
        print("removed..")
        print(traceResults)
        remove <- append(remove,which(imgPaths==img))
      }
    }
    print(remove)
    if(length(remove)>0){print("removing");imgPaths <- imgPaths[-remove]}
    hashData <- as.data.frame(traceToHash(traceImg))
    
    # name lists of data
    names(hashData) <- basename(imgPaths)
    names(traceCoord) <- basename(imgPaths)
    names(idData) <- basename(imgPaths)
    if(!appendNew)
    {
      saveEnvir$hashData <- list()
      saveEnvir$traceData <- list()
      saveEnvir$idData <- NULL
    }
    
    saveEnvir$hashData <- append(hashData,saveEnvir$hashData)
    saveEnvir$traceData <- append(traceCoord,saveEnvir$traceData)
    saveEnvir$idData <- append(idData,saveEnvir$idData)
  }
}


loadRdata <- function(directory,saveEnvir,appendNew,isRef)
{
  withProgress(
    message = "Searching for Rdata Files", value = 0,{
    RdataFiles <- try(list.files(directory, full.names=TRUE, pattern="\\.Rdata$", recursive=appendNew))
    if(typeof(RdataFiles) != "try-error" && length(RdataFiles)>0)
    {
      tempEnvir <- new.env()
      tempEnvir$hashData <- list()
      tempEnvir$traceData <- list()
      tempEnvir$idData <- NULL
      
      loopEnvir <- new.env()
      for(RdataFile in unique(dirname(RdataFiles)))
      {
        incProgress(1/length(unique(dirname(RdataFiles))))
        if(file.exists(file.path(RdataFile,"finFindR.Rdata")))
        {
          print(paste(length(dirname(RdataFiles)),isRef,"of",file.path(RdataFile,"finFindR.Rdata")))
          load(file.path(RdataFile,"finFindR.Rdata"),loopEnvir)
          
          # to make references dir invariant, we only saved the photo name and look for dir when we are loading
          if(isRef)
          {
            names(loopEnvir$hashData) <- normalizePath(file.path(RdataFile,names(loopEnvir$hashData)))
            names(loopEnvir$traceData) <- normalizePath(file.path(RdataFile,names(loopEnvir$traceData)))
            names(loopEnvir$idData) <- normalizePath(file.path(RdataFile,names(loopEnvir$idData)))
          }
          
          # this check is needed for a problem with the origional pregen rdata
          # typically should not be needed
          consistencyCheck <- c(length(loopEnvir$hashData),length(loopEnvir$traceData),length(loopEnvir$idData) )
          if(min(consistencyCheck) != max(consistencyCheck))
          {
            refNames <- list(names(loopEnvir$hashData),names(loopEnvir$traceData),names(loopEnvir$idData))[which.min(consistencyCheck)]
            
            loopEnvir$hashData <- loopEnvir$hashData[unique(unlist(refNames))]
            loopEnvir$traceData <- loopEnvir$traceData[unique(unlist(refNames))]
            loopEnvir$idData <- loopEnvir$idData[unlist(refNames)]
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
        title = "Search error",
        HTML(paste("No Rdata file found in:<br>",directory)),
        size = "m",
        easyClose = TRUE
      ))
    }
  })
}


calculateRankTable <- function(rankTable,sessionQuery,sessionReference)
{
  counterEnvir <- new.env()
  counterEnvir$progressTicker <- 0
  counterEnvir$length <- length(sessionQuery$hashData)
  withProgress(
    message = 'Matching', value = 0,
    distances <- as.data.frame(t(apply(data.frame(sessionQuery$hashData),2,function(x)distanceToRef(x,data.frame(sessionReference$hashData),counterEnvir))))
  )
  colnames(distances) <- NULL
  
  if(length(distances)>0)
  {
    withProgress(
    message = 'Sorting', value = 0,{
      sortingIndex <- as.data.frame(apply(distances,1,function(x)order(x,decreasing = F)))
      incProgress(1/6)
      rankTable$Name <- apply(sortingIndex,1,function(x)names(sessionReference$idData)[x])
      # single queries need to be turned back from vectors
      if(nrow(distances)<=1){rankTable$Name <- as.data.frame(t(rankTable$Name))}
      rownames(rankTable$Name) <- names(sessionQuery$hashData)
      incProgress(1/6)
      
      rankTable$NameSimple <- matrix(basename(as.character(rankTable$Name)),nrow=nrow(rankTable$Name),ncol=ncol(rankTable$Name))
      rownames(rankTable$NameSimple) <- names(sessionQuery$hashData)
      incProgress(1/6)
      
      rankTable$ID <- apply(sortingIndex,1,function(x)sessionReference$idData[x])
      # single queries need to be turned back from vectors
      if(nrow(distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID))}
      rownames(rankTable$ID) <- names(sessionQuery$hashData)
      incProgress(1/6)

      rankTable$Unique <- t(!apply(rankTable$ID,1,duplicated))
      rownames(rankTable$Unique) <- names(sessionQuery$hashData)
      incProgress(1/6)
      
      rankTable$Distance <- t(apply(distances,1,function(x)sort(x,decreasing = F)))
      rownames(rankTable$Distance) <- names(sessionQuery$hashData)
      incProgress(1/6)
    })
  }
}
# extractMetadata <- function(directory,saveEnvir)
# {
#   print("searching")
#   imgs <- try(list.files(directory,full.names = T,pattern = "\\.JPG$"))
#   if(typeof(imgs) != "try-error" && length(imgs)>0)
#   {
#     metadata <- easyEXIF(list.files(directory,full.names = T,pattern = "\\.JPG$"))
#     metadata <- as.data.frame(do.call(rbind, metadata))
#     colnames(metadata) <- c("ID","Hash","Lat","Lon","Image")
#     assign('metadata',metadata,envir = saveEnvir)
#   }else{
#     showModal(modalDialog(
#       title = paste("No JPG found in:",directory),
#       size = "s",
#       easyClose = TRUE
#     ))
#   }
# }


# --- Server Logic -----------------------------------------------------------------------------------------
# ==================================================================================================================

function(input, output, session) {
  
  # --- stop r from app ui
  session$onSessionEnded(function(){
    stopApp()
  })
  
  sessionReference <- new.env()
  sessionQuery <- new.env()
  sessionStorage <- new.env()
  plotsPanel <- new.env()
  workingImage <- new.env()
  
  #the table query panel is persistant and so is initialized here
  plotsPanel[["TableQuery"]] <- reactiveValues(fin=NULL,
                                               coord=NULL,
                                               locked=TRUE,
                                               mode="default")
  
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
  
  
  # --- clear session memory
  observeEvent(input$clearQuery,{
    sessionQuery$hashData <- NULL
    sessionQuery$traceData <- list()
    sessionQuery$idData <- NULL
  })
  observeEvent(input$clearRef,{
    sessionReference$hashData <- NULL
    sessionReference$traceData <- list()
    sessionReference$idData <- NULL
  })
  observeEvent(c(input$clearRef,input$clearQuery),{
    rankTable$Name=NULL
    rankTable$NameSimple=NULL
    rankTable$CatalogID=NULL
    rankTable$Unique=NULL
    rankTable$Distance=NULL
    rankTable$editCount=0
    
    displayActive$activeSelections <- NULL
    displayActive$lockedSelections <- NULL
    plotsPanel[["TableQuery"]]$fin=NULL
    plotsPanel[["TableQuery"]]$coord=NULL
    plotsPanel[["TableQuery"]]$locked=TRUE#included for consistancy
    plotsPanel[["TableQuery"]]$mode="default"
  })
  
  # --- relable Rdata via finBase csv
  observeEvent(input$labelWithCSV,{
    if(!is.null(input$csvLabeler))
    {
      renameTable <- read.csv(input$csvLabeler$datapath)
      if(all(c("Image","CatalogID") %in% colnames(renameTable)))
      {
        x <- data.frame(Image = as.character(unlist(renameTable['Image'])),ID=renameTable['CatalogID'])
        y <- data.frame(Image=names(sessionQuery$idData),ids=sessionQuery$idData)
        
        if(input$removeForeign)
        {
          correction <- merge(x=x,y=y,by.x='Image', by.y='Image')
        }else{
          correction <- merge(x=x,y=y,by.x='Image', by.y='Image', all.y=T)
        }
        
        sessionQuery$idData <- as.integer(unlist(correction['CatalogID']))
        names(sessionQuery$idData) <- as.character(unlist(correction['Image']))
        
        sessionQuery$hashData <- sessionQuery$hashData[names(sessionQuery$idData)]
        sessionQuery$traceData <- sessionQuery$traceData[names(sessionQuery$idData)]

        rankTable$editCount <- rankTable$editCount+1
      }else{
        showModal(modalDialog(
          title = "Label CSV Error",
          'CSV must contain "Image" and "CatalogID" columns',
          size = "s",
          easyClose = TRUE
        ))
      }
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
  
  observeEvent(input$concatRdata,{
    
    if(dir.exists(input$referenceDirectory) &&
       !is.null(input$referenceDirectory) && 
       input$referenceDirectory != "" && 
       length(input$referenceDirectory)>0)
    {
      concat <- as.environment(as.list(sessionReference, all.names=TRUE))
      names(concat$hashData) <- basename(names(concat$hashData))
      names(concat$traceData) <- basename(names(concat$traceData))
      names(concat$idData) <- basename(names(concat$idData))
      
      save(list = as.character(c("hashData","traceData","idData")),
           file=file.path(input$referenceDirectory,"finFindR.Rdata"),
           envir = concat)
      showModal(modalDialog(
        title = paste("Concatenation Successful"),
        paste(input$referenceDirectory),
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
  readyToRemove <-  reactiveValues(imgName=NULL,
                                   selection=NULL)
  observeEvent(input$finalizeRemove,{
    
    sessionQuery$idData <- sessionQuery$idData[which(names(sessionQuery$idData)!=readyToRemove$imgName)]
    sessionQuery$hashData[readyToRemove$imgName] <- NULL
    sessionQuery$traceData[readyToRemove$imgName] <- NULL
    
    rankTable$Name <- rankTable$Name[names(sessionQuery$idData),]
    rankTable$NameSimple <- rankTable$NameSimple[names(sessionQuery$idData),]
    rankTable$ID <- rankTable$ID[names(sessionQuery$idData),]
    rankTable$Unique <- rankTable$Unique[names(sessionQuery$idData),]
    rankTable$Distance <- rankTable$Distance[names(sessionQuery$idData),]
    
    rankTable$editCount <- rankTable$editCount+1
    
    removeIndex <- which(displayActive$activeSelections==readyToRemove$selected)
    
    if(length(removeIndex)>0)
    {
      hashMapLabel <- strsplit(readyToRemove$selected,": ")[[1]]
      imageRegister <- hashMapLabel[2]
      
      panelID <- gsub("[[:punct:]]", "", hashMapLabel[2])
      panelID <- gsub("[[:space:]]", "", panelID)
      remove(list=as.character(paste(panelID)),envir = plotsPanel)
      
      displayActive$activeSelections <- displayActive$activeSelections[-removeIndex]
    }
    
    
    removeModal(session = getDefaultReactiveDomain())
    print(paste("removed",readyToRemove$imgName))
    readyToRemove$imgName <- NULL
    readyToRemove$selected <- NULL
  })
  
  verifyRemove <- function(name,hashRowSelection)
  {
    # hash selection only significant for cluster view
    if(!is.null(hashRowSelection))
    {
      readyToRemove$selected <- hashRowSelection
    }
    showModal(modalDialog(
      title = "Remove data",
      HTML(paste("Are you sure you want to remove<br>",name,"?")),
      footer = tagList(actionButton("finalizeRemove", "Remove"),modalButton("Cancel")),#actionButton("cancelRemove","Cancel")),
      size = "s",
      easyClose = TRUE
    ))
  }
  prepRemoval <- function(imgSelected,readyToRemove,hashRowSelection=NULL)
  {
    readyToRemove$imgName <- imgSelected
    verifyRemove(imgSelected,hashRowSelection)
  }
  
  
  # --- trace with human input
  readyToRetrace <-  reactiveValues(imgName=NULL,
                                    directory=NULL,
                                    panelID=NULL,
                                    traceResults=list())
  traceGuideCounter <-  reactiveValues(count=-1)
  steps <- c("tip","trailingEnd")
  
  traceGuides <-  reactiveValues(tip=c(NULL,NULL),
                                 trailingEnd=c(NULL,NULL))
  # --- restore defaults upon cancel
  cancelRetrace <- function(readyToRetrace,targetEnvir)
  {
    if(length(readyToRetrace$panelID)>0)
    {
      plotsPanel[[readyToRetrace$panelID]]$mode <- "default"
      
      traceGuideCounter$count <- (-1)
      
      readyToRetrace$imgName <- NULL
      readyToRetrace$panelID <- NULL
      readyToRetrace$directory <- NULL
      readyToRetrace$traceResults <- list()
    }else{
      print("no changes")
      #plotsPanel[[readyToRetrace$panelID]]$mode <- "default"
    }
  }
  
  # --- save trace edit and update tables
  saveRetrace <- function(readyToRetrace,targetEnvir)
  {
    if(length(readyToRetrace$traceResults)>0)
    {
      targetEnvir$traceData[readyToRetrace$imgName] <-  list(readyToRetrace$traceResults$coordinates)
      targetEnvir$hashData[readyToRetrace$imgName] <-  list(traceToHash( list(readyToRetrace$traceResults$annulus )))
      print("retrace hash calculated")
      
      # --- recalculate rank table ------------------
      if(length(sessionReference$hashData)>0)
      {
        newDistances <- distanceToRef( (unlist(targetEnvir$hashData[readyToRetrace$imgName])),
                                       data.frame(sessionReference$hashData))
        newSortingIndex <- order(newDistances)
        
        rankTable$Name[readyToRetrace$imgName,] <- names(sessionReference$idData[newSortingIndex])
        rankTable$NameSimple[readyToRetrace$imgName,] <- basename(names(sessionReference$idData[newSortingIndex]))
        rankTable$ID[readyToRetrace$imgName,] <- sessionReference$idData[newSortingIndex]
        rankTable$Unique[readyToRetrace$imgName,] <- !duplicated(sessionReference$idData[newSortingIndex])
        rankTable$Distance[readyToRetrace$imgName,] <- newDistances[newSortingIndex]
        
        rankTable$editCount <- rankTable$editCount+1
      }
      # ---------------------------------------------
      
      plotsPanel[[readyToRetrace$panelID]]$mode <- "default"
      
      traceGuideCounter$count <- (-1)
      
      readyToRetrace$imgName <- NULL
      readyToRetrace$panelID <- NULL
      readyToRetrace$directory <- NULL
      readyToRetrace$traceResults <- list()
    }else{
      print("skip len-0 trace")
    }
  }
  
  # --- set window to retrace
  prepRetrace <- function(panelID,imageName,targetDir,readyToRetrace)
  {
    plotsPanel[[panelID]]$mode <- "fix"
    
    readyToRetrace$imgName <- imageName
    readyToRetrace$panelID <- panelID
    readyToRetrace$directory <- targetDir
  }
  
  
  observeEvent(input$clickPointSet,{
    if(!is.null(readyToRetrace$imgName))
    {
      traceGuideCounter$count <- (traceGuideCounter$count+1)%%2
      if(traceGuideCounter$count <= 0)
      {
        traceGuides$tip <- c(NULL,NULL)
        traceGuides$trailingEnd <- c(NULL,NULL)
      }
      
      traceGuides[[steps[traceGuideCounter$count+1]]] <- c(round(input$clickPointSet$x,0),
                                                           round(input$clickPointSet$y,0))
      if(traceGuideCounter$count==1)
      {
        startStopPoints <- data.frame(traceGuides$tip,traceGuides$trailingEnd)
        withProgress(message = 'Retracing', value = .5,
                     detail = paste(readyToRetrace$imgName),
                     {
                       
                       traceResults <- try(traceFromImage(load.image(file.path(readyToRetrace$directory,
                                                                             readyToRetrace$imgName)),
                                                        startStopPoints,
                                                        pathNet))
                       incProgress(.25)
                       #needed to fix bug in traceToHash function
                       #names(traceResults)<-NULL
                       #names(traceResults)<-readyToRetrace$imgName
                       readyToRetrace$traceResults <- traceResults
                       
                       
                       print("retraced")
                       if(class(traceResults)!="try-error" && 
                          length(unlist(traceResults)[[1]])>0 &&
                          !is.null(unlist(traceResults)[[1]]))
                       {
                         plotsPanel[[readyToRetrace$panelID]]$coord <- list(traceResults$coordinates)
                         
                         incProgress(.25)
                         print("rendered")
                       }
                     })
      }
    }
  })
  
  # --- set ID
  assignID <- function(panelID,imageName,targetEnvir)
  {
    if(input[[paste0("textID",panelID)]] %in% c(""," "))
    {
      #targetEnvir$idData[imageName] <- "unlabeled"
    }else{
      targetEnvir$idData[imageName] <- input[[paste0("textID",panelID)]]
      rankTable$editCount <- rankTable$editCount+1
    }
  }
  
  
  ##############################################################################################
  #<-><-><-><-> Rank Table <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
  ##############################################################################################
  
  rankTable <- reactiveValues(Name=NULL,
                              NameSimple=NULL,
                              ID=NULL,
                              Unique=NULL,
                              Distance=NULL,
                              editCount=0)
  topMatchPerClass <- function(table,index)
  {
    if(length(table)>0 && length(index)>0)
    {
      if(is.null(table) || is.null(index))
      {
        return(NULL)
      }else{
        
        #if(nrow(index)==1){table <- t(table)}
        sortedIndex <- t(apply(index,1,function(x)order(x,na.last = T,decreasing = T)))
        table[!index] <- NA
        for(i in seq_len(nrow(table))){table[i,] <- table[i,sortedIndex[i,], drop=FALSE]}
        return(table)
      }
    }else{
      return(NULL)
    }
  }
  rankTableUniqueOnly <- reactiveValues(NameSimple=NULL,
                                        ID=NULL,
                                        Distance=NULL)
  observeEvent(rankTable$editCount,{
    rankTableUniqueOnly$NameSimple <- topMatchPerClass(rankTable$NameSimple, rankTable$Unique)
    rankTableUniqueOnly$ID <- topMatchPerClass(rankTable$ID, rankTable$Unique)
    rankTableUniqueOnly$Distance <- topMatchPerClass(rankTable$Distance, rankTable$Unique)
  })
  
  # --- tableQuery panel mod events
  observeEvent(input[[paste0("retrace","TableQuery")]],ignoreInit=T,{
    prepRetrace(panelID="TableQuery",
                imageName=rownames(rankTable$Name)[activeRankTableCell$cell][1],
                targetDir = input$queryDirectory,
                readyToRetrace=readyToRetrace)
  })
  observeEvent(input[[paste0("cancelRetrace","TableQuery")]],ignoreInit=T,{
    cancelRetrace(readyToRetrace=readyToRetrace,
                  targetEnvir=sessionQuery)
    # reset outputs
    plotsPanel[["TableQuery"]]$fin <- file.path(input$queryDirectory, imageNameTableQuery())
    plotsPanel[["TableQuery"]]$coord <- matrix(sessionQuery$traceData[imageNameTableQuery()] ,ncol=2)
    
  })
  observeEvent(input[[paste0("saveRetrace","TableQuery")]],ignoreInit=T,{
    saveRetrace(readyToRetrac=readyToRetrace,
                targetEnvir=sessionQuery)
    
    print("save complete")
    
    # reset outputs
    plotsPanel[["TableQuery"]]$fin <- file.path(input$queryDirectory, imageNameTableQuery())
    plotsPanel[["TableQuery"]]$coord <- matrix(sessionQuery$traceData[imageNameTableQuery()],ncol=2)
    
    print("outputs reset")
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
  traceBatchDone <- reactiveValues(count = 0)
  observeEvent(input$traceBatchQuery,{
    withProgress(message = 'Processing Images', value = 0, 
                 processImageData(input$queryDirectory,sessionQuery,FALSE,pathNet))
    traceBatchDone$count <- traceBatchDone$count+1
  })
  # --- get data out
  observeEvent(input[[paste0("remove","TableQuery")]],{
    prepRemoval(imageNameTableQuery(),readyToRemove)
  })
  
  
  # --- rank table downloads -----------------------------------------
  #NameTable
  output$NameTableDownload <- downloadHandler(
    filename = function() {
      paste0("NameTable",gsub(" ", "_", date()),".csv")
    },
    content = function(file) {
      index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$NameSimple)))
      write.csv(
        if(input$topPerId)
        {
          rankTableUniqueOnly$NameSimple[,index]
        }else{
          rankTable$NameSimple[,index]
        }
      ,file, row.names = T)
    }
  )
  #IDTable
  output$IDTableDownload <- downloadHandler(
    filename = function() {
      paste0("IDTable",gsub(" ", "_", date()),".csv")
    },
    content = function(file) {
      index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$ID)))
      write.csv(
        if(input$topPerId)
        {
          rankTableUniqueOnly$ID[,index]
        }else{
          rankTable$ID[,index]
        }
      ,file, row.names = T)
    }
  )
  #DistanceTable
  output$DistanceTableDownload <- downloadHandler(
    filename = function() {
      paste0("DistanceTable_",gsub(" ", "_", date()),".csv")
    },
    content = function(file) {
      index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$Distance)))
      write.csv(
        if(input$topPerId)
        {
          rankTableUniqueOnly$Distance[,index]
        }else{
          rankTable$Distance[,index]
        }
      ,file, row.names = T)
    }
  )
  
  
  # --- table of ranked matches -----------------------------------------
  
  # --- display specific image
  activeRankTableCell <- reactiveValues(cell=matrix(0,1,2),
                                        delayCell=NULL)
  imageNameTableQuery <- reactive(rownames(rankTable$Name)[activeRankTableCell$cell][1])
  
  observeEvent(input[[paste0("changeID","TableQuery")]],{
    plotsPanel[["TableQuery"]]$mode <- "setID"
  })
  observeEvent(input[[paste0("saveID","TableQuery")]],{
    assignID(panelID="TableQuery",
             imageName=imageNameTableQuery(),
             targetEnvir=sessionQuery)
    
    #neded to update FIX LATER
    output$imageIDTableQuery <- renderText(sessionQuery$idData[imageNameTableQuery()])
    
    plotsPanel[["TableQuery"]]$mode <- "default"
  })
  
  output[[paste0("header","TableQuery")]] <- renderUI({
    if(!is.na(imageNameTableQuery()) && length(imageNameTableQuery())>0 )
    {
      generateDisplayHeader("TableQuery",
                            mode = plotsPanel[["TableQuery"]]$mode,
                            closeOption = F,
                            fixOption = T)
    }
  })
  
  
  output$imageNameTableQuery <- renderText(imageNameTableQuery())
  output$imageIDTableQuery <- renderText(sessionQuery$idData[imageNameTableQuery()])
  
  output$imageTableQuery <- renderPlot({
    print(paste('plot:',plotsPanel[["TableQuery"]]$fin))
    plotFinTrace(load.image(plotsPanel[["TableQuery"]]$fin),
                 plotsPanel[["TableQuery"]]$coord,
                 input$traceTableQuery)
  })
  
  # --- rankTable rendering
  output$matchName <- DT::renderDataTable({
    index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$NameSimple)))
    if(input$topPerId)
    {
      rankTableUniqueOnly$NameSimple[,index, drop=FALSE]
    }else{
      rankTable$NameSimple[,index, drop=FALSE]
    }},
    selection = list(mode="single",target = "cell"),
    options = list(lengthChange = T, rownames=T)
  )
  output$matchID <- DT::renderDataTable({
    index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$ID)))
    if(input$topPerId)
    {
      rankTableUniqueOnly$ID[,index, drop=FALSE]
    }else{
      rankTable$ID[,index, drop=FALSE]
    }},
    selection = list(mode="single",target = "cell"),
    options = list(lengthChange = T, rownames=T)
  )
  output$matchDistance <- DT::renderDataTable({
    print(rankTable$Distance)
    print(dim(rankTable$Distance))
    index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$Distance)))
    if(input$topPerId)
    {
      round(rankTableUniqueOnly$Distance[,index, drop=FALSE],2)
    }else{
      round(rankTable$Distance[,index, drop=FALSE],2)
    }},
    selection = list(mode="single",target = "cell"),
    options = list(lengthChange = T, rownames=T)
  )
  
  # --- rankTable syncronize selection
  proxyNameTbl <- DT::dataTableProxy(outputId="matchName", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = T)
  proxyIDTbl <- DT::dataTableProxy(outputId="matchID", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = T)
  proxyDistanceTbl <- DT::dataTableProxy(outputId="matchDistance", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = T)
  
  proxyFastNameTbl <- DT::dataTableProxy(outputId="matchName", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = F)
  proxyFastIDTbl <- DT::dataTableProxy(outputId="matchID", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = F)
  proxyFastDistanceTbl <- DT::dataTableProxy(outputId="matchDistance", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = F)
  
  
  observeEvent(input$matchName_cell_clicked,
               if(length(input$matchName_cells_selected)==2)
               {
                 activeRankTableCell$cell <- input$matchName_cells_selected
                 
                 selectCells(proxyDistanceTbl,selected=input$matchName_cells_selected)
                 selectCells(proxyIDTbl,selected=input$matchName_cells_selected)
                 
                 selectCells(proxyFastDistanceTbl,selected=input$matchName_cells_selected)
                 selectCells(proxyFastIDTbl,selected=input$matchName_cells_selected)
               }
  )
  observeEvent(input$matchID_cell_clicked,
               if(length(input$matchID_cells_selected)==2)
               {
                 activeRankTableCell$cell <- input$matchID_cells_selected
                 
                 selectCells(proxyNameTbl,selected=input$matchID_cells_selected)
                 selectCells(proxyDistanceTbl,selected=input$matchID_cells_selected)
                 
                 selectCells(proxyFastNameTbl,selected=input$matchID_cells_selected)
                 selectCells(proxyFastDistanceTbl,selected=input$matchID_cells_selected)
               }
  )
  observeEvent(input$matchDistance_cell_clicked,
               if(length(input$matchDistance_cells_selected)==2)
               {
                 activeRankTableCell$cell <- input$matchDistance_cells_selected
                 
                 selectCells(proxyNameTbl,selected=input$matchDistance_cells_selected)
                 selectCells(proxyIDTbl,selected=input$matchDistance_cells_selected)
                 
                 selectCells(proxyFastNameTbl,selected=input$matchDistance_cells_selected)
                 selectCells(proxyFastIDTbl,selected=input$matchDistance_cells_selected)
               }
  )
  # make sure updated when rendered
  observeEvent(input$matchesTblPanel,{
    if(input$matchesTblPanel=="DistanceTab")
    {
      if(length(input$matchDistance_cells_selected)!=2)
      {
        selectCells(proxyDistanceTbl,selected=activeRankTableCell$cell)
      }
    }else if(input$matchesTblPanel=="NameTab"){
      if(length(input$matchName_cells_selected)!=2)
      {
        selectCells(proxyNameTbl,selected=activeRankTableCell$cell)
      }
    }else if(input$matchesTblPanel=="IDTab"){
      if(length(input$matchID_cells_selected)!=2)
      {
        selectCells(proxyIDTbl,selected=activeRankTableCell$cell)
      }
    }
  })
  
  
  
  # --- rankTable calculate distances
  observeEvent(c(input$loadRdataQuery,
                 input$loadRdataRef,
                 traceBatchDone$count), {
                   print("is.null?")
                   if(!is.null(sessionReference$hashData) && 
                      !is.null(sessionQuery$hashData))
                   {
                     print("calculating rank table")
                     calculateRankTable(rankTable,sessionQuery,sessionReference)
                     rankTable$editCount <- rankTable$editCount+1
                   }
                   
                 })
  
  
  
  # --- fin image and overlay of traced path
  observeEvent(c(input$matchName_cell_clicked,
                 input$matchID_cell_clicked,
                 input$matchDistance_cell_clicked),{
                   
                   # QUERY
                   if(!is.null(activeRankTableCell$cell))
                   {
                     # make sure name is new so we dont waste render time
                     if(is.null(plotsPanel[["TableQuery"]]$fin))
                     {
                       plotsPanel[["TableQuery"]]$fin <- file.path(input$queryDirectory, imageNameTableQuery())
                     }else{
                       if(plotsPanel[["TableQuery"]]$fin != file.path(input$queryDirectory, imageNameTableQuery()))
                       {
                         if(plotsPanel[["TableQuery"]]$mode != "default")
                         {
                           #make sure we have a clean slate
                           cancelRetrace(readyToRetrace=readyToRetrace,
                                         targetEnvir=sessionQuery)
                         }

                         plotsPanel[["TableQuery"]]$fin <- file.path(input$queryDirectory, imageNameTableQuery())
                         plotsPanel[["TableQuery"]]$coord <- sessionQuery$traceData[imageNameTableQuery()]
                         
                         
                       }
                     }
                   }
                   
                   if(!is.null(activeRankTableCell$cell)){
                     
                     # REFERENCE
                     output$imageNameTableRef <- renderText(rankTable$NameSimple[activeRankTableCell$cell])
                     output$imageIDTableRef <- renderText(rankTable$ID[activeRankTableCell$cell])
                     
                     output$imageTableRef <- renderPlot({
                       if(length(activeRankTableCell$cell)>1)
                       {
                         print(paste('plot:',rankTable$Name[activeRankTableCell$cell]))
                         plotFinTrace(load.image(rankTable$Name[activeRankTableCell$cell]),
                                      sessionReference$traceData[rankTable$Name[activeRankTableCell$cell]],
                                      input$traceTableRef)
                         

                       }else{
                         NULL
                       }
                     })
                   }
                 })
  
  
  ##############################################################################################
  #<-><-> Hierarchical Clustering <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
  ##############################################################################################
  
  selectedRange <- reactiveValues(y = NULL)
  displayActive <- reactiveValues(activeSelections = NULL,
                                  lockedSelections = NULL)
  
  hashRow <- reactiveValues(names = NULL)
  
  # --- Hash Reference Table
  observeEvent(c(input$loadRdataQuery,
                 input$loadRdataRef,
                 input$clearRef,
                 input$clearQuery,
                 traceBatchDone$count,
                 rankTable$editCount), {
                   selectedRange$y <- c(0, 1)
                   sessionStorage$permutation <- NULL
                   
                   output$hashComparison <- renderPlot(width = "auto",
                                                       height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)),
                                                       {
                                                         if(length(sessionReference$hashData)>0 || length(sessionQuery$hashData)>0)
                                                         {
                                                           allHashData <- append(sessionQuery$hashData,sessionReference$hashData)
                                                           
                                                           hashRow$names <- append(if(length(sessionQuery$hashData)>0){paste(sessionQuery$idData,"Query:",names(sessionQuery$hashData))},
                                                                                   if(length(sessionReference$hashData)>0){paste(sessionReference$idData,"Refer:",names(sessionReference$hashData))})
                                                           
                                                           hashData <- t(data.matrix(data.frame(allHashData)))
                                                           
                                                           rownames(hashData) <- lapply(strsplit(hashRow$names,": "),function(x){paste(x[1],basename(x[2]))})
                                                           
                                                           if(length(sessionStorage$permutation) == 0 || is.null(sessionStorage$permutation))
                                                           {
                                                             par(mar = c(0,0,0,0))
                                                             dendroParams <- heatmap(hashData,
                                                                                     margins = c(1,12),
                                                                                     col = gray.colors(100),
                                                                                     Colv = NA)
                                                             sessionStorage$permutation <- dendroParams$rowInd
                                                           }
                                                           
                                                           indRange <- ceiling(selectedRange$y*length(sessionStorage$permutation))
                                                           indRange[1] <- max(1,indRange[1],na.rm = TRUE)
                                                           indRange[2] <- min(length(sessionStorage$permutation),indRange[2],na.rm = TRUE)
                                                           
                                                           par(mar = c(0,0,0,0))
                                                           
                                                           dendroParams <- heatmap(hashData[sessionStorage$permutation[indRange[1]:indRange[2]],],
                                                                                   margins = c(1,12),
                                                                                   col = gray.colors(100),
                                                                                   Colv = NA)
                                                           
                                                           sessionStorage$permutation <- sessionStorage$permutation[c(indRange[1]:indRange[2])[dendroParams$rowInd]]
                                                         }else{
                                                           NULL
                                                         }
                                                       })
                 })
  
  observeEvent(c(input$dblclickHashMap), {
    brush <- input$brush
    
    if(!is.null(brush)){
      selectedRange$y <- c(brush$ymin, brush$ymax)
    } else {
      #zoom out
      selectedRange$y <- c(0, 1)
      sessionStorage$permutation <- NULL
    }
  })
  
  # --- instance selection
  observeEvent(input$clickHashMap, {
    click <- input$clickHashMap$y
    brush <- input$brush
    
    if(!is.null(input$hover$y) && !is.null(sessionStorage$permutation))
    {
      print(paste("hashMapClick",input$clickHashMap$y))
      if(round(input$clickHashMap$y)<=1 &&
         round(input$clickHashMap$y)>=0)
      {
        
        if (!is.null(brush$ymin)) {
          preInd <- c(brush$ymin, brush$ymax)*(length(sessionStorage$permutation)-1)
          indexRange <- c(floor(mean(preInd)-(plotLim/2)),ceiling(mean(preInd)+(plotLim/2)))
          indexRange[1] <- max(1,indexRange[1],na.rm = TRUE)
          indexRange[2] <- min(length(sessionStorage$permutation)-1,indexRange[2],na.rm = TRUE)
          
          newRender <- hashRow$names[sessionStorage$permutation[indexRange[1]:indexRange[2]]]
          newUniqueIndex <- which(!(newRender %in% displayActive$activeSelections))
          if(length(newUniqueIndex)>0 &&
             input$hover$y==input$clickHashMap$y)
          {
            #FINISH LOCK
            # keepIndex <- displayActive$activeSelections %in% c(newRender[-newUniqueIndex], displayActive$lockedSelections)
            # renderList <- append(displayActive$activeSelections[keepIndex],newRender[newUniqueIndex])
            #displayActive$activeSelectionsp[!keepIndex] <- newRender[newUniqueIndex]
            
            displayActive$activeSelections <- head(append(displayActive$lockedSelections,
                                                          newRender[newUniqueIndex]),plotLim)
            print(displayActive$activeSelections)
          }
        }else{
          
          ind <- round(input$clickHashMap$y*(length(sessionStorage$permutation)-1))
          ind <- max(0,ind,na.rm = TRUE)
          ind <- min(length(sessionStorage$permutation)-1,ind,na.rm = TRUE)
          
          newRender <- hashRow$names[sessionStorage$permutation[ind+1]]
          if(!(newRender %in% displayActive$activeSelections) &&
             input$hover$y==input$clickHashMap$y)
          {
            # keepIndex <- (displayActive$activeSelections %in% displayActive$lockedSelections)
            # print(keepIndex)
            # displayActive$activeSelections <- head(append(displayActive$activeSelections[keepIndex],newRender),plotLim)
            
            displayActive$activeSelections <- head(append(displayActive$lockedSelections,
                                                          newRender),plotLim)
            print(displayActive$activeSelections)
          }
        }
      }
    }
  })
  
  
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
  
  output$displayWindows <- renderUI({
    fluidRow(
      column(width = 6,
             lapply(displayActive$activeSelections[c(TRUE,FALSE)], function(display) {
               if(!is.na(display)){return(windowGenerator(display,plotsPanel))}#else{return(NULL)}
             })
      ),
      column(width = 6,
             lapply(displayActive$activeSelections[c(FALSE,TRUE)], function(display) {
               if(!is.na(display)){return(windowGenerator(display,plotsPanel))}#else{return(NULL)}
             })
      )
    )
  })
  
  
  ##############################################################################################
  #<-><-> Crop <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
  ##############################################################################################
  
  observeEvent(input$cropRawImages,{
    print("cropping")
    cropPath <- normalizePath(input$queryDirectory,"/")
    dir.create(file.path(paste0(cropPath,"_finFindR-Crops")), showWarnings = FALSE)
    print(cropPath)
    cropDirectory(searchDirectory=cropPath,
                  saveDirectory=paste0(cropPath,"_finFindR-Crops"),
                  cropNet,
                  workingImage,
                  minXY=100,
                  includeSubDir=T,
                  mimicDirStructure=T)
  })
}