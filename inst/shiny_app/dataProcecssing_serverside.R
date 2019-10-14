


# --- Helper functions -----------------------------------------------------------------------------------------
# ==================================================================================================================


getImgNames <- function(directory,
                        saveEnvir)
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

cropDirectory <- function(searchDirectory,
                          saveDirectory,
                          cropNet,
                          workingImage,
                          minXY=200,
                          sensitivity,
                          labelTarget,
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
                   try(cropFins(imageName=imgName,
                                cropNet=cropNet,
                                workingImage=workingImage,
                                saveDir=file.path(sub('/$','',saveDirectory),sub('^/','',newDirStruct)),
                                minXY=minXY,
                                target=labelTarget,
                                threshold=1-sensitivity))
                   incProgress(1/sum(index), detail = paste(basename(imgName)," -- ",progressTicker,"of",sum(index)))
                   
                 }
               })
}

processImageData <- function(directory,
                             saveEnvir,
                             appendNew,
                             mxnetModel,
                             pathNet)
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
      incProgress(1/length(imgPaths), detail = paste(basename(img)," : ",progressTicker,"of",length(imgPaths)))
      traceResults <- try(traceFromImage(load.image(img),NULL,pathNet))
      if(class(traceResults)!="try-error" && 
         length(unlist(traceResults)[[1]])>0 &&
         !is.null(unlist(traceResults)[[1]]))
      {
        traceImg <- append(traceImg,list(traceResults$annulus))
        traceCoord <- append(traceCoord,list( encodePath(traceResults$coordinates) ))
        
        idData <- append(idData,"unlabeled")
      }else{
        
        print("removed..")
        print(traceResults)
        remove <- append(remove,which(imgPaths==img))
      }
    }
    print(remove)
    if(length(remove)>0){print("removing");imgPaths <- imgPaths[-remove]}
    # hashData <- as.data.frame(traceToHash(traceImg,mxnetModel))
    hashData <- traceToHash(traceImg,mxnetModel)
    
    # name lists of data
    names(idData) <- basename(imgPaths)
    if(!appendNew)
    {
      saveEnvir$idData <- NULL
    }
    saveEnvir$idData <- append(idData,saveEnvir$idData)
    
    fins <- data.frame(name = as.character(basename(imgPaths)),
                       id = as.character(saveEnvir$idData),
                       trace = I(lapply(traceCoord, function(x) { serialize(x, NULL) })),
                       hash = I(lapply(hashData, function(x) { serialize(x, NULL) })) )
    conn <- dbConnect(RSQLite::SQLite(), file.path(unique(dirname(imgPaths)),"finFindR.db"))
    dbWriteTable(conn, "fins", fins,overwrite=TRUE)
    dbDisconnect(conn)
    gc()
  }
}

topMatchPerClass <- function(table,
                             index)
{
  if(length(table)>0 && length(index)>0)
  {
    if(is.null(table) || is.null(index))
    {
      return(NULL)
    }else{
      
      table[!index] <- NA
      sortedIndex <- t(apply(index,1,function(x)order(x,na.last = T,decreasing = T)))
      # table[!index] <- NA
      for(i in seq_len(nrow(table))){table[i,] <- table[i,sortedIndex[i,], drop=FALSE]}
      return(table)
    }
  }else{
    return(NULL)
  }
}


calculateRankTable <- function(rankTable,
                               sessionQuery,
                               sessionReference)
{
  counterEnvir <- new.env()
  counterEnvir$progressTicker <- 0
  counterEnvir$reactiveDomain <- getDefaultReactiveDomain()
  counterEnvir$length <- length(sessionQuery$idData)
  
  
  withProgress(
    message = 'Matching', value = 0, session = counterEnvir$reactiveDomain,
    {
      comparisonResults <- distanceToRefParallel(queryHashData=sessionQuery$gethashData(names(sessionQuery$idData)),
                                                 referenceHashData=sessionReference$gethashData(names(sessionReference$idData)),
                                                 counterEnvir=counterEnvir,
                                                 batchSize = 500,
                                                 displayProgressInShiny=T)
      comparisonResults$sortingIndex <- t(comparisonResults$sortingIndex)
      incProgress(0,
                  detail = paste("Matching Complete"),
                  session = counterEnvir$reactiveDomain)
    }
  )
  
  {
    withProgress(
      message = 'Sorting', value = 0,{
        # browser()
        rownames <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
        
        incProgress(0,detail=paste("file locations"))
        rankTable$Name <- t(apply(comparisonResults$sortingIndex,1,function(x)names(sessionReference$idData)[x]))
        simpleNamesVec <- basename(names(sessionReference$idData))
        incProgress(1/8)
        rankTable$NameSimple <- t(apply(comparisonResults$sortingIndex,1,function(x)simpleNamesVec[x]))
        # single queries need to be turned back from vectors
        if(nrow(comparisonResults$distances)<=1)
        {
          rankTable$Name <- as.data.frame(t(rankTable$Name))
          rankTable$NameSimple <- as.data.frame(t(rankTable$NameSimple))
        }
        rownames(rankTable$Name) <- rownames
        rownames(rankTable$NameSimple) <- rownames
        
        incProgress(1/8,detail=paste("IDs"))
        rankTable$ID <- t(apply(comparisonResults$sortingIndex,1,function(x)sessionReference$idData[x]))
        # single queries need to be turned back from vectors
        if(nrow(comparisonResults$distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID))}
        rownames(rankTable$ID) <- rownames
        
        incProgress(1/4,detail=paste("extracting top class matches"))
        rankTable$Unique <- t(!apply(rankTable$ID,1,duplicated))
        rownames(rankTable$Unique) <- rownames
        
        incProgress(1/4,detail=paste("distance"))
        # rankTable$Distance <- t(apply(comparisonResults$distances,1,function(x)sort(x,decreasing = F)))
        rankTable$Distance <- (comparisonResults$distances)
        rownames(rankTable$Distance) <- rownames
        incProgress(1/4,detail=paste("Done"))
      })
    gc()
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