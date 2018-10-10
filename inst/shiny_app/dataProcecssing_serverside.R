


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
                                                         batch.size=min(ncol(data),128))
                           
                           .self$iter <- array_iter
                           
                           .self$data.shape <- data.shape
                           
                           .self
                         },
                         
                         value=function(){
                           val.x <- as.array(.self$iter$value()$data)
                           val.x[is.na(val.x) | is.nan(val.x) | is.infinite(val.x)]<-(.5)
                           dim(val.x) <- c(data.shape,16,3,ncol(val.x))
                           
                           list(data=mx.nd.array(val.x))
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

traceToHash <- function(traceData,
                        mxnetModel)
{
  iterInputFormat <- sapply(traceData,function(x){as.numeric(resize(x,size_x = 300,interpolation_type = 6))})
  dataIter <- finIter$new(data = iterInputFormat,
                          data.shape = 300)
  print("embed")
  netEmbedding <- mxnet:::predict.MXFeedForwardModel(mxnetModel,
                                                     dataIter,
                                                     array.layout = "colmajor",
                                                     ctx= mx.cpu(),
                                                     allow.extra.params=T)
  rm(dataIter)
  gc()
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
    hashData <- as.data.frame(traceToHash(traceImg,mxnetModel))
    
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

distanceToRef <- function(queryHash,
                          referenceHash)
{
  if(length(referenceHash)>0 && !is.null(queryHash))
  {
    diff <- apply(referenceHash,2,
                 function(x,queryHash)
                 {
                   distance <- sqrt(sum((as.numeric(x)-as.numeric(queryHash))^2))
                   return(if(!is.nan(distance)){distance}else{0})
                 },queryHash=queryHash)
    return(diff)
  }
}



calculateRankTable <- function(rankTable,
                               sessionQuery,
                               sessionReference)
{

  counterEnvir <- new.env()
  counterEnvir$progressTicker <- 0
  counterEnvir$reactiveDomain <- getDefaultReactiveDomain()
  counterEnvir$length <- length(sessionQuery$hashData)
  
  
  withProgress(
    message = 'Matching', value = 0, session = counterEnvir$reactiveDomain,
    {
      fullQueryIndeces <- seq_len(length(sessionQuery$hashData))
      queryChunkIndex <- split(fullQueryIndeces, ceiling(seq_along(fullQueryIndeces)/1000))
      chunkListIndex <- 1
      mxdistanceChunks <- list()
      
      for(index in queryChunkIndex)
      {
        queryChunk <- sessionQuery$hashData[index]
        
        mxdistanceChunks[[chunkListIndex]] <- mx.nd.sqrt(
          mx.nd.nansum(
            mx.nd.square(
              mx.nd.broadcast.sub(
                lhs = mx.nd.expand.dims(data=mx.nd.transpose(
                  mx.nd.array(data.matrix(data.frame(queryChunk)))), axis=1),
                rhs = mx.nd.expand.dims(data=mx.nd.transpose(
                  mx.nd.array(data.matrix(data.frame(sessionReference$hashData)))), axis=2)
              )
            ),axis = 0
          )
        )
        gc()
        
        chunkListIndex = chunkListIndex+1
        counterEnvir$progressTicker = counterEnvir$progressTicker+length(index)
        incProgress(length(index)/counterEnvir$length,
                    detail = paste(counterEnvir$progressTicker,"of",counterEnvir$length),
                    session = counterEnvir$reactiveDomain)
        
      }
      mxdistances <-  mx.nd.concat(mxdistanceChunks,dim = 1)
      #clear the nd arrays
      rm(mxdistanceChunks)
      gc()
      sortingIndex <- as.data.frame(as.array(mx.nd.transpose(mx.nd.argsort(mxdistances,axis = 0)+1)))
      #sortingIndex <- mx.nd.transpose(mx.nd.argsort(mxdistances,axis = 0))
      distances <- as.data.frame(as.array(mxdistances))
      incProgress(0,
                  detail = paste("Matching Complete"),
                  session = counterEnvir$reactiveDomain)
    }
  )
  
  {
    withProgress(
      message = 'Sorting', value = 0,{
        rownames <- paste(names(sessionQuery$hashData),":",sessionQuery$idData)
        
        incProgress(0,detail=paste("file locations"))
        rankTable$Name <- apply(sortingIndex,1,function(x)names(sessionReference$idData)[x])
        simpleNamesVec <- basename(names(sessionReference$idData))
        rankTable$NameSimple <- apply(sortingIndex,1,function(x)simpleNamesVec[x])
        # single queries need to be turned back from vectors
        if(nrow(distances)<=1)
        {
          rankTable$Name <- as.data.frame(t(rankTable$Name))
          rankTable$NameSimple <- as.data.frame(t(rankTable$NameSimple))
        }
        rownames(rankTable$Name) <- rownames
        rownames(rankTable$NameSimple) <- rownames
        
        incProgress(1/4,detail=paste("IDs"))
        rankTable$ID <- apply(sortingIndex,1,function(x)sessionReference$idData[x])
        # single queries need to be turned back from vectors
        if(nrow(distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID))}
        rownames(rankTable$ID) <- rownames
        
        incProgress(1/4,detail=paste("extracting top matches"))
        rankTable$Unique <- t(!apply(rankTable$ID,1,duplicated))
        rownames(rankTable$Unique) <- rownames
        
        incProgress(1/4,detail=paste("distance"))
        rankTable$Distance <- t(apply(distances,1,function(x)sort(x,decreasing = F)))
        rownames(rankTable$Distance) <- rownames
        incProgress(1/4,detail=paste("Done"))
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