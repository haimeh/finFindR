
#' @title traceToHash 
#' @description Function which takes the output of \code{traceFromImage} and returns objects used for matching
#' @return Value of type list containing:
#' "coordinates" a dataframe of coordinates
#' "annulus" a 3 channel image of isolated features
traceToHash <- function(traceData,
                        mxnetModel = NULL)
{
  
  finIter <- getRefClass("finIter",where = as.environment(".finFindREnv"))
  if (is.null(mxnetModel))
  {
    mxnetModel <- mxnet::mx.model.load(file.path( system.file("extdata", package="finFindR"),'fin_triplet32_4096_final'), 5600)
  }
  print("iter")
  iterInputFormat <- sapply(traceData,function(x){as.numeric(resize(x,size_x = 300,interpolation_type = 6))})
  # browser()
  dataIter <- finIter$new(data = iterInputFormat,
                          data.shape = 300)
  print("embed")
  is.mx.dataiter <- function(x) {
      any(is(x, "Rcpp_MXNativeDataIter") || is(x, "Rcpp_MXArrayDataIter"))
  }
  
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

#' @title distanceToRef
#' @description Function to calculate the distances from a single query hash to a reference catalogue
#' @param queryHashData vector containing a hash for matching
#' @param referenceHashData matrix containing a reference catalogue of hashes
#' @export 
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


#' @title distanceToRefParallel 
#' @description Function performing batched matching between a query catalogue to a reference catalogue
#' To call from opencpu, basic format resembles hashes={[0.1, 1.5, 2.2, 3.0],[6.0, 3.3, 4.1, 5.3]}
#' where each vector denotes the featues extracted from an image of a dorsal fin.
#' for example:
#' curl http://localhost:8004/ocpu/library/finFindR/R/distanceToRefParallel/json\
#'  -d "{\"queryHashData\":{\"unknown1\":[1,2,3]},\"referenceHashData\":{\"sal\":[-1,2,4],\"bob\":[-1,-2,-4]}}"\
#'  -H "Content-Type: application/json"  
#' @param queryHashData dataframe (or list) containing the hashes for matching
#' @param referenceHashData dataframe (or list) containing a reference catalogue of hashes
#' @param batchSize int denoting the number of query instances to process at once
#' @param counterEnvir r environment object to hold a progress counter for display purposes
#' @param displayProgressInShiny bool denoting if function is called inside an rshiny instance
#' @return list of two dataframes. The dataframes are formatted as follows:
#' Each row denotes a query image in the same order as provided in the function call. 
#' ie If the first hash in queryHashData was extracted from an image of dolphin "alice", the first row contains matches to dolphin "alice" \n
#' Each column represents a potential match from the referenceHashData, 
#' Columns are ordered by proximity of match from with the closest match being in column 1\n
#' The index refers to the referenceHashData list provided. 
#' If column 1 for dolphin "alice" is 12, then the 12th element in referenceHashData is the best match.
#' "sortingIndex" denotes the element from best to worst match in the reference catalogue.
#' "distances" denotes the distance to the index specified in "sorting Index"
#' @export 
distanceToRefParallel <- function(queryHashData,
                                  referenceHashData,
                                  batchSize = 500,
                                  returnLimit = min(100,length(referenceHashData)),
                                  counterEnvir=new.env(),
                                  displayProgressInShiny=F)
{
  fullQueryIndeces <- seq_len(length(queryHashData))
  queryChunkIndex <- split(fullQueryIndeces, ceiling(seq_along(fullQueryIndeces)/batchSize))
  chunkListIndex <- 1
  mxdistanceChunks <- list()
  sortingIndexChunks <- list()
  
  referenceArray <- mx.nd.expand.dims(data=mx.nd.transpose(
    mx.nd.array(data.matrix(data.frame(referenceHashData)))), axis=2)
  
  for(index in queryChunkIndex)
  {
    queryArrayChunk <- mx.nd.expand.dims(data=mx.nd.transpose(
      mx.nd.array(data.matrix(data.frame(queryHashData[index])))), axis=1)
    
    mxdistance <- mx.nd.sqrt(
      mx.nd.nansum(
        mx.nd.square(
          mx.nd.broadcast.sub(
            lhs = queryArrayChunk,
            rhs = referenceArray
          )
        ),axis = 0
      )
    )
    
    
    # browser()
    sortingIndexChunks[[chunkListIndex]] <- mx.nd.take(mx.nd.argsort(mxdistance,axis = 0)+1,mx.nd.array(0:(returnLimit-1)),axis=0)
    mxdistanceChunks[[chunkListIndex]] <- mx.nd.topk(mxdistance,k=returnLimit,axis = 0,is_ascend = T,ret_typ = 'value')
    
    
    rm(queryArrayChunk,mxdistance)
    gc()
    
    chunkListIndex = chunkListIndex+1
    counterEnvir$progressTicker = counterEnvir$progressTicker+length(index)
    
    if(displayProgressInShiny)
    {
      incProgress(length(index)/counterEnvir$length,
                  detail = paste(counterEnvir$progressTicker,"of",counterEnvir$length),
                  session = counterEnvir$reactiveDomain)
    }
  }
  mxdistances <-  mx.nd.concat(mxdistanceChunks,dim = 1)
  mxsortingIndex <-  mx.nd.concat(sortingIndexChunks,dim = 1)
  
  
  # sortingIndex <- as.data.frame(as.array(mx.nd.transpose(mx.nd.argsort(mxdistances,axis = 0)+1)))
  
  distances <- as.data.frame(as.array(mxdistances))
  sortingIndex <- as.data.frame(as.array(mx.nd.transpose(mxsortingIndex)))

  #clear the nd arrays
  rm(mxdistanceChunks,mxdistances,sortingIndexChunks,mxsortingIndex,referenceArray)
  gc()
  return(list(distances=distances,sortingIndex=sortingIndex))
}

