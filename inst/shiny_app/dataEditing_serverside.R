

verifyRemove <- function(name,readyToRemove,hashRowSelection=NULL)
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
  verifyRemove(imgSelected,readyToRemove,hashRowSelection)
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