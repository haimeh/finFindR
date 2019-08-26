

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



loadRdata <- function(directory,saveEnvir,appendNew)
{
  if(!appendNew)
  {
    rm(list=ls(envir = saveEnvir),envir = traceEnv)
    gc()
  }
  withProgress(
    message = "Searching for Rdata Files", value = 0,{
      RdataFiles <- try(list.files(directory, full.names=TRUE, pattern="\\.Rdb$|\\.Rdata$", recursive=appendNew))
      if(typeof(RdataFiles) != "try-error" && length(RdataFiles)>0)
      {
        for(RdataFile in unique(dirname(RdataFiles)))
        {
          saveEnvir[RdataFile] <- new.env()
          
          incProgress(1/length(unique(dirname(RdataFiles))))
          
          if(file.exists(file.path(RdataFile,"finFindR.Rdata")) && !file.exists(file.path(RdataFile,"finFindR.Rdb")))
          {
            # create a converted copy of old storage method
            tempEnvir <- new.env()
            load(file.path(RdataFile,"finFindR.Rdata"),tempEnvir)
            catalogue <- list()
            for (name in names(idData))
            {
              catalogue[paste0(name,"_h")] <- tempEnvir$hashData[name]
              catalogue[paste0(name,"_i")] <- tempEnvir$idData[name]
              if(class(tempEnvir$traceData[name])!="integer")
              {
                catalogue[paste0(name,"_t")] <- encodePath(tempEnvir$traceData[name])
              }else{
                catalogue[paste0(name,"_t")] <- tempEnvir$traceData[name]
              }
            }
            tools:::makeLazyLoadDB(catalogue, file.path(RdataFile,"finFindR"))
            rm(tempEnvir)
            gc()
          }
          if(file.exists(file.path(RdataFile,"finFindR.Rdb")))
          {
            saveEnvir[[RdataFile]]$hashData <- new.env()
            saveEnvir[[RdataFile]]$traceData <- new.env()
            saveEnvir[[RdataFile]]$idData <- new.env()
            
            print(paste(length(dirname(RdataFiles)),isRef,"of",file.path(RdataFile,"finFindR.Rdb")))
            
            lazyLoad(file.path(RdataFile,"finFindR"),env=saveEnvir[[RdataFile]]$hashData,filter = function(x)endsWith(x,"_h"))
            lazyLoad(file.path(RdataFile,"finFindR"),env=saveEnvir[[RdataFile]]$traceData,filter = function(x)endsWith(x,"_t"))
            lazyLoad(file.path(RdataFile,"finFindR"),env=saveEnvir[[RdataFile]]$idData,filter = function(x)endsWith(x,"_i"))
            
          }
        }
        
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