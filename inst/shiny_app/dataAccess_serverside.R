
getData <- function(directory,column,images=NULL)
{
  conn <- dbConnect(RSQLite::SQLite(), file.path(directory,"finFindR.db"))
  if(is.null(images))
  {
    dbdata <- dbGetQuery(conn, paste0("SELECT ",column," FROM fins"))[,1]
  }else{
    dbdata <- dbGetQuery(conn, paste0("SELECT ",column," FROM fins WHERE name IN ('",paste(images,collapse = "','"),"')"))[,1]
  }
  dbDisconnect(conn)
  gc()
  
  if(column == "hash")
  {
    dbdata <- lapply(dbdata, unserialize)
  }else if(column == "trace"){
    dbdata <- lapply(dbdata, function(x){decodePath(unserialize(x))})
  }
  names(dbdata) <- images
  return(dbdata)
}
setData <- function(directory,column,image_val)
{
  if( is.null(names(image_val)) ){stop("setData requires named list for image_val")}
  if(column == "hash")
  {
    values <- I(lapply(image_val, function(x) { serialize(x, NULL)} ))
  }else if (column == "trace")
  {
    values <- I(lapply(image_val, function(x) {  serialize(encodePath(x), NULL) } ))
  }else{
    values <- image_val
  }
  conn <- dbConnect(RSQLite::SQLite(), file.path(directory,"finFindR.db"))
  for(i in 1:length(image_val))
  {
    if(column == "hash" || column == "trace")
    {
      dbExecute(conn, paste0("UPDATE fins SET ",column," = X'",paste0(values[[i]],collapse = ""),"' WHERE name = '",names(image_val)[i],"'"))
    }else{
      dbExecute(conn, paste0("UPDATE fins SET ",column," = '",values[[i]],"' WHERE name = '",names(image_val)[i],"'"))
    }
  }
  dbDisconnect(conn)
  gc()
}
deleteData <- function(directory,images)
{
  conn <- dbConnect(RSQLite::SQLite(), file.path(directory,"finFindR.db"))
  dbExecute(conn, paste0("DELETE FROM fins WHERE name IN ('",paste(images,collapse = "','"),"')"))
  dbDisconnect(conn)
  gc()
}
loadRdata <- function(directory,saveEnvir,appendNew,isRef)
{
  print("clearing?")
  if(!appendNew){saveEnvir$idData <- NULL;gc()}
  
  # if(!appendNew)
  # {
  #   for(category in c("idData","hashData","traceData"))
  #   {
  #     rm(list=names(saveEnvir[[category]]),envir = saveEnvir[[category]])
  #   }
  #   gc()
  # }
  print("readu")
  withProgress(
    message = "Searching for Rdata Files", value = 0,{
      RdataFiles <- try(list.files(directory, full.names=TRUE, pattern="\\.db$|\\.Rdata$", recursive=appendNew))
      if(typeof(RdataFiles) != "try-error" && length(RdataFiles)>0)
      {
        dirEnvir <- new.env()
        for(RdataFile in unique(dirname(RdataFiles)))
        {
          print(RdataFile)
          
          dirEnvir[[RdataFile]] <- new.env()
          incProgress(1/length(unique(dirname(RdataFiles))))
          
          if(file.exists(file.path(RdataFile,"finFindR.Rdata")) && !file.exists(file.path(RdataFile,"finFindR.db")))
          {
            # create a converted copy of old storage method
            tempEnvir <- new.env()
            load(file.path(RdataFile,"finFindR.Rdata"),tempEnvir)
            print("converting")
            for(imgName in names(tempEnvir$traceData))
            {
              if(class(tempEnvir$traceData[[imgName]])!="integer")
              {
                tempEnvir$traceData[[imgName]] <- encodePath(tempEnvir$traceData[[imgName]])
              }else{
                tempEnvir$traceData[[imgName]] <- tempEnvir$traceData[[imgName]]
              }
            }
            fins <- data.frame(name = as.character(names(tempEnvir$idData)),
                               id = as.character(tempEnvir$idData),
                               trace = I(lapply(tempEnvir$traceData, function(x) { serialize(x, NULL)} ))[names(tempEnvir$idData)],
                               hash = I(lapply(tempEnvir$hashData, function(x) { serialize(x, NULL)} ))[names(tempEnvir$idData)] )
            conn <- dbConnect(RSQLite::SQLite(), file.path(RdataFile,"finFindR.db"))
            dbWriteTable(conn, "fins", fins,overwrite=TRUE)
            dbDisconnect(conn)
            
            print("db created")
            rm(tempEnvir)
            gc()
          }
          if(file.exists(file.path(RdataFile,"finFindR.db")))
          {
            conn <- dbConnect(RSQLite::SQLite(), file.path(RdataFile,"finFindR.db"))
            imageNames <- as.character(dbGetQuery(conn, "SELECT name FROM fins")[,1])
            idData <- as.character(dbGetQuery(conn, "SELECT id FROM fins")[,1])
            dbDisconnect(conn)
            gc()
            
            # names(idData) <- imageNames

            fullImageName <- imageNames
            if(isRef)
            {
              fullImageName <- file.path(RdataFile,fullImageName)
            }
            names(idData) <- fullImageName
            if(isRef)
            {
              saveEnvir$idData <- append(saveEnvir$idData,idData)
            }else{
              saveEnvir$idData <- idData
            }
          }
        }
        rm(dirEnvir)
        gc()
        print("load complete")
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