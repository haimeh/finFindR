library("shiny")
library("DT")
library("mxnet")
library("imager")
library("finFindR")

options(shiny.maxRequestSize=10*1024^2)
options(stringsAsFactors=FALSE)

appendRecursive <- TRUE
plotLim <- 4

appScripts <- system.file("shiny_app", package="finFindR")
sapply(list.files(path=appScripts,pattern="*_serverside.R",full.names = T),source,.GlobalEnv)

networks <- system.file("extdata", package="finFindR")
pathNet <- mxnet::mx.model.load(file.path(networks,'tracePath128'), 0020)
mxnetModel <- mxnet::mx.model.load(file.path(networks,'fin_triplet32_4096_final'), 5600)
cropNet <- mxnet::mx.model.load(file.path(networks,'cropperInit'), 940)




# --- Server Logic -----------------------------------------------------------------------------------------
# ==================================================================================================================

function(input, output, session) {
  
  # -- get functions used locally
  for (file in list.files(path=appScripts,pattern="*_local.R",full.names = T))
  {
    source(file,local = T)
  }
  
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
      if(length(sessionQuery$idData)>0)
      {
        renameTable <- read.csv(input$csvLabeler$datapath)
        if(all(c("Image","CatalogID") %in% colnames(renameTable)))
        {
          if(nrow(renameTable)==0)
          {
            showModal(modalDialog(
              title = "CSV Format Error",
              'CSV cannot be empty',
              size = "s",
              easyClose = TRUE
            ))
          }else{
          
            x <- data.frame(Image = as.character(unlist(renameTable['Image'])),ID=renameTable['CatalogID'])
            y <- data.frame(Image = names(sessionQuery$idData),ids=sessionQuery$idData)
            
            if(input$removeForeign)
            {
              correction <- merge(x=x,y=y,by.x='Image', by.y='Image')
            }else{
              correction <- merge(x=x,y=y,by.x='Image', by.y='Image', all.y=T)
            }
            
            sessionQuery$idData <- as.character(unlist(correction['CatalogID']))
            names(sessionQuery$idData) <- as.character(unlist(correction['Image']))
            
            sessionQuery$hashData <- sessionQuery$hashData[names(sessionQuery$idData)]
            sessionQuery$traceData <- sessionQuery$traceData[names(sessionQuery$idData)]
    
            rankTable$editCount <- rankTable$editCount+1
          }
        }else{
          showModal(modalDialog(
            title = "CSV Format Error",
            'CSV must contain "Image" and "CatalogID" columns',
            size = "s",
            easyClose = TRUE
          ))
        }
      }else{
        showModal(modalDialog(
          title = "No Session Query Images Available",
          "Please load images for labeling, into the Session Query",
          size = "s",
          easyClose = TRUE
        ))
      }
    }else{
      showModal(modalDialog(
        title = "Label CSV Error",
        'No .csv file selected',
        size = "s",
        easyClose = TRUE
      ))
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
  
  
  # --- trace with human input
  readyToRetrace <-  reactiveValues(imgName=NULL,
                                    directory=NULL,
                                    panelID=NULL,
                                    traceResults=list())
  traceGuideCounter <-  reactiveValues(count=-1)
  steps <- c("tip","trailingEnd")
  
  traceGuides <-  reactiveValues(tip=c(NULL,NULL),
                                 trailingEnd=c(NULL,NULL))
  
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
  

  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #<-><-><-><-> Rank Table <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  rankTable <- reactiveValues(Name=NULL,
                              NameSimple=NULL,
                              ID=NULL,
                              Unique=NULL,
                              Distance=NULL,
                              editCount=0)

  rankTableUniqueOnly <- reactiveValues(NameSimple=NULL,
                                        Name = NULL,
                                        ID=NULL,
                                        Distance=NULL)
  observeEvent(rankTable$editCount,{
    rankTableUniqueOnly$NameSimple <- topMatchPerClass(rankTable$NameSimple, rankTable$Unique)
    rankTableUniqueOnly$Name <- topMatchPerClass(rankTable$Name, rankTable$Unique)
    rankTableUniqueOnly$ID <- topMatchPerClass(rankTable$ID, rankTable$Unique)
    rankTableUniqueOnly$Distance <- topMatchPerClass(rankTable$Distance, rankTable$Unique)
  })
  
  # --- tableQuery panel mod events
  observeEvent(input[[paste0("retrace","TableQuery")]],ignoreInit=T,{
    prepRetrace(panelID="TableQuery",
                imageName= strsplit(rownames(rankTable$Name)[activeRankTableCell$cell][1]," : ")[[1]][1],#strsplit(rownames(rankTable$Name)[activeRankTableCell$cell][1]," : ")[[1]][1],
                rowName = rownames(rankTable$Name)[activeRankTableCell$cell][1],
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
                targetEnvir=sessionQuery,
                mxnetModel=mxnetModel)
    
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
                 processImageData(input$queryDirectory,sessionQuery,FALSE,mxnetModel,pathNet))
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
  # dont forget, we need to split the id from the image name
  imageNameTableQuery <- reactive(strsplit(rownames(rankTable$Name)[activeRankTableCell$cell][1]," : ")[[1]][1])
  
  observeEvent(input[[paste0("changeID","TableQuery")]],{
    plotsPanel[["TableQuery"]]$mode <- "setID"
  })
  
  observeEvent(input[[paste0("saveID","TableQuery")]],{
    assignID(panelID="TableQuery",
             imageName=imageNameTableQuery(),
             rankTable=rankTable,
             activeCell=activeRankTableCell,
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
  tableOptions = list(lengthChange = T, 
                 rownames=T, 
                 ordering=F, 
                 paging = T,
                 scrollY = "500px",
                 pageLength = 1000, lengthMenu = list('500', '1000','2000', '10000'))
  #columnDefs = list(list(targets = c(1:50), searchable = FALSE))
  output$matchName <- DT::renderDataTable({
    index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$NameSimple)))
    if(input$topPerId)
    {
      rankTableUniqueOnly$NameSimple[,index, drop=FALSE]
    }else{
      rankTable$NameSimple[,index, drop=FALSE]
    }},
    selection = list(mode="single",target = "cell"),
    options = tableOptions
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
    options = tableOptions
    )
  output$matchDistance <- DT::renderDataTable({
    index <- seq_len(min(as.integer(input$rankLim),ncol(rankTable$Distance)))
    if(input$topPerId)
    {
      round(rankTableUniqueOnly$Distance[,index, drop=FALSE],2)
    }else{
      round(rankTable$Distance[,index, drop=FALSE],2)
    }},
    selection = list(mode="single",target = "cell"),
    options = tableOptions
    )
  
  # # --- rankTable syncronize selection
  proxyFastNameTbl <- DT::dataTableProxy(outputId="matchName", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = F)
  proxyFastIDTbl <- DT::dataTableProxy(outputId="matchID", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = F)
  proxyFastDistanceTbl <- DT::dataTableProxy(outputId="matchDistance", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = F)
  
  proxyNameTbl <- DT::dataTableProxy(outputId="matchName", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = T)
  proxyIDTbl <- DT::dataTableProxy(outputId="matchID", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = T)
  proxyDistanceTbl <- DT::dataTableProxy(outputId="matchDistance", session = shiny::getDefaultReactiveDomain(),deferUntilFlush = T)
  
  # --- fin image and overlay of traced path
  observeEvent(c(input$matchName_cell_clicked,
                 input$matchID_cell_clicked,
                 input$matchDistance_cell_clicked),{
                   
                   if(length(input$matchName_cells_selected)==2 &&
                      input$matchesTblPanel == "NameTab")
                   {
                     activeRankTableCell$cell <- input$matchName_cells_selected
                     
                     selectCells(proxyDistanceTbl,selected=input$matchName_cells_selected)
                     selectCells(proxyIDTbl,selected=input$matchName_cells_selected)
                     
                     selectCells(proxyFastDistanceTbl,selected=input$matchName_cells_selected)
                     selectCells(proxyFastIDTbl,selected=input$matchName_cells_selected)
                   }
                   
                   if(length(input$matchID_cells_selected)==2 &&
                      input$matchesTblPanel == "IDTab")
                   {
                     activeRankTableCell$cell <- input$matchID_cells_selected
                     
                     selectCells(proxyNameTbl,selected=input$matchID_cells_selected)
                     selectCells(proxyDistanceTbl,selected=input$matchID_cells_selected)
                     
                     selectCells(proxyFastNameTbl,selected=input$matchID_cells_selected)
                     selectCells(proxyFastDistanceTbl,selected=input$matchID_cells_selected)
                   }
                   
                   if(length(input$matchDistance_cells_selected)==2 &&
                      input$matchesTblPanel == "DistanceTab")
                   {
                     activeRankTableCell$cell <- input$matchDistance_cells_selected
                     
                     selectCells(proxyNameTbl,selected=input$matchDistance_cells_selected)
                     selectCells(proxyIDTbl,selected=input$matchDistance_cells_selected)
                     
                     selectCells(proxyFastNameTbl,selected=input$matchDistance_cells_selected)
                     selectCells(proxyFastIDTbl,selected=input$matchDistance_cells_selected)
                   }
                   
                   # QUERY
                   if(!is.null(activeRankTableCell$cell))
                   {
                     plotsPanel[["TableQuery"]]$fin <- file.path(input$queryDirectory, imageNameTableQuery())
                     if(plotsPanel[["TableQuery"]]$mode != "default")
                     {
                       #make sure we have a clean slate
                       cancelRetrace(readyToRetrace=readyToRetrace,
                                     targetEnvir=sessionQuery)
                     }
                     plotsPanel[["TableQuery"]]$coord <- sessionQuery$traceData[imageNameTableQuery()]
                     
                   }
                   
                   if(!is.null(activeRankTableCell$cell)){
                     
                     # REFERENCE
                     if(input$topPerId)
                     {
                       output$imageNameTableRef <- renderText(rankTableUniqueOnly$NameSimple[activeRankTableCell$cell])
                       output$imageIDTableRef <- renderText(rankTableUniqueOnly$ID[activeRankTableCell$cell])
                       
                       output$imageTableRef <- renderPlot({
                         if(length(activeRankTableCell$cell)>1)
                         {
                           print(paste('plot:',rankTableUniqueOnly$Name[activeRankTableCell$cell]))
                           plotFinTrace(load.image(rankTableUniqueOnly$Name[activeRankTableCell$cell]),
                                        sessionReference$traceData[rankTableUniqueOnly$Name[activeRankTableCell$cell]],
                                        input$traceTableRef)
                         }else{
                           NULL
                         }
                       })                     
                       }else{
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
                     
                   }
              })
  
    
  
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
                     calculateRankTable(rankTable=rankTable,
                                        sessionQuery=sessionQuery,
                                        sessionReference=sessionReference)
                     rankTable$editCount <- rankTable$editCount+1
                   }
                 })
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #<-><-> Hierarchical Clustering <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
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
                   
                   if(length(sessionReference$hashData)>0 || length(sessionQuery$hashData)>0)
                   {
                     sessionStorage$permutation <- NULL
                     
                     allHashData <- append(sessionQuery$hashData,sessionReference$hashData)
                     
                     hashRow$names <- append(if(length(sessionQuery$hashData)>0){paste(sessionQuery$idData,"Query:",names(sessionQuery$hashData))},
                                             if(length(sessionReference$hashData)>0){paste(sessionReference$idData,"Refer:",names(sessionReference$hashData))})
                     
                     hashData <- t(data.matrix(data.frame(allHashData)))
                     
                     rownames(hashData) <- hashRow$names#lapply(strsplit(hashRow$names,": "),function(x){paste(x[1],basename(x[2]))})
                     
                     dist_mat <- dist(hashData, method = 'euclidean')
                     hclust_avg <- hclust(dist_mat, method = 'average')
                     sessionStorage$permutation <- hclust_avg$order
                     
                     
                     # create 19 breaks and 20 rgb color values ranging from white to blue
                     tblBreaks <- quantile(hashData, probs = seq(.05, .95, .05), na.rm = TRUE)
                     tblColors <- round(seq(255, 40, length.out = length(tblBreaks) + 1), 0) %>%{paste0("rgb(255,", ., ",", ., ")")}
                     hashTable <- DT::datatable(round(hashData[sessionStorage$permutation,],-1)/10 ) %>% formatStyle(names(hashData), backgroundColor = styleInterval(tblBreaks, tblColors))
                     
                     output$hashComparison <-  DT::renderDataTable(
                       {hashTable},
                       selection = list(mode="single",target = "row"),
                       options = tableOptions
                       )
                     
                     #output$hashComparison <-  DT::renderDataTable({hashData[sessionStorage$permutation,]})
                   }
                 })
  
  # 
  # # --- instance selection
  # observeEvent(input$hashComparison_cell_clicked, {
  #   
  #   if(!is.null(sessionStorage$permutation))
  #   {
  #     newRender <- hashRow$names[sessionStorage$permutation[input$matchName_cells_selected[1]]]
  #     if(!(newRender %in% displayActive$activeSelections))
  #     {
  #       displayActive$activeSelections <- head(append(displayActive$lockedSelections,
  #                                                     newRender),plotLim)
  #     }
  #   }
  # })
  
  
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
    
    labelValue = switch(input$cropTarget,
           "Body&Fin"=c(1,2),
           "Fin"=2)
    
    cropDirectory(searchDirectory=cropPath,
                  saveDirectory=paste0(cropPath,"_finFindR-Crops"),
                  cropNet,
                  workingImage,
                  minXY=100,
                  sensitivity=input$Sensitivity,
                  labelTarget=labelValue,
                  includeSubDir=T,
                  mimicDirStructure=T)
  })
}
