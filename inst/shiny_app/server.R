library("shiny")
library("DT")
library("imager")
library("finFindR")
#NOTE: dont forget to removee
#sapply(list.files(path="../../R/",pattern="*.R$",full.names = T),source,.GlobalEnv);library("Rcpp");sapply(list.files(path="../../src/",pattern="*.cpp$",full.names = T),function(x){sourceCpp(x,rebuild=T,verbose=T)})

options(shiny.maxRequestSize=10*1024^2)
options(stringsAsFactors=FALSE)

appendRecursive <- TRUE
plotLim <- 4

appScripts <- system.file("shiny_app", package="finFindR")
sapply(list.files(path=appScripts,pattern="*_serverside.R",full.names = T),source,.GlobalEnv)
#sapply(list.files(path=".",       pattern="*_serverside.R",full.names = T),source,.GlobalEnv)

networks <- system.file("extdata", package="finFindR")
#networks <- "../extdata"
#load(file.path(networks,"hashSVD.Rdata"))
load(file.path(system.file("extdata", package="finFindR"),"hashSVD.Rdata"))

#pathNet <- mxnet::mx.model.load(file.path(networks,'SWA_finTrace_fin'), 1000)
pathNet <- mxnet::mx.model.load(file.path(networks,             'SWA_cont2_traceLong7_bn_6,10,5_RGB_fin'), 0000)
#pathNet <- mxnet::mx.model.load(file.path("../../inst/extdata",'SWA_cont2_traceLong7_bn_6,10,5_RGB_fin'), 0000)
#pathNet <- mxnet::mx.model.load(file.path("../../inst/extdata",'prime_traceLong2_bn_6,10,5_RGB_fin'), 0000)

cropNet <- mxnet::mx.model.load(file.path(networks,'cropperInit'), 941)
mxnetModel <- mxnet::mx.model.load(file.path(networks,'fin_triplet32_4096_final'), 5600)


# --- Server Logic -----------------------------------------------------------------------------------------
# ==================================================================================================================

function(input, output, session) {
	
	# -- get functions used locally
	#for (file in list.files(path=".",pattern="*_local.R",full.names = T))
	#{
	#	source(file,local = T)
	#}
	for (file in list.files(path=appScripts,pattern="*_local.R",full.names = T))
	{
		source(file,local = T)
	}
  #load(file.path(system.file("extdata", package="finFindR"),"hashSVD.Rdata"))
  #print(hashSVD$U)
  
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
												focusedCoord=NULL,
												locked=TRUE,
												mode="default")
	
	
	finPartCombos <- unique(lapply(apply(expand.grid(rep(list(c("Trailing","Leading","Peduncle")),3)),1,unique),sort))

	#ifelse(length(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures))==0,finPartCombos[[1]][1],ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures))
	#observeEvent(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures),{
	#	#print(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures))
	#	#if(is.null(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures))){
	#	if(length(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures))==0){
	#	updateCheckboxGroupInput(session,
	#							inputId = "targetFeatures",
	#							label = "Select target features for comparisons",
	#							choices = c("Trailing","Leading","Peduncle"),
	#							selected="Trailing",
	#	)
	#	}
	#})
	# --- clear session memory
	observeEvent(input$clearQuery,{
		for(finPart in c("Trailing","Leading","Peduncle")){
			sessionQuery$hashData[[finPart]] <- NULL
			sessionQuery$traceData[[finPart]] <- list()
		}
		for(finParts in finPartCombos){

			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- NULL
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Name <- NULL
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$ID <- NULL
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Distance <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$Name <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$ID <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance <- NULL
		}

		rankTable$editCount <- rankTable$editCount+1
		sessionQuery$idData <- NULL
	})
	observeEvent(input$clearRef,{
		for(finPart in c("Trailing","Leading","Peduncle")){
			sessionReference$hashData[[finPart]] <- NULL
			sessionReference$traceData[[finPart]] <- list()
		}
		for(finParts in finPartCombos){

			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- NULL
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Name <- NULL
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$ID <- NULL
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Distance <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$Name <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$ID <- NULL
			rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance <- NULL
		}
		rankTableUniqueOnly$NameSimple <- NULL
		rankTableUniqueOnly$Name <- NULL
		rankTableUniqueOnly$ID <- NULL
		rankTableUniqueOnly$Distance <- NULL
		rankTable$NameSimple <- NULL
		rankTable$Name <- NULL
		rankTable$ID <- NULL
		rankTable$Distance <- NULL

		rankTable$editCount <- rankTable$editCount+1
		sessionReference$idData <- NULL
	})

	rankTableParts <- new.env()
	rankTableUniqueOnlyParts <- new.env()
	for(finParts in finPartCombos){
		rankTableParts[[paste0(sort(finParts),collapse="")]] = reactiveValues(Name=NULL,
									NameSimple=NULL,
									ID=NULL,
									Unique=NULL,
									Distance=NULL,
									editCount=0)
		rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]] <- reactiveValues(NameSimple=NULL,
									Name = NULL,
									ID=NULL,
									Distance=NULL)
	}
		#rankTableParts$Trailing = reactiveValues(Name=NULL,
		#							NameSimple=NULL,
		#							ID=NULL,
		#							Unique=NULL,
		#							Distance=NULL,
		#							editCount=0)
		#rankTableParts$Leading = reactiveValues(Name=NULL,
		#							NameSimple=NULL,
		#							ID=NULL,
		#							Unique=NULL,
		#							Distance=NULL,
		#							editCount=0)
		#rankTableParts$Peduncle = reactiveValues(Name=NULL,
		#							NameSimple=NULL,
		#							ID=NULL,
		#							Unique=NULL,
		#							Distance=NULL,
		#							editCount=0)
	
	#rankTableUniqueOnlyParts <- new.env()
	#rankTableUniqueOnlyParts$Trailing <- reactiveValues(NameSimple=NULL,
	#							Name = NULL,
	#							ID=NULL,
	#							Distance=NULL)
	#rankTableUniqueOnlyParts$Leading <- reactiveValues(NameSimple=NULL,
	#							Name = NULL,
	#							ID=NULL,
	#							Distance=NULL)
	#rankTableUniqueOnlyParts$Peduncle <- reactiveValues(NameSimple=NULL,
	#							Name = NULL,
	#							ID=NULL,
	#							Distance=NULL)
	

	observeEvent(c(input$clearRef,input$clearQuery),{
		rankTable$Name=NULL
		rankTable$NameSimple=NULL
		rankTable$CatalogID=NULL
		rankTable$Unique=NULL
		rankTable$Distance=NULL
		rankTableUniqueOnly$NameSimple <- NULL
		rankTableUniqueOnly$Name <- NULL
		rankTableUniqueOnly$ID <- NULL
		rankTableUniqueOnly$Distance <- NULL
		#for(finPart in c("Trailing","Leading","Peduncle")){
		#	DistanceParts[[finPart]]=NULL
		#}
		rankTable$editCount=0
		
		displayActive$activeSelections <- NULL
		displayActive$lockedSelections <- NULL
		plotsPanel[["TableQuery"]]$fin=NULL
		plotsPanel[["TableQuery"]]$coord=NULL
		plotsPanel[["TableQuery"]]$focusedCoord=NULL
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
							correction <- merge(x=x,y=y,by.x='Image', by.y='Image', all.y=F, all.x=F)
						}else{
							correction <- merge(x=x,y=y,by.x='Image', by.y='Image', all.y=T, all.x=F)
						}
						
						sessionQuery$idData <- as.character(unlist(correction['CatalogID']))
						names(sessionQuery$idData) <- as.character(unlist(correction['Image']))
						
						print("renaming")
						for(finPart in c("Trailing","Leading","Peduncle")){
							#TODO: fix name things!!! almost done!
							#index <- names(sessionQuery$idData) %in% names(sessionQuery$hashData[[finPart]])
							#sessionQuery$hashData[[finPart]] <- sessionQuery$hashData[[finPart]][names(sessionQuery$idData)[index]]
							#sessionQuery$traceData[[finPart]] <- sessionQuery$traceData[[finPart]][names(sessionQuery$idData)[index]]
							sessionQuery$hashData[[finPart]] <- sessionQuery$hashData[[finPart]][names(sessionQuery$idData)]
							sessionQuery$traceData[[finPart]] <- sessionQuery$traceData[[finPart]][names(sessionQuery$idData)]
						}
						#sessionQuery$hashData <- sessionQuery$hashData[names(sessionQuery$idData)]
						#sessionQuery$traceData <- sessionQuery$traceData[names(sessionQuery$idData)]

						if(input$removeForeign)
						{
							#missedForeignIndex <- which(!is.na(sessionQuery$idData) | is.nan(sessionQuery$idData))
							missedForeignIndex <- which(!(is.na(sessionQuery$idData) | is.nan(sessionQuery$idData)))
							for(finPart in c("Trailing","Leading","Peduncle")){
								sessionQuery$hashData[[finPart]] <- sessionQuery$hashData[[finPart]][missedForeignIndex]
								sessionQuery$traceData[[finPart]] <- sessionQuery$traceData[[finPart]][missedForeignIndex]
							}
							sessionQuery$idData <- sessionQuery$idData[missedForeignIndex]
						}
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
			gc()
			concat <- as.environment(as.list(sessionReference, all.names=TRUE))

			for(finPart in c("Trailing","Leading","Peduncle")){
				names(sessionQuery$hashData[[finPart]]) <- basename(names(concat$hashData[[finPart]]))
				names(sessionQuery$traceData[[finPart]]) <- basename(names(concat$traceData[[finPart]]))
			}
			#names(concat$hashData) <- basename(names(concat$hashData))
			#names(concat$traceData) <- basename(names(concat$traceData))
			names(concat$idData) <- basename(names(concat$idData))
			
			save(list = as.character(c("hashData","traceData","idData")),
					file=file.path(input$referenceDirectory,"finFindR.Rdata"),
					envir = concat)
			rm(concat)
			gc()
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
	



#rownames <- list(Trailing=NULL,Leading=NULL,Peduncle=NULL)
#for(finPart in c("Trailing","Leading","Peduncle")){
#	rankTableParts[[finPart]]$Name      
#	rankTableParts[[finPart]]$NameSimple
#	rankTableParts[[finPart]]$ID        
#	rankTableParts[[finPart]]$Unique    
#	rankTableParts[[finPart]]$Distance  
#}




	# --- Remove entry
	#TODO: finish fixing remove enty
	#NOTE: image not getting removed????
	readyToRemove <- reactiveValues(imgName=NULL,
									selected=NULL)
									#selection=NULL)
	observeEvent(input$finalizeRemove,{
		
		rownamesTmp <-  sessionQuery$idData[which(names(sessionQuery$idData)==readyToRemove$imgName)]
		rownames <- paste(names(rownamesTmp),":",rownamesTmp)
		
		sessionQuery$idData <- sessionQuery$idData[which(names(sessionQuery$idData)!=readyToRemove$imgName)]
		for(finPart in c("Trailing","Leading","Peduncle")){
			#if(!is.null(sessionQuery$hashData[[input$segmentTarget]][readyToRemove$imgName])){
			sessionQuery$hashData[[finPart]][readyToRemove$imgName] <- NULL
			sessionQuery$traceData[[finPart]][readyToRemove$imgName] <- NULL
			#}
		}

		#rownames <- paste(names(sessionQuery$hashData[[input$segmentTarget]]),":",sessionQuery$idData)
		
		#for(finPart in c("Trailing","Leading","Peduncle")){
		for(finParts in finPartCombos){
			rankTableParts[[paste0(sort(finParts),collapse="")]]$Name       <- rankTableParts[[paste0(sort(finParts),collapse="")]]$Name      [rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Name      ) != rownames,]
			rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple[rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple) != rownames,]
			rankTableParts[[paste0(sort(finParts),collapse="")]]$ID         <- rankTableParts[[paste0(sort(finParts),collapse="")]]$ID        [rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$ID        ) != rownames,]
			rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique     <- rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique    [rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique    ) != rownames,]
			rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance   <- rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance  [rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance  ) != rownames,]

			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Name       <- rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Name      [rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Name      ) != rownames,]
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$NameSimple[rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$NameSimple) != rownames,]
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$ID         <- rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$ID        [rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$ID        ) != rownames,]
			rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Distance   <- rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Distance  [rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Distance  ) != rownames,]
		}
		
		#rankTable$Name <- rankTable$Name[rownames,]
		#rankTable$NameSimple <- rankTable$NameSimple[rownames,]
		#rankTable$ID <- rankTable$ID[rownames,]
		#rankTable$Unique <- rankTable$Unique[rownames,]
		
		#updateDisplayTables(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures),rankTableParts,rankTable )
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
		print("doneRemove")
	})




	
	
	# --- trace with human input
	readyToRetrace <-  reactiveValues(imgName=NULL,
										directory=NULL,
										panelID=NULL,
										traceResults=list())
	traceGuideCounter <-	reactiveValues(count=-1)
	#steps <- c("tip","trailingEnd")
	steps <- c("start","end")
	
	traceGuides <- reactiveValues(tip=c(NULL,NULL),
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
				#if( sum(traceGuides[[1]]-traceGuides[[2]])>5 ){
				startStopPoints <- data.frame(traceGuides$start,traceGuides$end)
				withProgress(message = 'Retracing', value = .5,
							detail = paste(readyToRetrace$imgName),
							{
								#TODO: add input[[which edge selecteed]]
								selectedChan <- c("Peduncle"=2,"Trailing"=4,"Leading"=6)

								traceResults <- try(traceFromImage(load.image(file.path(readyToRetrace$directory,
																				readyToRetrace$imgName)),
																	startStopPoints,
																	pathNet,
																	selectedChan[input$segmentTarget]))
																	#(input$segmentTarget=="Trailing")))
								incProgress(.25)
								#needed to fix bug in traceToHash function
								#names(traceResults)<-NULL
								#names(traceResults)<-readyToRetrace$imgName
								readyToRetrace$traceResults <- traceResults
								
								#TODO: Warning: Error in <-: object 'panelID' not found

								print("retraced")
								if(class(traceResults)!="try-error" && 
									length(unlist(traceResults)[[1]])>0 &&
									!is.null(unlist(traceResults)[[1]]))
								{
									plotsPanel[[readyToRetrace$panelID]]$focusedCoord[[input$segmentTarget]] <- encodePath(traceResults$coordinates)
									#plotsPanel[[readyToRetrace$panelID]]$focusedCoord <- traceResults$coordinates
									#plotsPanel[[readyToRetrace$panelID]]$coord[input$segmentTarget][targetImage] == encodePath(traceResults$coordinates)
									#targetImage <- names(plotsPanel[[readyToRetrace$panelID]]$coord[input$segmentTarget])

#plotsPanel[[panelID]]$coord <- lapply(targetEnvir$traceData,function(x){x[imageRegister]})
#plotsPanel[[readyToRetrace$panelID]]$coord[[input$segmentTarget]][[targetImage]] <- encodePath(traceResults$coordinates)
#plotsPanel[[readyToRetrace$panelID]]$coord[[input$segmentTarget]][[targetImage]] <- 0
									print(paste('panelID_retrace:',readyToRetrace$panelID))
#plotsPanel[[readyToRetrace$panelID]]$coord[[input$segmentTarget]] <- encodePath(traceResults$coordinates)
									#plotsPanel[[panelID]]$coord <- lapply(targetEnvir$traceData,function(x){x[imageRegister]})
									#names(plotsPanel[[panelID]]$coord) <- names(targetEnvir$traceData)
									
									incProgress(.25)
									print("rendered")
								}
							})
				#}else{	plotsPanel[[readyToRetrace$panelID]]$focusedCoord[[input$segmentTarget]] = NULL}
			}

		}
	})
	
	
#access_rv = function(rv,field){
#	isolate(purrr::pluck(rv, !!!field))
#}
#
#access_rv(rv,c("a1","b1")) # works, return "b1"
	
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	#<-><-><-><-> Rank Table <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	# TODO: add distancees for other fin components?
	rankTable <- reactiveValues(Name=NULL,
								NameSimple=NULL,
								ID=NULL,
								Unique=NULL,
								Distance=NULL,
								editCount=0)
	#rankTableParts <- list(
	#	Trailing = reactiveValues(Name=NULL,
	#								NameSimple=NULL,
	#								ID=NULL,
	#								Unique=NULL,
	#								Distance=NULL,
	#								editCount=0),
	#	Leading = reactiveValues(Name=NULL,
	#								NameSimple=NULL,
	#								ID=NULL,
	#								Unique=NULL,
	#								Distance=NULL,
	#								editCount=0),
	#	Peduncle = reactiveValues(Name=NULL,
	#								NameSimple=NULL,
	#								ID=NULL,
	#								Unique=NULL,
	#								Distance=NULL,
	#								editCount=0)
	#)
	
	rankTableUniqueOnly <- reactiveValues(NameSimple=NULL,
											Name = NULL,
											ID=NULL,
											Distance=NULL)
	#rankTableUniqueOnlyParts <- list(
	#Trailing <- reactiveValues(NameSimple=NULL,
	#							Name = NULL,
	#							ID=NULL,
	#							Distance=NULL),
	#Leading <- reactiveValues(NameSimple=NULL,
	#							Name = NULL,
	#							ID=NULL,
	#							Distance=NULL),
	#Peduncle <- reactiveValues(NameSimple=NULL,
	#							Name = NULL,
	#							ID=NULL,
	#							Distance=NULL)
	#)

	observeEvent(rankTable$editCount,{
		#req(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures))
		#for(finPart in c("Trailing","Leading","Peduncle")){
		#for(finParts in finPartCombos){
		for(finParts in sapply(finPartCombos,function(x){any(input$segmentTarget==x)})){
		rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- topMatchPerClass(rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple, rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique)
		rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Name <- topMatchPerClass(rankTableParts[[paste0(sort(finParts),collapse="")]]$Name, rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique)
		rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$ID <- topMatchPerClass(rankTableParts[[paste0(sort(finParts),collapse="")]]$ID, rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique)

		#rankTableUniqueOnlyParts[[finPart]]$NameSimple <- topMatchPerClass(rankTable$NameSimple, rankTable$Unique)
		#rankTableUniqueOnlyParts[[finPart]]$Name <- topMatchPerClass(rankTable$Name, rankTable$Unique)
		#rankTableUniqueOnlyParts[[finPart]]$ID <- topMatchPerClass(rankTable$ID, rankTable$Unique)

		#tmpDistance <- as.list(rep(NA,length(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures))))
		#names(tmpDistance) <- ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)
		#for(finPart in ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)){
		#	tmpDistance[[finPart]] <- DistanceParts[[finPart]]
		#}
		#rankTable$Distance <- Reduce('+', tmpDistance[ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)])
		#rankTableUniqueOnlyParts[[finPart]]$Distance <- topMatchPerClass(rankTable$Distance, rankTable$Unique)
		rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Distance <- topMatchPerClass(rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance, rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique)

		
		}
		#updateDisplayTables(finPartSelected=ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures), rankTableUniqueOnlyParts, rankTableUniqueOnly)
		#updateDisplayTables(finPartSelected=ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures), rankTableParts, rankTable)
		# distance ensujres something is in
		# maybe this can be done more efficiently..
		if(!is.null(rankTable$ID) && !is.null(rankTableUniqueOnly$ID))
		{
			#rownames(rankTable$Name) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			#rownames(rankTable$NameSimple) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			#rownames(rankTable$ID) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			#rownames(rankTable$Unique) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			#rownames(rankTable$Distance) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)

			##for(finPart in c("Trailing","Leading","Peduncle")){
			##	rownames(DistanceParts[[finPart]]) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			##	rownames(DistancePartsUniqueOnly[[finPart]]) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			##}

			#rownames(rankTableUniqueOnly$Name) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			#rownames(rankTableUniqueOnly$NameSimple) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			#rownames(rankTableUniqueOnly$ID) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)
			#rownames(rankTableUniqueOnly$Distance) <- paste(names(sessionQuery$idData),":",sessionQuery$idData)


			#rownames <- list(Trailing=NULL,Leading=NULL,Peduncle=NULL)
			##for(finPart in c("Trailing","Leading","Peduncle")){
			#for(finParts in finPartCombos){

			#	#queryNamesPrev <- names(queryTmp[["Trailing"]])
			#	#queryNames <- names(sessionQuery$hashData[[finPart]])
			#	#querySharedNames <- merge(x=cbind(queryNamesPrev,1), y=cbind(queryNames,1), by.x="queryNamesPrev", by.y="queryNames",  all.y=F, all.x=F)[,1]
			#	#queryTmp[[finPart]] <- lapply(querySharedNames,function(i){append(queryTmp[["Trailing"]][[i]],
			#	#													sessionQuery$hashData[[finPart]][[i]][1:8])})
			#	#names(queryTmp[[finPart]]) <- querySharedNames

			#	#referNamesPrev <- names(referTmp[["Trailing"]])
			#	#referNames <- names(sessionReference$hashData[[finPart]])
			#	#referSharedNames <- merge(x=cbind(referNamesPrev,1), y=cbind(referNames,1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]
			#	#referTmp[[finPart]] <- lapply(referSharedNames,function(i){append(referTmp[["Trailing"]][[i]],
			#	#													sessionReference$hashData[[finPart]][[i]][1:8])})
			#	#names(referTmp[[finPart]]) <- referSharedNames

			#		#browser()
			#	querySharedNames <- merge(x=cbind(queryNamesPrev=names(sessionQuery$hashData[["Trailing"]]),1),     y=cbind(queryNames=names(sessionQuery$hashData[[finPart]]),1),     by.x="queryNamesPrev", by.y="queryNames",  all.y=F, all.x=F)[,1]
			#	#referSharedNames <- merge(x=cbind(referNamesPrev=names(sessionReference$hashData[["Trailing"]]),1), y=cbind(referNames=names(sessionReference$hashData[[finPart]]),1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]

			#	tmpRowNames <- names(sessionQuery$idData[querySharedNames])
			#	#tmpRowNames <- names(sessionQuery$idData[names(sessionQuery$hashData[[finPart]])])
			#	rownames[[paste0(sort(finParts),collapse="")]] <- paste(tmpRowNames,":",sessionQuery$idData[tmpRowNames])
			#	#tmpRowNames <- names(sessionQuery$hashData[[finPart]])[!is.null(sessionQuery$hashData[[finPart]])]
			#	#rownames <- paste(tmpRowNames,":",sessionQuery$idData[tmpRowNames])

			#	rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$NameSimple) <- rownames[[paste0(sort(finParts),collapse="")]]
			#	rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Name) <- rownames[[paste0(sort(finParts),collapse="")]]
			#	rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$ID) <- rownames[[paste0(sort(finParts),collapse="")]]
			#	rownames(rankTableUniqueOnlyParts[[paste0(sort(finParts),collapse="")]]$Distance) <- rownames[[paste0(sort(finParts),collapse="")]]

			#	rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple) <- rownames[[paste0(sort(finParts),collapse="")]]
			#	rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Name) <- rownames[[paste0(sort(finParts),collapse="")]]
			#	rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$ID) <- rownames[[paste0(sort(finParts),collapse="")]]
			#	rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance) <- rownames[[paste0(sort(finParts),collapse="")]]
			#}
			updateDisplayTables(finPartSelected=paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],list(input$targetFeatures))[[1]]),collapse=""), rankTableUniqueOnlyParts, rankTableUniqueOnly)
			updateDisplayTables(finPartSelected=paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],list(input$targetFeatures))[[1]]),collapse=""), rankTableParts, rankTable)
		}
	})

	# --- clear segment
	observeEvent(input[[paste0("removeSegment","TableQuery")]],ignoreInit=T,{
			showModal(modalDialog(
				title = paste("Are you sure you want to remove",input$segmentTarget,"segment?"),
				size = "s",
				footer=tagList(actionButton("confirmSegmentRemove","Yes"),modalButton("Cancel")),
				easyClose = F
			))
	})
	observeEvent(input$confirmSegmentRemove,{
		removeModal()
		sessionQuery$traceData[[paste0(sort(input$segmentTarget),collapse="")]][[imageNameTableQuery()]] <- c(0,0,4,0)
		sessionQuery$hashData[[paste0(sort(input$segmentTarget),collapse="")]][[imageNameTableQuery()]] <- NULL
		plotsPanel[["TableQuery"]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageNameTableQuery()]]})

		rownamesTmp <-  sessionQuery$idData[which(names(sessionQuery$idData)==imageNameTableQuery())]
		rownames <- paste(names(rownamesTmp),":",rownamesTmp)
		#sessionQuery$idData <- sessionQuery$idData[which(names(sessionQuery$idData)!=imageNameTableQuery())]
		#sessionQuery$hashData[[input$segmentTarget]][imageNameTableQuery()] <- NULL
		#sessionQuery$traceData[[input$segmentTarget]][imageNameTableQuery()] <- NULL
		rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name       <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      ) != rownames,]
		rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple[rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple) != rownames,]
		rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID         <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        ) != rownames,]
		rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Unique     <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Unique    [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Unique    ) != rownames,]
		rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance   <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  ) != rownames,]

		rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name       <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      [rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      ) != rownames,]
		rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple[rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple) != rownames,]
		rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID         <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        [rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        ) != rownames,]
		rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance   <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  [rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  ) != rownames,]

		activeRankTableCell$cell <- matrix(1,1,2)
		#updateDisplayTables(finPartSelected=ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures), rankTableParts, rankTable)
		rankTable$editCount <- rankTable$editCount+1
	})
	
	# --- tableQuery panel mod events
	observeEvent(input[[paste0("retrace","TableQuery")]],ignoreInit=T,{
			print("retraceTableQuery493")
		plotsPanel[["TableQuery"]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageNameTableQuery()]]})
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
		#plotsPanel[["TableQuery"]]$coord <- matrix(sessionQuery$traceData[imageNameTableQuery()] ,ncol=2)
		#plotsPanel[["TableQuery"]]$coord <- lapply(sessionQuery$traceData,function(x){x[imageNameTableQuery()]})
		#names(plotsPanel[["TableQuery"]]$coord) <- names(sessionQuery$traceData)

		#plotsPanel[["TableQuery"]]$coord <- do.call(rbind,lapply(sessionQuery$traceData,function(x){decodePath(unlist(x[[imageNameTableQuery()]]))}))

		print("509")
		coordTmp <- lapply(sessionQuery$traceData,function(x){decodePath(unlist(x[[imageNameTableQuery()]]))})
		plotsPanel[["TableQuery"]]$coord <- do.call(rbind,coordTmp[!apply(1>=sapply(coordTmp,dim),2,prod)])
		plotsPanel[["TableQuery"]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageNameTableQuery()]]})
	})

	observeEvent(input[[paste0("saveRetrace","TableQuery")]],ignoreInit=T,{
		print("saveretrace tablequery")
		print(length(readyToRetrace))
		saveRetrace(readyToRetrace=readyToRetrace,
					targetEnvir=sessionQuery,
					mxnetModel=mxnetModel)
		
		print("save complete")
		
		# reset outputs
		plotsPanel[["TableQuery"]]$fin <- file.path(input$queryDirectory, imageNameTableQuery())
		#plotsPanel[["TableQuery"]]$coord <- matrix(sessionQuery$traceData[imageNameTableQuery()],ncol=2)
		#plotsPanel[["TableQuery"]]$coord <- lapply(sessionQuery$traceData,function(x){x[imageNameTableQuery()]})
		#names(plotsPanel[["TableQuery"]]$coord) <- names(sessionQuery$traceData)

		#plotsPanel[["TableQuery"]]$coord <- do.call(rbind,lapply(sessionQuery$traceData,function(x){decodePath(unlist(x[[imageNameTableQuery()]]))}))


		#hashMapLabel <- strsplit(selection,": ")[[1]]
		#imageRegister <- hashMapLabel[3]
		#panelID <- gsub("[[:punct:]]", "", paste0(hashMapLabel,collapse = ""))
		#panelID <- gsub("[[:space:]]", "", panelID)

		print("saving retrace test")
		coordTmp <- lapply(sessionQuery$traceData,function(x){decodePath(unlist(x[[imageNameTableQuery()]]))})
		plotsPanel[["TableQuery"]]$coord <- do.call(rbind,coordTmp[!apply(1>=sapply(coordTmp,dim),2,prod)])
		plotsPanel[["TableQuery"]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageNameTableQuery()]]})
		
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
	#activeRankTableCell <- reactiveValues(cell=matrix(0,1,2),
	activeRankTableCell <- reactiveValues(cell=matrix(1,1,2),
											delayCell=NULL)
	# dont forget, we need to split the id from the image name
	#imageNameTableQuery <- tryCatch(reactive(strsplit(rownames(rankTable$Name)[activeRankTableCell$cell][1]," : ")[[1]][1]), error = function(e) browser(), finally=print("finished"))
	#imageNameTableQuery <- try({reactive(strsplit(rownames(rankTable$Name)[activeRankTableCell$cell][1]," : ")[[1]][1])})
	#imageNameTableQuery <- reactive({print(input$targetFeatures);strsplit(rownames(rankTableParts[[paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)),collapse="")]]$Name)[activeRankTableCell$cell][1]," : ")[[1]][1]})
	imageNameTableQuery <- reactive({strsplit(rownames(rankTableParts[[paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],list(input$targetFeatures))[[1]]),collapse="")]]$Name)[activeRankTableCell$cell][1]," : ")[[1]][1]})
	
	observeEvent(input[[paste0("changeID","TableQuery")]],{
		plotsPanel[["TableQuery"]]$mode <- "setID"
	})
	
	observeEvent(input[[paste0("saveID","TableQuery")]],{
		assignID(panelID="TableQuery",
						imageName=imageNameTableQuery(),
						#rankTable=rankTable,
						#activeCell=activeRankTableCell,
						targetEnvir=sessionQuery)
		
		#neded to update FIX LATER
		output$imageIDTableQuery <- renderText(sessionQuery$idData[imageNameTableQuery()])
		
		plotsPanel[["TableQuery"]]$mode <- "default"
	})
	
	output[[paste0("header","TableQuery")]] <- renderUI({
		if(!is.na(imageNameTableQuery()) && length(imageNameTableQuery())>0 )
		{
			generateDisplayHeader("TableQuery",
									plotsPanel=plotsPanel,
									mode = plotsPanel[["TableQuery"]]$mode,
									closeOption = F,
									fixOption = T)
		}
	})
	
	
	output$imageNameTableQuery <- renderText(imageNameTableQuery())
	output$imageIDTableQuery <- renderText(sessionQuery$idData[imageNameTableQuery()])
	
	output$imageTableQuery <- renderPlot({
			print("656")
		print(paste('plot:',plotsPanel[["TableQuery"]]$fin))
		plotFinTrace(load.image(plotsPanel[["TableQuery"]]$fin),
								plotsPanel[["TableQuery"]]$coord,
								plotsPanel[["TableQuery"]]$focusedCoord,
								plotsPanel[["TableQuery"]]$mode,
								#input$segmentTarget,
								input,
								input$traceTableQuery)
	})
	
	# --- rankTable rendering
	tableOptions = list(lengthChange = T, 
						rownames=T, 
						ordering=F, 
						paging = T,
						scrollY = "500px",
						scrollX = "750px",
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
								print("762")
							plotsPanel[["TableQuery"]]$fin <- file.path(input$queryDirectory, imageNameTableQuery())
							if(plotsPanel[["TableQuery"]]$mode != "default")
							{
								#make sure we have a clean slate
								cancelRetrace(readyToRetrace=readyToRetrace,
															targetEnvir=sessionQuery)
							}
							#plotsPanel[["TableQuery"]]$coord <- sessionQuery$traceData[imageNameTableQuery()]
							#plotsPanel[["TableQuery"]]$coord <- lapply(sessionQuery$traceData,function(x){x[imageNameTableQuery()]})
							#names(plotsPanel[["TableQuery"]]$coord) <- names(sessionQuery$traceData)
							#plotsPanel[["TableQuery"]]$coord <- do.call(rbind,lapply(sessionQuery$traceData,function(x){if(is.null(unlist(x[[imageNameTableQuery()]]))){return(c(0,0))}else{decodePath(unlist(x[[imageNameTableQuery()]]))} }))
							plotsPanel[["TableQuery"]]$coord <- do.call(rbind,lapply(sessionQuery$traceData,function(x){decodePath(unlist(x[[imageNameTableQuery()]])) }))
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
			#plotsPanel[[panelID]]$coord <- do.call(rbind,lapply(targetEnvir$traceData,function(x){decodePath(unlist(x[[imageName]]))}))
										print(paste('plot:',rankTableUniqueOnly$Name[activeRankTableCell$cell]))
		#coordTmp <- lapply(sessionQuery$traceData,function(x){decodePath(unlist(x[[imageNameTableQuery()]]))})
		#plotsPanel[["TableQuery"]]$coord <- do.call(rbind,coordTmp[!apply(1>=sapply(coordTmp,dim),2,prod)])
		#plotsPanel[["TableQuery"]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageNameTableQuery()]]})
										#coordinates,
										#focusedCoordinates
										#plotsPanel[["TableQuery"]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageNameTableQuery()]]})
										#sessionQuery$hashData[[finPart]] <- sessionQuery$hashData[[finPart]][names(sessionQuery$idData)]
										plotFinTrace(load.image(rankTableUniqueOnly$Name[activeRankTableCell$cell]),
																do.call(rbind,lapply(sessionReference$traceData,function(x){decodePath(unlist(x[[rankTableUniqueOnly$Name[activeRankTableCell$cell] ]]))})),
																#decodePath(unlist(sessionReference$traceData[[ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)]][rankTableUniqueOnly$Name[activeRankTableCell$cell]])),
																lapply(sessionReference$traceData,function(x){x[[ rankTableUniqueOnly$Name[activeRankTableCell$cell] ]]}),
																mode="default",
																#input$segmentTarget,
																input,
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
																#sessionReference$traceData[rankTable$Name[activeRankTableCell$cell]],
																do.call(rbind,lapply(sessionReference$traceData,function(x){decodePath(unlist(x[[rankTable$Name[activeRankTableCell$cell] ]]))})),
																#sessionReference$traceData[rankTable$Name[activeRankTableCell$cell]][[input$segmentTarget]],
																#decodePath(unlist(sessionReference$traceData[[ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)]][rankTable$Name[activeRankTableCell$cell]])),
																lapply(sessionReference$traceData,function(x){x[[ rankTable$Name[activeRankTableCell$cell] ]]}),
																mode="default",
																#input$segmentTarget,
																input,
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
					if(!is.null(sessionReference$hashData[["Trailing"]]) && 
							!is.null(sessionQuery$hashData[["Trailing"]]))
					{
						print("calculating rank table")
						calculateRankTable(rankTable=rankTable,
										   rankTableParts=rankTableParts,
											sessionQuery=sessionQuery,
											sessionReference=sessionReference,
											input)
						print("862")
						rankTable$editCount <- rankTable$editCount+1
					}
				})
	
	
	observeEvent(c(rankTable$editCount, input$targetFeatures),{
						#print(paste(paste0("things go obs: ",input$targetFeatures),paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)),collapse="")))
						#paste(paste0("things go obs: ",input$targetFeatures,collapse=" "))
						updateDisplayTables(finPartSelected=paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],list(input$targetFeatures))[[1]]),collapse=""), rankTableUniqueOnlyParts, rankTableUniqueOnly)
						updateDisplayTables(finPartSelected=paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],list(input$targetFeatures))[[1]]),collapse=""), rankTableParts, rankTable)
				 }
	)
	
	
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
						if((length(sessionReference$hashData[["Trailing"]])+length(sessionQuery$hashData[["Trailing"]]))>1)
						{
							sessionStorage$permutation <- NULL
							
							allHashData <- append(sessionQuery$hashData[["Trailing"]],sessionReference$hashData[["Trailing"]])
							
							hashRow$names <- append(if(length(sessionQuery$hashData[["Trailing"]])>0){paste("Query:",sessionQuery$idData[names(sessionQuery$hashData[["Trailing"]])],":",names(sessionQuery$hashData[["Trailing"]]))},
													if(length(sessionReference$hashData[["Trailing"]])>0){paste("Refer:",sessionReference$idData[names(sessionReference$hashData[["Trailing"]])],":",names(sessionReference$hashData[["Trailing"]]))})
							
							hashData <- t(data.matrix(data.frame(allHashData)))
							# simplify the display name (space denotes second "* : *" instead of first "*: *")
							rownames(hashData) <- lapply(strsplit(hashRow$names," : "),function(x){paste(x[1],basename(x[2]),sep = " : ")})
							
							dist_mat <- dist(hashData, method = 'euclidean')
							hclust_avg <- hclust(dist_mat, method = 'ward.D')
							sessionStorage$permutation <- hclust_avg$order
							
							
							testHashTable <- round(hashData[sessionStorage$permutation,],-1)/10
							
							tblBreaks <- quantile(testHashTable, probs = seq(.05, .95, .05), na.rm = TRUE)
							tblColors <- round(seq(255, 40, length.out = length(tblBreaks) + 1), 0) %>% {paste0("rgb(",.,",",.,",255)")}
							
							colnames(testHashTable) <- c(letters,1:6)
							
							output$hashComparison <-  DT::renderDataTable(
								datatable(testHashTable ,
										selection = list(mode="multiple",target = "row"),
										options = list(lengthChange = T, 
														rownames=T, 
														ordering=F, 
														autoWidth = T,
														paging = T,
														scrollX=T,
														scrollY = "500px",
														pageLength = 1000, lengthMenu = list('500', '1000','2000', '10000'),
														columnDefs = list(list(targets = c(1:32), searchable = FALSE, width = "1")))
								) %>% formatStyle(colnames(testHashTable), backgroundColor = styleInterval(tblBreaks, tblColors)),
								searchDelay = 1000)
						}
					})
	
	
	# --- instance selection
	observeEvent(input$hashComparison_cell_clicked, {
		
		if(!is.null(sessionStorage$permutation))
		{
			newRender <- hashRow$names[sessionStorage$permutation[input$hashComparison_rows_selected]]
			testIfNewRender <- which(!(newRender %in% displayActive$lockedSelections))
			if(length(testIfNewRender)>0)
			{
				displayActive$activeSelections <- head(append(displayActive$lockedSelections,
														newRender[testIfNewRender]),plotLim)
			}
		}
	})
	
	
	
	# ---------- Cluster Display windows --------------------------------------------------
	
	windowObserver <- function(imageName,plotsPanel,targetDir,panelID,selection,targetEnvir)
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

		# --- clear segment
		observeEvent(input[[paste0("removeSegment",panelID)]],ignoreInit=T,{
				showModal(modalDialog(
					title = paste("Are you sure you want to remove?",input$segmentTarget,"segment"),
					size = "s",
					footer=tagList(actionButton("confirmSegmentRemoveW","Yes"),modalButton("Cancel")),
					easyClose = F
				))
		})

		observeEvent(input$confirmSegmentRemoveW,{
		removeModal()
		#sessionQuery$traceData[[input$segmentTarget]][[imageNameTableQuery()]] <- c(0,0,4,0)
		#sessionQuery$hashData[[input$segmentTarget]][[imageNameTableQuery()]] <- NULL
		#plotsPanel[["TableQuery"]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageNameTableQuery()]]})
		sessionQuery$traceData[[paste0(sort(input$segmentTarget),collapse="")]][[imageName]] <- c(0,0,4,0)
		sessionQuery$hashData[[paste0(sort(input$segmentTarget),collapse="")]][[imageName]] <- NULL
		plotsPanel[[panelID]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageName]]})

		if(!is.null(rankTable$ID) && !is.null(rankTableUniqueOnly$ID))
		{
			rownamesTmp <-  sessionQuery$idData[which(names(sessionQuery$idData)==imageName)]
			rownames <- paste(names(rownamesTmp),":",rownamesTmp)
			#sessionQuery$idData <- sessionQuery$idData[which(names(sessionQuery$idData)!=imageNameTableQuery())]
			#sessionQuery$hashData[[input$segmentTarget]][imageNameTableQuery()] <- NULL
			#sessionQuery$traceData[[input$segmentTarget]][imageNameTableQuery()] <- NULL
			rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name       <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      ) != rownames,]
			rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple[rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple) != rownames,]
			rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID         <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        ) != rownames,]
			rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Unique     <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Unique    [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Unique    ) != rownames,]
			rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance   <- rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  [rownames(rankTableParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  ) != rownames,]

			rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name       <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      [rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Name      ) != rownames,]
			rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple[rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$NameSimple) != rownames,]
			rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID         <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        [rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$ID        ) != rownames,]
			rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance   <- rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  [rownames(rankTableUniqueOnlyParts[[paste0(sort(input$segmentTarget),collapse="")]]$Distance  ) != rownames,]

			activeRankTableCell$cell <- matrix(1,1,2)
			#updateDisplayTables(finPartSelected=ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures), rankTableParts, rankTable)
			rankTable$editCount <- rankTable$editCount+1
		}
		})

		# --- set window to retrace
		observeEvent(input[[paste0("retrace",panelID)]],ignoreInit=T,{
							# activeRankTableCell$cell
			plotsPanel[[panelID]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageName]]})
			prepRetrace(panelID=panelID,
						imageName=imageName,
						rowName = rownames(rankTable$Name)[activeRankTableCell$cell][1],
						targetDir = input$queryDirectory,
						readyToRetrace=readyToRetrace)
		})
		
		#TODO: change how these restore events are handled for the multi trace features
		# --- restore defaults upon cancel
		observeEvent(input[[paste0("cancelRetrace",panelID)]],ignoreInit=T,{
			cancelRetrace(readyToRetrace=readyToRetrace,
										targetEnvir=sessionQuery)
			#plotsPanel[[panelID]]$coord <- targetEnvir$traceData[imageName]
			#plotsPanel[[panelID]]$coord <- lapply(targetEnvir$traceData,function(x){x[imageName]})
			#names(plotsPanel[[panelID]]$coord) <- names(targetEnvir$traceData)
			plotsPanel[[panelID]]$coord <- do.call(rbind,lapply(targetEnvir$traceData,function(x){decodePath(unlist(x[[imageName]]))}))
		})
		
		# --- save trace edit
		observeEvent(input[[paste0("saveRetrace",panelID)]],ignoreInit=T,{
			print(panelID)
			print(length(readyToRetrace))
			saveRetrace(readyToRetrac=readyToRetrace,
									targetEnvir=sessionQuery,
									mxnetModel=mxnetModel)
			print("save complete")
			# reset outputs
			#plotsPanel[[panelID]]$coord <- targetEnvir$traceData[imageName]
			#plotsPanel[[panelID]]$coord <- lapply(targetEnvir$traceData,function(x){x[imageName]})
			#names(plotsPanel[[panelID]]$coord) <- names(targetEnvir$traceData)
			##plotsPanel[[panelID]]$coord <- do.call(rbind,lapply(targetEnvir$traceData,function(x){decodePath(unlist(x[[imageName]]))}))
			coordTmp <- lapply(sessionQuery$traceData,function(x){decodePath(unlist(x[[imageName]]))})
			plotsPanel[[panelID]]$coord <- do.call(rbind,coordTmp[!apply(1>=sapply(coordTmp,dim),2,prod)])
			plotsPanel[[panelID]]$focusedCoord <- lapply(sessionQuery$traceData,function(x){x[[imageName]]})
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
	
	
	windowGenerator <- function(selection,
								plotsPanel)
	{
		hashMapLabel <- strsplit(selection,": ")[[1]]
		imageRegister <- hashMapLabel[3]
		panelID <- gsub("[[:punct:]]", "", paste0(hashMapLabel,collapse = ""))
		panelID <- gsub("[[:space:]]", "", panelID)
		
		{
			plotsPanel[[panelID]] <- reactiveValues(fin=NULL,
													coord=NULL,
													targetCoord=NULL,
													locked=FALSE,
													mode="default")
		}
		
		sourceType <- hashMapLabel[1]
		
		output[[paste0("imageName",panelID)]] <- renderText(imageRegister)
		
		if(substr(sourceType,nchar(sourceType)-4,nchar(sourceType)) == "Query")
		{
			targetEnvir <- sessionQuery
			allowEdit <- T
			targetDir <- normalizePath(file.path(input$queryDirectory,imageRegister),"/")
		}else{
			targetEnvir <- sessionReference
			allowEdit <- F
			targetDir <- imageRegister
		}
		
		#plotsPanel[[panelID]]$coord <- targetEnvir$traceData[["Trailing"]][imageRegister]
		#TODO we need to change this if we have selected a particular trace portion?
		#plotsPanel[[panelID]]$coord <- do.call(rbind,lapply(targetEnvir$traceData,function(x){decodePath(unlist(x[[imageRegister]]))}))
		coordTmp <- lapply(targetEnvir$traceData,function(x){decodePath(unlist(x[[imageRegister]]))})
		plotsPanel[[panelID]]$coord <- do.call(rbind,coordTmp[!apply(1>=sapply(coordTmp,dim),2,prod)])

					#plotCoordinates <- do.call(rbind,lapply(coordinates,function(x){decodePath(unlist(x))}))
		#plotsPanel[[panelID]]$focusedCoord<- do.call(rbind,targetEnvir$traceData[[input$segmentTarget]][[imageRegister]])
		#plotsPanel[[panelID]]$focusedCoord <- decodePath(targetEnvir$traceData[["Trailing"]][[imageRegister]])
		plotsPanel[[panelID]]$focusedCoord <- lapply(targetEnvir$traceData,function(x){x[[imageRegister]]})

		#plotsPanel[[panelID]]$coord <- lapply(targetEnvir$traceData,function(x){x[[imageRegister]]})
		#names(plotsPanel[[panelID]]$coord) <- names(targetEnvir$traceData)
		
		output[[paste0("header",panelID)]] <- renderUI({
			generateDisplayHeader(panelID,
									plotsPanel=plotsPanel,
									mode= plotsPanel[[panelID]]$mode,
									closeOption = T,
									fixOption=allowEdit)
		})
		
		output[[paste0("imageID",panelID)]] <- renderText(targetEnvir$idData[imageRegister])
		
		# --- image displays
		output[[paste0("image",panelID)]] <- renderPlot({
			print(paste('plot:',targetDir))
			print(paste('panelID:',panelID))
			plotFinTrace(load.image(targetDir),
									plotsPanel[[panelID]]$coord,
									plotsPanel[[panelID]]$focusedCoord,
									plotsPanel[[panelID]]$mode,
									#input$segmentTarget,
									input,
									input[[paste0("trace",panelID)]])#includeTrace
		})
		
		windowObserver(imageRegister,plotsPanel,targetDir,panelID,selection,targetEnvir)
		
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
					lapply(displayActive$activeSelections[c(FALSE,TRUE)], function(display) {#,sessionQuery,sessionReference
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
