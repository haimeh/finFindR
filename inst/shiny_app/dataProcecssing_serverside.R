


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
	  load(file.path(system.file("extdata", package="finFindR"),"hashSVD.Rdata"))
	  print(hashSVD$U)
	  
		remove <- NULL
		
		#hashData <- list()
		traceImg <- list()
		#traceCoord <- list()
		#idData <- NULL
		hashTemplate <- rep(NA,length(imgPaths))
		traceTemplate <- rep(NA,length(imgPaths))
		#traceTemplate <- array(0,c(2,length(imgPaths)))
		names(traceTemplate) <- basename(imgPaths)
		names(hashTemplate) <- basename(imgPaths)
		
		hashData <- list(Trailing=hashTemplate, Leading=hashTemplate, Peduncle=hashTemplate)
		traceCoord <- list(Trailing=traceTemplate, Leading=traceTemplate, Peduncle=traceTemplate)
		#traceImg <- list(Trailing=storageTemplate, Leading=storageTemplate, Peduncle=storageTemplate)
		idData <- NULL
		
			finParts <- list("Trailing","Leading","Peduncle")
			#finParts <- list("Peduncle","Trailing","Leading")

			#cl <- parallel::makeCluster(getOption("cl.cores", length(finParts)))
			#parallel::clusterExport(cl,"pathNet")
			#parallel::clusterEvalQ(cl, 
			#	library(finFindR)
			#)
		progressTicker <- 0
		for(img in imgPaths)
		{
			#harakami garden avatour amore

			print(paste("loading",basename(img)))
			progressTicker <- progressTicker+1
			incProgress(1/length(imgPaths), detail = paste(basename(img)," : ",progressTicker,"of",length(imgPaths)))


			#parallel::clusterExport(cl, "img")
			#traceResults <- parallel::parLapply(cl, finParts,function(finPart) {
			#	traceResults <- try(finFindR::traceFromImage(imager::load.image(img),NULL,pathNet, (finPart=="Trailing")))
			#	if(class(traceResults)!="try-error" && 
			#		length(unlist(traceResults)[[1]])>0 &&
			#		!is.null(unlist(traceResults)[[1]]))
			#	{
			#		#return(list(annulus=imager::load.image(img),coordinates=pathNet,dim=img))
			#		return(traceResults)
			#	}
			#	return(list(annulus=NULL,coordinates=NULL,dim=NULL))
			#})

			# channel 1 : all
			# channel 2 : peduncle
			# channel 3 : trans
			# channel 4 : trailing
			# channel 5 : tip
			# channel 6 : leading
			selectedChan <- c("Peduncle"=2,"Trailing"=4,"Leading"=6)
			result <-
			traceResults <- list()
			justStartCoord <- NULL
			netOut <- NULL
			for(finPart in finParts){
				print(finPart);
				print("nn");
				reuseData <- switch(finPart,
					"Trailing"=list(justStartCoord=NULL,userNetOut=NULL),
					"Peduncle"=list(justStartCoord=traceResults[["Trailing"]]$coordinates[nrow(traceResults[["Trailing"]]$coordinates),], userNetOut=reuseData$userNetOut),
					"Leading"=list(justStartCoord=traceResults[["Trailing"]]$coordinates[1,], userNetOut=reuseData$userNetOut)
				)
				result <- try(invisible(traceFromImage(fin=imager::load.image(img),startStopCoords = NULL,pathNet = pathNet, edgeChan=selectedChan[[finPart]], justStartCoord=reuseData$justStartCoord, userNetOut=reuseData$userNetOut)))
				if(class(result)=="try-error"){
					result <- list(annulus=NULL,coordinates=NULL,dim=NULL, netOut=NULL)
				}
				traceResults[[finPart]] <- result
			}
			#traceResults <- lapply(finParts,function(finPart){
			#	print(finPart);
			##try(traceFromImage(imager::load.image(img),NULL,pathNet, (finPart=="Trailing")))
			#				#justStartCoord = NULL,
			#				#userNetOut = NULL)
			#	if(finPart=="Trailing"){
			#		justStartCoord
			#	}
			#	result <- try(traceFromImage(imager::load.image(img),NULL,pathNet, selectedChan[[finPart]]))
			#	if(class(result)=="try-error"){
			#		return(list(annulus=NULL,coordinates=NULL,dim=NULL, netOut=NULL))
			#	}
			#	return(result)
			#})
			names(traceResults) <- finParts

			## name lists of data
			for(finPart in finParts){
				print(finPart);
				print("hash");
				#BUG: we need to address the parts of code that expect the null results to be removed..
				traceImg[[finPart]][basename(img)] <- list(traceResults[[finPart]]$annulus)
				#if(nrow(traceResults[[finPart]]$coordinates)>2 & class(traceResults[[finPart]]$coordinates) != "try-error" & !is.null(traceResults[[finPart]]$coordinates)){
				#if(class(traceResults[[finPart]]$coordinates[1]) != "try-error" & !is.null(traceResults[[finPart]]$coordinates)){
				if(!is.null(traceResults[[finPart]]$coordinates)){
					traceCoord[[finPart]][basename(img)] <- list(encodePath(traceResults[[finPart]]$coordinates))
				}else{
					traceCoord[[finPart]][basename(img)] <- list(c(0,0,4,0))
				}
			}
			idData[basename(img)] <- "unlabeled"
		}
			#parallel::stopCluster(cl)

		for(finPart in finParts){
			failureIndex <- !sapply(traceImg[[finPart]],is.null)
			#TODO: remove nulls before traceToHash and then add them back?

			if(finPart == "Trailing"){
				hashData[[finPart]] <- as.data.frame(traceToHash(traceImg[[finPart]][failureIndex],mxnetModel),check.names=F)
			}else{
				hashDataPart <- as.data.frame(traceToHash(traceImg[[finPart]][failureIndex],mxnetModel),check.names=F)
				hashData[[finPart]] <- as.data.frame(t(t(hashDataPart) %*% hashSVD$U %*% hashSVD$D),check.names=F,check.names=F)*1000
			}

			#hashData[[finPart]] <- as.data.frame(traceToHash(traceImg[[finPart]][failureIndex],mxnetModel))
			#if(any(processIndex)){
			#		hashData[[finPart]][which(!failureIndex)] <- list(NULL)
			#}
			if(!appendNew)
			{
				saveEnvir[["hashData"]][[finPart]]  <- NULL
				saveEnvir[["traceData"]][[finPart]] <- NULL
				saveEnvir[["idData"]]    <- NULL
			}
			saveEnvir[["hashData"]][[finPart]]  <- append(hashData[[finPart]], saveEnvir$hashData[[finPart]])
			saveEnvir[["traceData"]][[finPart]] <- append(traceCoord[[finPart]], saveEnvir$traceData[[finPart]])
			saveEnvir[["idData"]] <- append(idData,saveEnvir$idData)
		}
	}
}


#annulus_coordinates = parallel::mclapply(list(...), function(imageName){
#    returnObj <- list()
#    traceResults <- traceFromImage(fin=load.image(imageName),
#                   startStopCoords = NULL,
#                   pathNet = pathNet)
#    returnObj[paste0(imageName,"_ann")] <- list(traceResults$annulus)
#    returnObj[paste0(imageName,"_coo")] <- list(traceResults$coordinates)
#    return(returnObj)
#  }, mc.cores=cores)
#annulusImgs <- sapply(annulus_coordinates,function(x)
#                      {x_tmp <- x[1]; names(x_tmp) <- substr(names(x_tmp), 0,nchar(names(x_tmp))-4); return(x_tmp)} )
#edgeCoords <- sapply(annulus_coordinates,function(x)
#                     {x_tmp <- x[2]; names(x_tmp) <- substr(names(x_tmp), 0,nchar(names(x_tmp))-4); return(x_tmp)} )
#



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


updateDisplayTables <- function(finPartSelected, rankTableParts, rankTable){
	rankTable$NameSimple <- rankTableParts[[finPartSelected]]$NameSimple
	rankTable$Name       <- rankTableParts[[finPartSelected]]$Name
	rankTable$ID         <- rankTableParts[[finPartSelected]]$ID
	rankTable$Unique     <- rankTableParts[[finPartSelected]]$Unique
	rankTable$Distance   <- rankTableParts[[finPartSelected]]$Distance
}

calculateRankTable <- function(rankTable,
							   rankTableParts,
								sessionQuery,
								sessionReference,
								input)
{
	counterEnvir <- new.env()
	counterEnvir$progressTicker <- 0
	counterEnvir$reactiveDomain <- getDefaultReactiveDomain()
	counterEnvir$length <- length(sessionQuery$hashData)

	finPartCombos <- unique(lapply(apply(expand.grid(rep(list(c("Trailing","Leading","Peduncle")),3)),1,unique),sort))

	
	
	withProgress(
		message = 'Matching', value = 0, session = counterEnvir$reactiveDomain,
		{
			#TODO: change hardcode from trailing to whatever combo the user specify
			#TODO: we need to handle null content without messing up id alignment 
		comparisonResults <- list()

		queryTmp <- list()
		referTmp <- list()
		queryTmp[["Trailing"]] <- sessionQuery$hashData[["Trailing"]]
		referTmp[["Trailing"]] <- sessionReference$hashData[["Trailing"]]
		prevFinPart <- "Trailing"
		#for(finPart in c("Leading","Peduncle")){
		#	queryNamesPrev <- names(queryTmp[[prevFinPart]])
		#	queryNames <- names(sessionQuery$hashData[[finPart]])
		#	querySharedNames <- merge(x=cbind(queryNamesPrev,1), y=cbind(queryNames,1), by.x="queryNamesPrev", by.y="queryNames",  all.y=F, all.x=F)[,1]
		#	queryTmp[[finPart]] <- lapply(querySharedNames,function(i){append(queryTmp[[prevFinPart]][[i]],
		#														sessionQuery$hashData[[finPart]][[i]][1:8])})
		#	names(queryTmp[[finPart]]) <- querySharedNames

		#	referNamesPrev <- names(referTmp[[prevFinPart]])
		#	referNames <- names(sessionReference$hashData[[finPart]])
		#	referSharedNames <- merge(x=cbind(referNamesPrev,1), y=cbind(referNames,1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]
		#	referTmp[[finPart]] <- lapply(referSharedNames,function(i){append(referTmp[[prevFinPart]][[i]],
		#														sessionReference$hashData[[finPart]][[i]][1:8])})
		#	names(referTmp[[finPart]]) <- referSharedNames
		##	prevFinPart <- finPart
		#}
		for(finPart in c("Leading","Peduncle")){
			queryNamesPrev <- names(queryTmp[["Trailing"]])
			queryNames <- names(sessionQuery$hashData[[finPart]])
			querySharedNames <- merge(x=cbind(queryNamesPrev,1), y=cbind(queryNames,1), by.x="queryNamesPrev", by.y="queryNames",  all.y=F, all.x=F)[,1]
			queryTmp[[finPart]] <- lapply(querySharedNames,function(i){append(queryTmp[["Trailing"]][[i]],
																sessionQuery$hashData[[finPart]][[i]][1:8])})
			names(queryTmp[[finPart]]) <- querySharedNames

			referNamesPrev <- names(referTmp[["Trailing"]])
			referNames <- names(sessionReference$hashData[[finPart]])
			referSharedNames <- merge(x=cbind(referNamesPrev,1), y=cbind(referNames,1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]
			referTmp[[finPart]] <- lapply(referSharedNames,function(i){append(referTmp[["Trailing"]][[i]],
																sessionReference$hashData[[finPart]][[i]][1:8])})
			names(referTmp[[finPart]]) <- referSharedNames
		#	prevFinPart <- finPart
		}

		print("compare")
		for(finPart in c("Trailing","Leading","Peduncle")){
		  
			#comparisonResults[[finPart]] <- distanceToRefParallel(queryHashData=sessionQuery$hashData[[finPart]],
			#											referenceHashData=sessionReference$hashData[[finPart]],
			comparisonResults[[finPart]] <- distanceToRefParallel(queryHashData= queryTmp[[finPart]],
														referenceHashData=referTmp[[finPart]],
														counterEnvir=counterEnvir,
														batchSize = 500,
														displayProgressInShiny=T)
			comparisonResults[[finPart]]$sortingIndex <- t(comparisonResults[[finPart]]$sortingIndex)
		}
		#TODO: make sure we keeep the image names aligned when we move on to the sorting stage
		incProgress(0,
					detail = paste("Matching Complete"),
					session = counterEnvir$reactiveDomain)
		}
	)
	
	{
	#rankTable <- reactiveValues(Name=NULL,
	#							NameSimple=NULL,
	#							ID=NULL,
	#							Unique=NULL,
	#							Distance=NULL,
	#							editCount=0)
	#rankTableParts <- list(
	#	Trailing = reactiveValues(Name=NULL,
	#	Leading = reactiveValues(Name=NULL,
	#	Peduncle = reactiveValues(Name=NULL,
	#)
	#
	#rankTableUniqueOnly <- reactiveValues(NameSimple=NULL,
	#										Name = NULL,
	#										ID=NULL,
	#										Distance=NULL)
	#rankTableUniqueOnlyParts <- list(

		#TODO: related to the line 280 todo, we are using the session Query to assign names, but this wont work naive 
		#TODO: row names are not assigned correctly
		withProgress(
			message = 'Sorting', value = 0,{
				rownames <- list(Trailing=NULL,Leading=NULL,Peduncle=NULL)
				for(finPart in c("Trailing","Leading","Peduncle")){
					#NOTE: none of the names should be missing, just null, so alignment should not be issue
					#TODO: tho the comparison table shouldnt have all images, so we need to subset by hashData null?
					#tmpRowNames <- names(sessionQuery$hashData[[finPart]])[!is.null(sessionQuery$hashData[[finPart]])]

					#tmpRowNames <- names(sessionQuery$idData[names(sessionQuery$hashData[[finPart]])])
					tmpRowNames <- names(sessionQuery$idData[names(queryTmp[[finPart]])])
					rownames[[finPart]] <- paste(tmpRowNames,":",sessionQuery$idData[tmpRowNames])
				}
				
				simpleNamesVec <- list()
				incProgress(0,detail=paste("file locations"))
				#rankTable$Name <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)names(sessionReference[[finPart]]$idData)[x])
				#simpleNamesVec <- basename(names(sessionReference$idData))
				for(finPart in c("Trailing","Leading","Peduncle")){
					#TODO: comparisonResults wont have every image name represented and so idData must be subset unlike abovee
					#rankTableParts[[finPart]]$Name <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)names(sessionReference$idData[names(sessionReference$hashData[[finPart]])])[x])
					rankTableParts[[finPart]]$Name <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)names(sessionReference$idData[names(referTmp[[finPart]])])[x])
					#simpleNamesVec[[finPart]] <- basename(names(sessionReference$idData[names(sessionReference$hashData[[finPart]])]))
					simpleNamesVec[[finPart]] <- basename(names(sessionReference$idData[names(referTmp[[finPart]])]))
				}
				incProgress(1/8)

				for(finPart in c("Trailing","Leading","Peduncle")){
					rankTableParts[[finPart]]$NameSimple <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)simpleNamesVec[[finPart]][x])
					# single queries need to be turned back from vectors
				 
					#rankTable$Name <- as.data.frame(t(rankTable$Name))
					#rankTable$NameSimple <- as.data.frame(t(rankTable$NameSimple))
					#rownames(rankTable$Name) <- rownames
					#rownames(rankTable$NameSimple) <- rownames
					if(nrow(comparisonResults[[finPart]]$distances)>0)
					{
						rankTableParts[[finPart]]$NameSimple <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)simpleNamesVec[[finPart]][x])
						rankTableParts[[finPart]]$Name <- as.data.frame((rankTableParts[[finPart]]$Name),check.names=F)
						rankTableParts[[finPart]]$NameSimple <- as.data.frame((rankTableParts[[finPart]]$NameSimple),check.names=F)
						#TODO: we may need rownames split by finPart?
						rownames(rankTableParts[[finPart]]$Name) <- rownames[[finPart]] 
						rownames(rankTableParts[[finPart]]$NameSimple) <- rownames[[finPart]] 
					}
				}
				
				incProgress(1/8,detail=paste("IDs"))
				#rankTable$ID <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)sessionReference$idData[x])
				## single queries need to be turned back from vectors
				#if(nrow(comparisonResults[[finPart]]$distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID))}
				#rownames(rankTable$ID) <- rownames

				for(finPart in c("Trailing","Leading","Peduncle")){
					rankTableParts[[finPart]]$ID <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)sessionReference$idData[x])
					# single queries need to be turned back from vectors
					if(nrow(comparisonResults[[finPart]]$distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID),check.names=F)}
					rownames(rankTableParts[[finPart]]$ID) <- rownames[[finPart]] 
				}
				
				incProgress(1/4,detail=paste("extracting top class matches"))
				#rankTable$Unique <- t(!apply(rankTable$ID,1,duplicated))
				#rownames(rankTable$Unique) <- rownames
				for(finPart in c("Trailing","Leading","Peduncle")){
					rankTableParts[[finPart]]$Unique <- t(!apply(rankTableParts[[finPart]]$ID,1,duplicated))
					rownames(rankTableParts[[finPart]]$Unique) <- rownames[[finPart]] 
				}
				
				incProgress(1/4,detail=paste("distance"))
				#rankTable$Distance <- t(apply(comparisonResults$distances,1,function(x)sort(x,decreasing = F)))
				#rankTable$Distance <- (comparisonResults$distances)
				###rownames(rankTable$Distance) <- rownames
				for(finPart in c("Trailing","Leading","Peduncle")){
					rankTableParts[[finPart]]$Distance <- (comparisonResults[[finPart]]$distances)
					rownames(rankTableParts[[finPart]]$Distance) <- rownames[[finPart]] 
				}
				incProgress(1/4,detail=paste("Done"))
			})
		print("compare end")
		updateDisplayTables(finPartSelected=input$targetFeatures, rankTableParts, rankTable)
		gc()
	}
}


# extractMetadata <- function(directory,saveEnvir)
# {
#		print("searching")
#		imgs <- try(list.files(directory,full.names = T,pattern = "\\.JPG$"))
#		if(typeof(imgs) != "try-error" && length(imgs)>0)
#		{
#			metadata <- easyEXIF(list.files(directory,full.names = T,pattern = "\\.JPG$"))
#			metadata <- as.data.frame(do.call(rbind, metadata))
#			colnames(metadata) <- c("ID","Hash","Lat","Lon","Image")
#			assign('metadata',metadata,envir = saveEnvir)
#		}else{
#			showModal(modalDialog(
#				title = paste("No JPG found in:",directory),
#				size = "s",
#				easyClose = TRUE
#			))
#		}
# }
