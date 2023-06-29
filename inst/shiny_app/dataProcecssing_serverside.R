


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

			# channel 1 : all
			# channel 2 : peduncle
			# channel 3 : trailing
			# channel 4 : leading
			# channel 5 : all2
			# channel 6 : tip
			# channel 7 : transi
			selectedChan <- c("Peduncle"=2,"Trailing"=3,"Leading"=4)
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
				result <- try((traceFromImage(fin=imager::load.image(img),startStopCoords = NULL,pathNet = pathNet, edgeChan=selectedChan[[finPart]], justStartCoord=reuseData$justStartCoord, userNetOut=reuseData$userNetOut)))
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
			if(any(failureIndex)){

				if(finPart == "Trailing"){
					hashData[[finPart]] <- as.data.frame(traceToHash(traceImg[[finPart]][failureIndex],mxnetModel),check.names=F)
				}else{
					hashDataPart <- as.data.frame(traceToHash(traceImg[[finPart]][failureIndex],mxnetModel),check.names=F)
					hashData[[finPart]] <- as.data.frame(t(t(hashDataPart) %*% hashSVD$U %*% hashSVD$D),check.names=F,check.names=F)*10000
				}
			}else{
				hashData[[finPart]] <- NULL
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
	counterEnvir$length <- length(sessionQuery$hashData$Trailing)

	finPartCombos <- unique(lapply(apply(expand.grid(rep(list(c("Trailing","Leading","Peduncle")),3)),1,unique),sort))
	
	withProgress(
		message = 'Matching', value = 0, session = counterEnvir$reactiveDomain,
		{
			#TODO: change hardcode from trailing to whatever combo the user specify
			#TODO: we need to handle null content without messing up id alignment 
		comparisonResults <- list()

		queryTmp <- list()
		referTmp <- list()
		#queryTmp[["Trailing"]] <- sessionQuery$hashData[["Trailing"]]
		#referTmp[["Trailing"]] <- sessionReference$hashData[["Trailing"]]
		#prevFinPart <- "Trailing"
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
		#for(finPart in c("Leading","Peduncle")){
		#	queryNamesPrev <- names(queryTmp[["Trailing"]])
		#	queryNames <- names(sessionQuery$hashData[[finPart]])
		#	querySharedNames <- merge(x=cbind(queryNamesPrev,1), y=cbind(queryNames,1), by.x="queryNamesPrev", by.y="queryNames",  all.y=F, all.x=F)[,1]
		#	queryTmp[[finPart]] <- lapply(querySharedNames,function(i){append(queryTmp[["Trailing"]][[i]],
		#														sessionQuery$hashData[[finPart]][[i]][1:8])})
		#	names(queryTmp[[finPart]]) <- querySharedNames

		#	referNamesPrev <- names(referTmp[["Trailing"]])
		#	referNames <- names(sessionReference$hashData[[finPart]])
		#	referSharedNames <- merge(x=cbind(referNamesPrev,1), y=cbind(referNames,1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]
		#	referTmp[[finPart]] <- lapply(referSharedNames,function(i){append(referTmp[["Trailing"]][[i]],
		#														sessionReference$hashData[[finPart]][[i]][1:8])})
		#	names(referTmp[[finPart]]) <- referSharedNames
		##	prevFinPart <- finPart
		#}

		#for(finParts in finPartCombos){
		#	if(finParts>1){
		#		#finParts <- c("Peduncle","Trailing")
		#		queryNames <- table(unlist(lapply(sessionQuery$hashData[finParts],names)))
		#		querySharedNames <- names(queryNames)[queryNames>=length(finParts)]
		#		queryTmp[[paste0(finPart,collapse="")]] <- as.list(do.call(rbind,lapply(finParts,function(x){as.data.frame(sessionQuery$hashData[[x]][querySharedNames])})))
		#		names(queryTmp[[paste0(finPart,collapse="")]]) <- querySharedNames

		#		referNames <- table(unlist(lapply(sessionReference$hashData[finParts],names)))
		#		referSharedNames <- names(referNames)[referNames>=length(finParts)]
		#		referTmp[[paste0(finPart,collapse="")]] <- as.list(do.call(rbind,lapply(finParts,function(x){as.data.frame(sessionReference$hashData[[x]][referSharedNames])})))
		#		names(referTmp[[paste0(finPart,collapse="")]]) <- referSharedNames
		#	}else{
		#		queryTmp[[finParts]] <- sessionQuery$hashData[[finPart]]

		#		referTmp[[finParts]] <- sessionReference$hashData[[finPart]]
		#	}
		#}
		print("compare")
		#for(finPart in c("Trailing","Leading","Peduncle")){
		for(finParts in finPartCombos){
		  
			if(length(finParts)>1){
				#finParts <- c("Peduncle","Trailing")
				queryNames <- table(unlist(lapply(sessionQuery$hashData[finParts],names)))
				querySharedNames <- names(queryNames)[queryNames>=length(finParts)]
				queryTmp[[paste0(sort(finParts),collapse="")]] <- as.list(do.call(rbind,lapply(finParts,function(x){as.data.frame(sessionQuery$hashData[[x]][querySharedNames])})))
				names(queryTmp[[paste0(sort(finParts),collapse="")]]) <- querySharedNames

				referNames <- table(unlist(lapply(sessionReference$hashData[finParts],names)))
				referSharedNames <- names(referNames)[referNames>=length(finParts)]
				referTmp[[paste0(sort(finParts),collapse="")]] <- as.list(do.call(rbind,lapply(finParts,function(x){as.data.frame(sessionReference$hashData[[x]][referSharedNames])})))
				names(referTmp[[paste0(sort(finParts),collapse="")]]) <- referSharedNames
			}else{
				queryTmp[[paste0(sort(finParts),collapse="")]] <- sessionQuery$hashData[[paste0(sort(finParts),collapse="")]]
				referTmp[[paste0(sort(finParts),collapse="")]] <- sessionReference$hashData[[paste0(sort(finParts),collapse="")]]
			}
			#comparisonResults[[finPart]] <- distanceToRefParallel(queryHashData=sessionQuery$hashData[[finPart]],
			#											referenceHashData=sessionReference$hashData[[finPart]],
			comparisonResults[[paste0(sort(finParts),collapse="")]] <- distanceToRefParallel(queryHashData= queryTmp[[paste0(sort(finParts),collapse="")]],
														referenceHashData=referTmp[[paste0(sort(finParts),collapse="")]],
														counterEnvir=counterEnvir,
														batchSize = 500,
														displayProgressInShiny=T)
			comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex <- t(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex)

			queryTmp[[paste0(sort(finParts),collapse="")]] <- lapply(queryTmp[[paste0(sort(finParts),collapse="")]],function(x){0})
			referTmp[[paste0(sort(finParts),collapse="")]] <- lapply(referTmp[[paste0(sort(finParts),collapse="")]],function(x){0})
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


#meh <- lapply(as.list(1:3),function(x){nms <- sample(letters,12); thn <- 1:12; names(thn) <- nms; return(thn)})


		#for(finParts in finPartCombos){
		#	queryNames <- table(unlist(lapply(sessionQuery$hashData[[finParts]],names)))
		#	querySharedNames <- names(queryNames)[queryNames>=length(finParts)]

		#	queryTmp[[paste0(finParts)]] <- lapply(querySharedNames,function(i){append(queryTmp[["Trailing"]][[i]],
		#														sessionQuery$hashData[[finParts]][[i]])})
		#	names(queryTmp[[finPart]]) <- querySharedNames
		#	

		#	referNames <- table(unlist(lapply(sessionReference$hashData[[finParts]],names)))
		#	referSharedNames <- names(referNames)[referNames>=length(finParts)]

		#	referNames <- lapply(sessionReference$hashData[[finParts]])

		#	referSharedNames <- merge(x=cbind(referNamesPrev,1), y=cbind(referNames,1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]

		#	referTmp[[finPart]] <- lapply(referSharedNames,function(i){append(referTmp[["Trailing"]][[i]],
		#														sessionReference$hashData[[finPart]][[i]])})
		#	names(referTmp[[finPart]]) <- referSharedNames
		##	prevFinPart <- finPart
		#}

		#comparisonResult <- distanceToRefParallel(queryHashData=hashData[["Trailing"]],
		#											referenceHashData=hashData[["Trailing"]],
		#											#counterEnvir=new.env(),
		#											batchSize = 500,
		#											displayProgressInShiny=F)
		#returnLimit = length(hashData$Trailing)
		#sortingIndexChunks <- mx.nd.take(mx.nd.argsort(mx.nd.array(data.matrix(comparisonResult$distances)), axis = 0)+1,mx.nd.array(0:(returnLimit-1)),axis=0)


		#TODO: related to the line 280 todo, we are using the session Query to assign names, but this wont work naive 
		#TODO: row names are not assigned correctly
		withProgress(
			message = 'Sorting', value = 0,{
				print("sort")
				rownames <- list(Trailing=NULL,Leading=NULL,Peduncle=NULL)
				#for(finPart in c("Trailing","Leading","Peduncle")){
				for(finParts in finPartCombos){
					#NOTE: none of the names should be missing, just null, so alignment should not be issue
					#TODO: tho the comparison table shouldnt have all images, so we need to subset by hashData null?
					#tmpRowNames <- names(sessionQuery$hashData[[finPart]])[!is.null(sessionQuery$hashData[[finPart]])]

					#tmpRowNames <- names(sessionQuery$idData[names(sessionQuery$hashData[[finPart]])])
					tmpRowNames <- names(sessionQuery$idData[names(queryTmp[[paste0(sort(finParts),collapse="")]])])
					rownames[[paste0(sort(finParts),collapse="")]] <- paste(tmpRowNames,":",sessionQuery$idData[tmpRowNames])
				}
				
				simpleNamesVec <- list()
				incProgress(0,detail=paste("file locations"))
				print("file")
				#rankTable$Name <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)names(sessionReference[[finPart]]$idData)[x])
				#simpleNamesVec <- basename(names(sessionReference$idData))
				#for(finPart in c("Trailing","Leading","Peduncle")){
				for(finParts in finPartCombos){
					#TODO: comparisonResults wont have every image name represented and so idData must be subset unlike abovee
					#rankTableParts[[finPart]]$Name <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)names(sessionReference$idData[names(sessionReference$hashData[[finPart]])])[x])
					rankTableParts[[paste0(sort(finParts),collapse="")]]$Name <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)names(sessionReference$idData[names(referTmp[[paste0(sort(finParts),collapse="")]])])[x])
					#simpleNamesVec[[finPart]] <- basename(names(sessionReference$idData[names(sessionReference$hashData[[finPart]])]))
					simpleNamesVec[[paste0(sort(finParts),collapse="")]] <- basename(names(sessionReference$idData[names(referTmp[[paste0(sort(finParts),collapse="")]])]))
				}
				incProgress(1/8)

				print("name")
				#for(finPart in c("Trailing","Leading","Peduncle")){
				for(finParts in finPartCombos){
					rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)simpleNamesVec[[paste0(sort(finParts),collapse="")]][x])
					# single queries need to be turned back from vectors
				 
					#rankTable$Name <- as.data.frame(t(rankTable$Name))
					#rankTable$NameSimple <- as.data.frame(t(rankTable$NameSimple))
					#rownames(rankTable$Name) <- rownames
					#rownames(rankTable$NameSimple) <- rownames
					if(nrow(comparisonResults[[paste0(sort(finParts),collapse="")]]$distances)>0)
					{
						rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)simpleNamesVec[[paste0(sort(finParts),collapse="")]][x])
						rankTableParts[[paste0(sort(finParts),collapse="")]]$Name <- as.data.frame((rankTableParts[[paste0(sort(finParts),collapse="")]]$Name),check.names=F)
						rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple <- as.data.frame((rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple),check.names=F)
						#TODO: we may need rownames split by finPart?
						rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Name) <- rownames[[paste0(sort(finParts),collapse="")]] 
						rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple) <- rownames[[paste0(sort(finParts),collapse="")]] 
					}
				}
				
				incProgress(1/8,detail=paste("IDs"))
				#rankTable$ID <- apply(comparisonResults[[finPart]]$sortingIndex,1,function(x)sessionReference$idData[x])
				## single queries need to be turned back from vectors
				#if(nrow(comparisonResults[[finPart]]$distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID))}
				#rownames(rankTable$ID) <- rownames

				#for(finPart in c("Trailing","Leading","Peduncle")){
				print("id")
				for(finParts in finPartCombos){
					rankTableParts[[paste0(sort(finParts),collapse="")]]$ID <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)sessionReference$idData[x])
					# single queries need to be turned back from vectors
					if(nrow(comparisonResults[[paste0(sort(finParts),collapse="")]]$distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID),check.names=F)}
					rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$ID) <- rownames[[paste0(sort(finParts),collapse="")]] 
				}
				
				incProgress(1/4,detail=paste("extracting top class matches"))
				#rankTable$Unique <- t(!apply(rankTable$ID,1,duplicated))
				#rownames(rankTable$Unique) <- rownames
				#for(finPart in c("Trailing","Leading","Peduncle")){
				print("top")
				for(finParts in finPartCombos){
					rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique <- t(!apply(rankTableParts[[paste0(sort(finParts),collapse="")]]$ID,1,duplicated))
					rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique) <- rownames[[paste0(sort(finParts),collapse="")]]
				}
				
				incProgress(1/4,detail=paste("distance"))
				print("dist")
				#rankTable$Distance <- t(apply(comparisonResults$distances,1,function(x)sort(x,decreasing = F)))
				#rankTable$Distance <- (comparisonResults$distances)
				###rownames(rankTable$Distance) <- rownames
				#for(finPart in c("Trailing","Leading","Peduncle")){
				for(finParts in finPartCombos){
					rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance <- (comparisonResults[[paste0(sort(finParts),collapse="")]]$distances)
					rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance) <- rownames[[paste0(sort(finParts),collapse="")]] 
				}
				incProgress(1/4,detail=paste("Done"))
			})
		print("compare end")
		updateDisplayTables(finPartSelected=paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],list(input$targetFeatures))[[1]]),collapse=""), rankTableParts, rankTable)
		#updateDisplayTables(finPartSelected=paste0(sort(ifelse(length(input$targetFeatures)==0,finPartCombos[[1]][1],input$targetFeatures)),collapse=""), rankTableUniqueOnlyParts, rankTableUniqueOnly)
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
