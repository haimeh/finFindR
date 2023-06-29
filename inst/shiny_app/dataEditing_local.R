# --- set ID
assignID <- function(panelID,
					imageName,
					targetEnvir)
{
	if(input[[paste0("textID",panelID)]] %in% c(""," "))
	{
		print("ignoring change")
	}else{
		targetEnvir$idData[imageName] <- input[[paste0("textID",panelID)]]
		
		rankTable$editCount <- rankTable$editCount+1
	}
}


# --- set window to retrace
prepRetrace <- function(panelID,
						imageName,
						rowName,
						targetDir,
						readyToRetrace)
{
	plotsPanel[[panelID]]$mode <- "fix"
	
	readyToRetrace$imgName <- imageName
	readyToRetrace$rowName <- rowName
	readyToRetrace$panelID <- panelID
	readyToRetrace$directory <- targetDir
}

# --- restore defaults upon cancel
cancelRetrace <- function(readyToRetrace,
						 targetEnvir)
{
	if(length(readyToRetrace$panelID)>0)
	{
		plotsPanel[[readyToRetrace$panelID]]$mode <- "default"
		
		traceGuideCounter$count <- (-1)
		
		readyToRetrace$imgName <- NULL
		readyToRetrace$rowName <- NULL
		readyToRetrace$panelID <- NULL
		readyToRetrace$directory <- NULL
		readyToRetrace$traceResults <- list()
	}else{
		print("no changes")
		#plotsPanel[[readyToRetrace$panelID]]$mode <- "default"
	}
}

# --- save trace edit and update tables
saveRetrace <- function(readyToRetrace,
						targetEnvir,
						mxnetModel){
			print("saveRetrace57")
	if(length(readyToRetrace$traceResults$coordinates)>0)
	{
	  load(file.path(system.file("extdata", package="finFindR"),"hashSVD.Rdata"))
	  

		#for(finPart in c("trailing","leading")){
			print("prep trace")
			# TODO: do we just retrace the whole thing?
			targetEnvir$traceData[[input$segmentTarget]][readyToRetrace$imgName] <- list(encodePath(readyToRetrace$traceResults$coordinates))
			#targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName] <- traceToHash( list(readyToRetrace$traceResults$annulus ), mxnetModel  )
			if(input$segmentTarget == "Trailing"){
				targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName] <- as.data.frame(traceToHash( list(readyToRetrace$traceResults$annulus ), mxnetModel  ),check.names=F)
				#hashData[[finPart]] <- as.data.frame(traceToHash(traceImg[[finPart]][failureIndex],mxnetModel))
			}else{
				hashDataPart <- as.data.frame(traceToHash( list(readyToRetrace$traceResults$annulus ), mxnetModel  ),check.names=F)
				#hashDataPart <- as.data.frame(traceToHash(traceImg[[finPart]][failureIndex],mxnetModel))
				targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName] <- as.data.frame(t(t(hashDataPart) %*% hashSVD$U %*% hashSVD$D),check.names=F)*10000
				#targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName] <- traceToHash( list(readyToRetrace$traceResults$annulus ), mxnetModel  )
			}
			print("retrace hash calculated")
			
			# --- recalculate rank table ------------------
			if(length(sessionReference$hashData[[input$segmentTarget]])>0)
			{
					


		comparisonResults <- list()
		queryTmp <- list()
		referTmp <- list()
		print("DEBUG REDUNDANT TMP THINGS?")

		for(finParts in finPartCombos[
sapply(finPartCombos,function(x){input$segmentTarget %in% x})
			]){
				print(paste0(sort(finParts),collapse=""))
			if(length(finParts)>1){
				#finParts <- c("Peduncle","Trailing")
				#queryNames <- table(unlist(lapply(targetEnvir$hashData[finParts],names)))
				#querySharedNames <- names(queryNames)[queryNames>=length(finParts)]

				#REF: targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName]
				queryTmp[[paste0(sort(finParts),collapse="")]] <- as.list(do.call(rbind,lapply(finParts,function(x){as.data.frame(targetEnvir$hashData[[x]][readyToRetrace$imgName],check.names=F)})))
				#names(queryTmp[[paste0(sort(finParts),collapse="")]]) <- readyToRetrace$imgName

				referNames <- table(unlist(lapply(sessionReference$hashData[finParts],names)))
				referSharedNames <- names(referNames)[referNames>=length(finParts)]
				referTmp[[paste0(sort(finParts),collapse="")]] <- as.list(do.call(rbind,lapply(finParts,function(x){as.data.frame(sessionReference$hashData[[x]][referSharedNames],check.names=F)})))
				names(referTmp[[paste0(sort(finParts),collapse="")]]) <- referSharedNames
			}else{
				queryTmp[[finParts]] <- targetEnvir$hashData[[paste0(sort(finParts),collapse="")]][readyToRetrace$imgName]
				referTmp[[finParts]] <- sessionReference$hashData[[paste0(sort(finParts),collapse="")]]
			}
			#comparisonResults[[finPart]] <- distanceToRefParallel(queryHashData=targetEnvir$hashData[[finPart]],
			#											referenceHashData=sessionReference$hashData[[finPart]],
			comparisonResults[[paste0(sort(finParts),collapse="")]] <- distanceToRefParallel(
																						queryHashData= queryTmp[[paste0(sort(finParts),collapse="")]],
																						referenceHashData=referTmp[[paste0(sort(finParts),collapse="")]]
																						)
			comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex <- t(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex)

			queryTmp[[paste0(sort(finParts),collapse="")]] <- lapply(queryTmp[[paste0(sort(finParts),collapse="")]],function(x){0})
			referTmp[[paste0(sort(finParts),collapse="")]] <- lapply(referTmp[[paste0(sort(finParts),collapse="")]],function(x){0})
		}



				#rownames <- list(Trailing=NULL,Leading=NULL,Peduncle=NULL)
				simpleNamesVec <- list()
		for(finParts in finPartCombos[
sapply(finPartCombos,function(x){input$segmentTarget %in% x})
			]){
					#NOTE: none of the names should be missing, just null, so alignment should not be issue
					#TODO: tho the comparison table shouldnt have all images, so we need to subset by hashData null?
					#tmpRowNames <- names(sessionQuery$idData[names(queryTmp[[paste0(sort(finParts),collapse="")]])])
					#rownames[[paste0(sort(finParts),collapse="")]] <- paste(tmpRowNames,":",sessionQuery$idData[tmpRowNames])

					#TODO: comparisonResults wont have every image name represented and so idData must be subset unlike abovee
					rankTableParts[[paste0(sort(finParts),collapse="")]]$Name[readyToRetrace$rowName,] <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)names(sessionReference$idData[names(referTmp[[paste0(sort(finParts),collapse="")]])])[x])
					simpleNamesVec[[paste0(sort(finParts),collapse="")]] <- basename(names(sessionReference$idData[names(referTmp[[paste0(sort(finParts),collapse="")]])]))
					rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple[readyToRetrace$rowName,] <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)simpleNamesVec[[paste0(sort(finParts),collapse="")]][x])
					if(nrow(comparisonResults[[paste0(sort(finParts),collapse="")]]$distances)>0)
					{
						rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple[readyToRetrace$rowName,] <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)simpleNamesVec[[paste0(sort(finParts),collapse="")]][x])
						
						#rankTableParts[[paste0(sort(finParts),collapse="")]]$Name[readyToRetrace$rowName,] <- (rankTableParts[[paste0(sort(finParts),collapse="")]]$Name)
						#rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple[readyToRetrace$rowName,] <- (rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple)

						##TODO: we may need rownames split by finPart?
						#rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Name) <- rownames[[paste0(sort(finParts),collapse="")]] 
						#rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$NameSimple) <- rownames[[paste0(sort(finParts),collapse="")]] 
					}
					rankTableParts[[paste0(sort(finParts),collapse="")]]$ID[readyToRetrace$rowName,] <- apply(comparisonResults[[paste0(sort(finParts),collapse="")]]$sortingIndex,1,function(x)sessionReference$idData[x])
					# single queries need to be turned back from vectors
					if(nrow(comparisonResults[[paste0(sort(finParts),collapse="")]]$distances)<=1){rankTable$ID <- as.data.frame(t(rankTable$ID),check.names=F)}
					#rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique[readyToRetrace$rowName,] <- t(!apply(rankTableParts[[paste0(sort(finParts),collapse="")]]$ID,1,duplicated))
					rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance[readyToRetrace$rowName,] <- (comparisonResults[[paste0(sort(finParts),collapse="")]]$distances)
					#rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$ID) <- rownames[[paste0(sort(finParts),collapse="")]] 
					#rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Unique) <- rownames[[paste0(sort(finParts),collapse="")]]
					#rownames(rankTableParts[[paste0(sort(finParts),collapse="")]]$Distance) <- rownames[[paste0(sort(finParts),collapse="")]] 
				}

			}





			#	# NOTE: I think we can calculate these things seprately
			#	if(input$segmentTarget != "Trailing"){
			#		#newDistances <- distanceToRef( unlist(targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName]),
			#		#								cbind(data.frame(sessionReference$hashData[[input$segmentTarget]]),(sessionReference$hashData[["Trailing"]]) ) )
			#		newDistances <- distanceToRef(
			#				queryHashData = unlist(targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName]),
			#				referenceHashData = cbind(data.frame(sessionReference$hashData[[input$segmentTarget]]),(sessionReference$hashData[["Trailing"]]) )
			#				)
			#	}else{
			#		newDistances <- distanceToRef(
			#				queryHashData = unlist(targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName]),
			#				referenceHashData = cbind(data.frame(sessionReference$hashData[[input$segmentTarget]]),(sessionReference$hashData[["Trailing"]]) )
			#				)
			#		browser()	
			#	}
			#	# make sure to clip to correct number of columns
			#	#newSortingIndex <- order(newDistances)[1:ncol(rankTable$Name)]
			#	tmpNames <- sapply(strsplit(rownames(rankTableParts[[input$segmentTarget]]$Name)," : "),function(x)x[[1]])
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
			#	referSharedNames <- merge(x=cbind(referNamesPrev=names(sessionReference$hashData[["Trailing"]]),1), y=cbind(referNames=names(sessionReference$hashData[[input$segmentTarget]]),1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]
			#	newSortingIndex <- order(newDistances)[1:length(referSharedNames)]
			#	#newSortingIndex <- order(newDistances)[1:length(sessionReference$hashData[[input$segmentTarget]])]
			#	
			#	#rankTable$Name[readyToRetrace$rowName,] <- names(sessionReference$idData[newSortingIndex])
			#	#rankTable$NameSimple[readyToRetrace$rowName,] <- basename(names(sessionReference$idData[newSortingIndex]))
			#	#rankTable$ID[readyToRetrace$rowName,] <- sessionReference$idData[newSortingIndex]
			#	#rankTable$Unique[readyToRetrace$rowName,] <- !duplicated(sessionReference$idData[newSortingIndex])
			#	##rankTable$Distance[readyToRetrace$rowName,] <- newDistances[newSortingIndex]
			#	##rankTable$Distance[[input$segmentTarget]][readyToRetrace$rowName,] <- newDistances[newSortingIndex]

			#	rankTableParts[[input$segmentTarget]]$Name[readyToRetrace$rowName,] <- names(sessionReference$idData[newSortingIndex])
			#	rankTableParts[[input$segmentTarget]]$NameSimple[readyToRetrace$rowName,] <- basename(names(sessionReference$idData[newSortingIndex]))
			#	rankTableParts[[input$segmentTarget]]$ID[readyToRetrace$rowName,] <- sessionReference$idData[newSortingIndex]
			#	rankTableParts[[input$segmentTarget]]$Unique[readyToRetrace$rowName,] <- !duplicated(sessionReference$idData[newSortingIndex])
			#	rankTableParts[[input$segmentTarget]]$Distance[readyToRetrace$rowName,] <- newDistances[newSortingIndex]

		#ran#kTableUniqueOnlyParts[[input$segmentTarget]]$Name       <- rankTableUniqueOnlyParts[[input$segmentTarget]]$Name      [rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$Name      ) != rownames,]
		#ran#kTableUniqueOnlyParts[[input$segmentTarget]]$NameSimple <- rankTableUniqueOnlyParts[[input$segmentTarget]]$NameSimple[rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$NameSimple) != rownames,]
		#ran#kTableUniqueOnlyParts[[input$segmentTarget]]$ID         <- rankTableUniqueOnlyParts[[input$segmentTarget]]$ID        [rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$ID        ) != rownames,]
		#ran#kTableUniqueOnlyParts[[input$segmentTarget]]$Distance   <- rankTableUniqueOnlyParts[[input$segmentTarget]]$Distance  [rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$Distance  ) != rownames,]
			#	#for(finPart in c("Trailing","Leading","Peduncle")){
			#	#	DistanceParts[[finPart]]=NULL
			#	#	DistanceParts[[input$segmentTarget]][readyToRetrace$rowName,] <- newDistances[newSortingIndex]
			#	#}
			#	rankTable$editCount <- rankTable$editCount+1
			#	#updateDisplayTables(finPartSelected=input$targetFeatures, rankTableParts, rankTable)
			##}
			## ---------------------------------------------
			
			plotsPanel[[readyToRetrace$panelID]]$mode <- "default"
			
			traceGuideCounter$count <- (-1)
			
			readyToRetrace$imgName <- NULL
			readyToRetrace$rowName <- NULL
			readyToRetrace$panelID <- NULL
			readyToRetrace$directory <- NULL
			readyToRetrace$traceResults <- list()
		#}
	}else{
		print("skip len-0 trace")
		plotsPanel[[readyToRetrace$panelID]]$mode <- "default"
	}
}
