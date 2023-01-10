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
	if(length(readyToRetrace$traceResults)>0)
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
				targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName] <- as.data.frame(t(t(hashDataPart) %*% hashSVD$U %*% hashSVD$D),check.names=F)*1000
				#targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName] <- traceToHash( list(readyToRetrace$traceResults$annulus ), mxnetModel  )
			}
			print("retrace hash calculated")
			
			# --- recalculate rank table ------------------
			if(length(sessionReference$hashData[[input$segmentTarget]])>0)
			{
				#newDistances <- distanceToRef( (unlist(targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName])),
				#								data.frame(sessionReference$hashData[[input$segmentTarget]]))
				# NOTE: I think we can calculate these things seprately
				if(input$segmentTarget != "Trailing"){
					newDistances <- distanceToRef( unlist(targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName]),
													cbind(data.frame(sessionReference$hashData[[input$segmentTarget]]),(sessionReference$hashData[["Trailing"]]) ) )
				}
				# make sure to clip to correct number of columns
				#newSortingIndex <- order(newDistances)[1:ncol(rankTable$Name)]
				tmpNames <- sapply(strsplit(rownames(rankTableParts[[input$segmentTarget]]$Name)," : "),function(x)x[[1]])
				#queryNamesPrev <- names(queryTmp[["Trailing"]])
				#queryNames <- names(sessionQuery$hashData[[finPart]])
				#querySharedNames <- merge(x=cbind(queryNamesPrev,1), y=cbind(queryNames,1), by.x="queryNamesPrev", by.y="queryNames",  all.y=F, all.x=F)[,1]
				#queryTmp[[finPart]] <- lapply(querySharedNames,function(i){append(queryTmp[["Trailing"]][[i]],
				#													sessionQuery$hashData[[finPart]][[i]][1:8])})
				#names(queryTmp[[finPart]]) <- querySharedNames

				#referNamesPrev <- names(referTmp[["Trailing"]])
				#referNames <- names(sessionReference$hashData[[finPart]])
				#referSharedNames <- merge(x=cbind(referNamesPrev,1), y=cbind(referNames,1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]
				#referTmp[[finPart]] <- lapply(referSharedNames,function(i){append(referTmp[["Trailing"]][[i]],
				#													sessionReference$hashData[[finPart]][[i]][1:8])})
				#names(referTmp[[finPart]]) <- referSharedNames
				referSharedNames <- merge(x=cbind(referNamesPrev=names(sessionReference$hashData[["Trailing"]]),1), y=cbind(referNames=names(sessionReference$hashData[[input$segmentTarget]]),1), by.x="referNamesPrev", by.y="referNames",  all.y=F, all.x=F)[,1]
				newSortingIndex <- order(newDistances)[1:length(referSharedNames)]
				#newSortingIndex <- order(newDistances)[1:length(sessionReference$hashData[[input$segmentTarget]])]
				
				#rankTable$Name[readyToRetrace$rowName,] <- names(sessionReference$idData[newSortingIndex])
				#rankTable$NameSimple[readyToRetrace$rowName,] <- basename(names(sessionReference$idData[newSortingIndex]))
				#rankTable$ID[readyToRetrace$rowName,] <- sessionReference$idData[newSortingIndex]
				#rankTable$Unique[readyToRetrace$rowName,] <- !duplicated(sessionReference$idData[newSortingIndex])
				##rankTable$Distance[readyToRetrace$rowName,] <- newDistances[newSortingIndex]
				##rankTable$Distance[[input$segmentTarget]][readyToRetrace$rowName,] <- newDistances[newSortingIndex]

				rankTableParts[[input$segmentTarget]]$Name[readyToRetrace$rowName,] <- names(sessionReference$idData[newSortingIndex])
				rankTableParts[[input$segmentTarget]]$NameSimple[readyToRetrace$rowName,] <- basename(names(sessionReference$idData[newSortingIndex]))
				rankTableParts[[input$segmentTarget]]$ID[readyToRetrace$rowName,] <- sessionReference$idData[newSortingIndex]
				rankTableParts[[input$segmentTarget]]$Unique[readyToRetrace$rowName,] <- !duplicated(sessionReference$idData[newSortingIndex])
				rankTableParts[[input$segmentTarget]]$Distance[readyToRetrace$rowName,] <- newDistances[newSortingIndex]

		#rankTableUniqueOnlyParts[[input$segmentTarget]]$Name       <- rankTableUniqueOnlyParts[[input$segmentTarget]]$Name      [rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$Name      ) != rownames,]
		#rankTableUniqueOnlyParts[[input$segmentTarget]]$NameSimple <- rankTableUniqueOnlyParts[[input$segmentTarget]]$NameSimple[rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$NameSimple) != rownames,]
		#rankTableUniqueOnlyParts[[input$segmentTarget]]$ID         <- rankTableUniqueOnlyParts[[input$segmentTarget]]$ID        [rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$ID        ) != rownames,]
		#rankTableUniqueOnlyParts[[input$segmentTarget]]$Distance   <- rankTableUniqueOnlyParts[[input$segmentTarget]]$Distance  [rownames(rankTableUniqueOnlyParts[[input$segmentTarget]]$Distance  ) != rownames,]
				#for(finPart in c("Trailing","Leading","Peduncle")){
				#	DistanceParts[[finPart]]=NULL
				#	DistanceParts[[input$segmentTarget]][readyToRetrace$rowName,] <- newDistances[newSortingIndex]
				#}
				rankTable$editCount <- rankTable$editCount+1
				#updateDisplayTables(finPartSelected=input$targetFeatures, rankTableParts, rankTable)
			}
			# ---------------------------------------------
			
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
