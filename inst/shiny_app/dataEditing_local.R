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
		browser("prepRetrace24")
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


		#for(finPart in c("trailing","leading")){
			print("prep trace")
			#browser()
			# TODO: do we just retrace the whole thing?
			targetEnvir$traceData[[input$segmentTarget]][readyToRetrace$imgName] <- list(encodePath(readyToRetrace$traceResults$coordinates))
			targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName] <- traceToHash( list(readyToRetrace$traceResults$annulus ), mxnetModel  )
			print("retrace hash calculated")
			
			# --- recalculate rank table ------------------
			if(length(sessionReference$hashData)>0)
			{
				newDistances <- distanceToRef( (unlist(targetEnvir$hashData[[input$segmentTarget]][readyToRetrace$imgName])),
												data.frame(sessionReference$hashData[[input$segmentTarget]]))
				# make sure to clip to correct number of columns
				newSortingIndex <- order(newDistances)[1:ncol(rankTable$Name)]
				
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

				#for(finPart in c("Trailing","Leading","Peduncle")){
				#	DistanceParts[[finPart]]=NULL
				#	DistanceParts[[input$segmentTarget]][readyToRetrace$rowName,] <- newDistances[newSortingIndex]
				#}
				rankTable$editCount <- rankTable$editCount+1
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
	}
}
