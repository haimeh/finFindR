

verifyRemove <- function(name,readyToRemove,hashRowSelection=NULL)
{
	# hash selection only significant for cluster view
	if(!is.null(hashRowSelection))
	{
		print(paste0("remove:",hashRowSelection))
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
				tempEnvir$hashData <- list(Trailing=NULL, Leading=NULL, Peduncle=NULL)
				tempEnvir$traceData <- list(Trailing=NULL, Leading=NULL, Peduncle=NULL)
				tempEnvir$idData <- NULL
				
				loopEnvir <- new.env()
				loopEnvir$hashData <- list(Trailing=NULL, Leading=NULL, Peduncle=NULL)
				loopEnvir$traceData <- list(Trailing=NULL, Leading=NULL, Peduncle=NULL)
				loopEnvir$idData <- NULL
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
							for(finPart in c("Trailing","Leading","Peduncle")){
								names(loopEnvir[["hashData"]][[finPart]]) <- normalizePath(file.path(RdataFile,names(loopEnvir$hashData[[finPart]])))
								names(loopEnvir[["traceData"]][[finPart]]) <- normalizePath(file.path(RdataFile,names(loopEnvir$traceData[[finPart]])))
							}
							names(loopEnvir[["idData"]]) <- normalizePath(file.path(RdataFile,names(loopEnvir$idData)))
						}
						
						# this check is needed for a problem with the origional pregen rdata
						# typically should not be needed
						#for(finPart in c("Trailing","Leading","Peduncle")){
						#	consistencyCheck <- c(length(loopEnvir$hashData[[finPart]]),length(loopEnvir$traceData[[finPart]]),length(loopEnvir$idData) )
						#	if(min(consistencyCheck) != max(consistencyCheck))
						#	{
						#		refNames <- list(names(loopEnvir$hashData[[finPart]]),names(loopEnvir$traceData[[finPart]]),names(loopEnvir$idData))[which.min(consistencyCheck)]
						#		
						#		loopEnvir$hashData[[finPart]] <- loopEnvir$hashData[[finPart]][unique(unlist(refNames))]
						#		loopEnvir$traceData[[finPart]] <- loopEnvir$traceData[[finPart]][unique(unlist(refNames))]
						#		loopEnvir$idData <- loopEnvir$idData[unlist(refNames)]
						#	}
						#}
						
						
						for(finPart in c("Trailing","Leading","Peduncle")){
							tempEnvir$hashData[[finPart]] <-  append(tempEnvir$hashData[[finPart]], loopEnvir$hashData[[finPart]])
							tempEnvir$traceData[[finPart]] <- append(tempEnvir$traceData[[finPart]],loopEnvir$traceData[[finPart]])

							#tempEnvir$hashData <- append(tempEnvir$hashData,loopEnvir$hashData)
							#tempEnvir$traceData <- append(tempEnvir$traceData,loopEnvir$traceData)
						}

						#tempEnvir$hashData <- append(tempEnvir$hashData,loopEnvir$hashData)
						#tempEnvir$traceData <- append(tempEnvir$traceData,loopEnvir$traceData)
						tempEnvir$idData <- append(tempEnvir$idData,loopEnvir$idData)
					}
				}
				print("test the envirs")
				rm(loopEnvir)
				
				if(!appendNew)
				{
					saveEnvir$hashData <- list()
					saveEnvir$traceData <- list()
					saveEnvir$idData <- NULL
				}

				for(finPart in c("Trailing","Leading","Peduncle")){
					saveEnvir$hashData[[finPart]] <- append(saveEnvir$hashData[[finPart]],tempEnvir$hashData[[finPart]])
					saveEnvir$traceData[[finPart]] <- append(saveEnvir$traceData[[finPart]],tempEnvir$traceData[[finPart]])
					#names(loopEnvir[["hashData"]][[finPart]]) <- normalizePath(file.path(RdataFile,names(loopEnvir$hashData[[finPart]])))
					#names(loopEnvir[["traceData"]][[finPart]]) <- normalizePath(file.path(RdataFile,names(loopEnvir$traceData[[finPart]])))
				}
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
