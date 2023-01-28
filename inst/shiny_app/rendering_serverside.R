#BUG: coordinates and trace null?
#plotFinTrace <- function(fin,coordinates,finComponent,showTrace)
plotFinTrace <- function(fin,coordinates,focusedCoordinates, mode, input, showTrace)
{
	if(length(fin)>0)
	{
		par(mar = c(0,0,0,0))
		if(length(coordinates)>0)
		{
			#dont forget to unlist coordinates
			#coordinates <- coordinates[[1]]
			#if(class(coordinates) == "matrix")
			#{
			#	print("uncompressed path")
			#	#coordinates <- do.call(rbind,coordinates)
			#	#coordinates <- coordinates
			#}else{
				print("decompressing coordinate path")
				#plotCoordinates <- decodePath(coordinates)
				if(mode=="fix"){
					#plotCoordinates <- do.call(rbind,focusedCoordinates[[input$segmentTarget]])
					if(length(focusedCoordinates) == 1){
						plotCoordinates <- array(1,c(1,2))
					}else{
						plotCoordinates <- decodePath(focusedCoordinates[[input$segmentTarget]])
					}
					#plotCoordinates <- focusedCoordinates
					#plotCoordinates <- decodePath(unlist(coordinates[[finComponent]]))
					#coordinates <- do.call(rbind,lapply(coordinates[[finComponent]]))
				}else{
						#browser()
					plotCoordinates <- coordinates
					#plotCoordinates <- do.call(rbind,lapply(coordinates,function(x){decodePath(unlist(x))}))
					#plotCoordinates <- coordinates
				}
			#}
			if(nrow(plotCoordinates) > 1 && showTrace)
			{
				plot(fin, ann=FALSE, asp = 1, axes = FALSE)
				par(new=TRUE)
				points(plotCoordinates[,1],plotCoordinates[,2],pch=".",col='red', ann=FALSE, asp = 0)
			}else{
				plot(fin, ann=FALSE, axes = FALSE)
			}
		}else{
			plot(fin, ann=FALSE, axes = FALSE)
		}
	}else{
		print("not an image")
	}

}

# --- set image header pannel
generateDisplayHeader <- function(instance,
								plotsPanel,
								mode="default",
								closeOption=T,
								fixOption=T){
	
	if(fixOption){
		modSelection <- tagList(
			actionButton(paste0("retrace",instance),"Fix"),
			actionButton(paste0("remove",instance),"Remove"),
			
			#radioButtons(
			#	inputId = paste0("segmentTarget"),
			#	label = "Target Fin Segment",
			#	choices = c("Trailing","Leading","Peduncle"),
			#	inline = T
			#),
			column(width = 3,

		checkboxInput(
				inputId = paste0("trace",instance),
				label = "Show Trace",
				value = FALSE
			))
			#column(width = 3,checkboxInput(
			#  inputId = paste0("trace",instance),
			#  label = "Show Trace",
			#  value = FALSE
			#))
		)
	}else{
		modSelection <- tagList(
			column(width = 4,
						 checkboxInput(
							 inputId = paste0("trace",instance),
							 label = "Show Trace",
							 value = TRUE
						 ))
		)
	}
	
	if(mode=="fix")
	{
		return(tagList(
			checkboxInput(
				inputId = paste0("trace",instance),
				label = "Show Trace",
				value = TRUE
			),
			radioButtons(
				inputId = paste0("segmentTarget"),
				label = "Target Fin Segment",
				choices = c("Trailing","Leading","Peduncle"),
				inline = T
			),
			actionButton(paste0("removeSegment",instance),"Remove Selected Segment"),

			h4("Click start point then Click end point"),
			actionButton(paste0("cancelRetrace",instance),"Cancel"),
			actionButton(paste0("saveRetrace",instance),"Keep Change")
		))
		
	}else{
		
		return(tagList(
			fluidRow(
				column(width = if(closeOption){9}else{12}, 
							 verbatimTextOutput(paste0("imageName",instance)),
							 tags$head(tags$style(paste0("#imageName",instance,"{overflow-x:hidden}"))),
							 
							 if(mode=="setID")
							 {
								 fluidRow(
									 column(width=9,
													textInput(paste0("textID",instance),label = NULL)),
									 column(width=3,
													actionButton(paste0("saveID",instance),label = "Set"))
								 )
							 }else{
								 fluidRow(
									 column(width=if(fixOption){8}else{12},
													tags$head(tags$style(paste0("#imageID",instance,"{overflow-x:hidden}"))),
													verbatimTextOutput(paste0("imageID",instance))),
									 if(fixOption)
									 {
										 column(width=3,
														actionButton(paste0("changeID",instance),"Change"))
									 }
								 )
							 }),
				if(closeOption)
				{
					column(width = 3,
								 actionButton(paste0("close",instance),"close"),
								 # FINISH LOCK
								 checkboxInput(
									 inputId = paste0("lock",instance),
									 label = "Lock",
									 value = ifelse(plotsPanel[[instance]]$locked,TRUE,FALSE)
								 )
					)
				}
			),modSelection
		))
	}
}
