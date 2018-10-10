
plotFinTrace <- function(fin,coordinates,trace)
{
  if(length(fin)>0)
  {
    par(mar = c(0,0,0,0))
    if(length(coordinates)>0)
    {
      #dont forget to unlist coordinates
      coordinates <- coordinates[[1]]
      if(nrow(coordinates) > 0 && trace)
      {
        plot(fin, ann=FALSE, asp = 1, axes = FALSE)
        par(new=TRUE)
        points(coordinates[,1],coordinates[,2],pch=".",col='red', ann=FALSE, asp = 0)
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
                                  mode="default",
                                  closeOption=T,
                                  fixOption=T){
  
  if(fixOption){
    modSelection <- tagList(
      actionButton(paste0("retrace",instance),"Fix"),
      actionButton(paste0("remove",instance),"Remove"),
      
      column(width = 3,checkboxInput(
        inputId = paste0("trace",instance),
        label = "Trace",
        value = FALSE
      ))
    )
  }else{
    modSelection <- tagList(
      column(width = 4,
             checkboxInput(
               inputId = paste0("trace",instance),
               label = "Trace",
               value = TRUE
             ))
    )
  }
  
  if(mode=="fix")
  {
    return(tagList(
      h4("Click start point then Click end point"),
      actionButton(paste0("cancelRetrace",instance),"Cancel"),
      actionButton(paste0("saveRetrace",instance),"Save")
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