

verifyRemove <- function(name,readyToRemove,hashRowSelection=NULL)
{
  # hash selection only significant for cluster view
  if(!is.null(hashRowSelection))
  {
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
