require("imager")

#' Parameter initialization
#' searchDirectory Directory for processing
#' saveDirectory Location for cropped images
#' minXY Image must be both wider and higher than minXY threshold
#' includeSubDIr search sub directory for jpg files
#' cropDirectory("L:/861-finFindR/SAB/SAB photos","L:/861-finFindR/SAB/SAB crops")
#' @export
cropDirectory <- function(searchDirectory,
                          saveDirectory,
                          minXY=600,
                          includeSubDir=T)
{
  queryImgs <- list.files(searchDirectory, 
                          pattern = "\\.JPG$|\\.jpg$", 
                          full.names = T, 
                          recursive = includeSubDir)
  completeImgs <- list.files(saveDirectory, 
                             pattern = "\\.JPG$|\\.jpg$", 
                             full.names = F, 
                             recursive = F)
  mainImgName <- lapply(queryImgs, function(imageName){strsplit(basename(imageName),"\\.")[[1]][1]})
  cropedImgNames <- lapply(completeImgs, function(imageName){strsplit(basename(imageName),"_")[[1]][1]})
  incompleteIndex <- which(!(mainImgName %in% cropedImgNames))
  
  for(img in queryImgs[incompleteIndex])
  {
    print("==== + ====")
    print(basename(img))
    try(cropFins(img,minXY,saveDirectory))
  }
}