
#helper function for cleaning up unwanted blobs
# removeSparse <- function(blobs,minSize=1)
# {
#   for(blobValue in unique(blobs))
#   {
#     blobCoordinates <- get.locations(blobs,function(x){x==blobValue})
#     if(nrow(blobCoordinates)<=minSize)
#     {
#       blobs[blobs==blobValue] <- 0
#     }
#   }
#   return(blobs)
# }

#' @param imageName path to image to be cropped
#' @param cropNet path to image to be cropped
#' @param saveDir directory to save cropped image
#' @export
cropFins <- function(imageName,cropNet,saveDir,minXY=200)
{
  if(!("MXFeedForwardModel" %in% class(cropNet))){stop("network must be of class MXFeedForwardModel")}
  
  image <- try(initializeImg(imageName,newX = 150,newY = 100))
  if(!is.cimg(image)){stop(paste("failed to load image:",imageName))}
  
  image <- as.array(image)
  dim(image) <- c(150,100,3,1)
  if(max(image)>1){image <- image/255}
  
  netOut <- mxnet:::predict.MXFeedForwardModel(X=image,
                                               model=cropNet,
                                               ctx=mxnet::mx.cpu(),
                                               array.layout = "colmajor")
  
  #find strong blobs
  #blobs <- removeSparse(label(netOut>.4,high_connectivity=F),25)
  blobs <- label(clean(netOut>.25,5),high_connectivity=F)
  edges <- clean(netOut>.75,3)
  
  keepers <- table(blobs*edges)
  if(length(keepers) > 1)
  {
    print(paste(length(keepers)-1,"fin(s)"))
    blobs[!(blobs %in% as.integer(names(which(keepers>1))))] <- 0
    
    #crop out each fin
    blobInc <- 0
    for(blobValue in unique(blobs)[which(unique(blobs)>0)])
    {
      blobInc <- blobInc+1
      blobCoordinates <- get.locations(blobs,function(x)x==blobValue)
      
      width <- max(blobCoordinates$x)-min(blobCoordinates$x)
      height <- max(blobCoordinates$y)-min(blobCoordinates$y)
      
      if(max(width,height) < 3*min(width,height))
      {
        marginX <- ceiling((width)/4)
        marginY <- ceiling((height)/4)
        
        mainImgName <- strsplit(basename(imageName),"\\.")[[1]][1]
        
        saveCrop(file.path(saveDir, paste0(mainImgName,"_",blobInc,".JPG")),
                 min(blobCoordinates$x)-marginX,
                 max(blobCoordinates$x)+marginX,
                 min(blobCoordinates$y)-(2*marginY),
                 max(blobCoordinates$y)+marginY,
                 width(image),height(image),minXY)
        unlink( list.files(dirname(tempdir()), pattern = "\\.PPM$|\\.ppm$", full.names = T) )
      }
    }
  }
}