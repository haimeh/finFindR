
#' @param imageName path to image to be cropped
#' @param cropNet path to image to be cropped
#' @param saveDir directory to save cropped image
#' @export
cropFins <- function(imageName,cropNet,workingImage,saveDir,minXY=100)
{
  if(!("MXFeedForwardModel" %in% class(cropNet))){stop("network must be of class MXFeedForwardModel")}
  
  workingImage$origImg <-  suppressWarnings( as.cimg(aperm(jpeg::readJPEG(imageName,F),c(2,1,3))) )
  
  image <- resize(workingImage$origImg,size_x = 150, size_y = 100, interpolation_type = 3)
  dim(image) <- c(150,100,3,1)
  image <- image/max(image)
  
  netOut <- mxnet:::predict.MXFeedForwardModel(X=image,
                                               model=cropNet,
                                               ctx=mxnet::mx.cpu(),
                                               array.layout = "colmajor")
  #browser()
  #plot(as.cimg(netOut))
  #find strong blobs
  blobs <- erode_square(label(dilate_square(isoblur(netOut,.5) >.45,3),high_connectivity=F),3) 
  plot(blobs)
  edges <- netOut>.75
  
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
      blobCoordinates <- get.locations(blobs,function(x)x==blobValue)-1
      
      width <- max(blobCoordinates$x)-min(blobCoordinates$x)
      height <- max(blobCoordinates$y)-min(blobCoordinates$y)
      
      if(max(width,height) < 3*min(width,height))
      {
        marginX <- ceiling((width)/3)
        marginY <- ceiling((height)/3)
        
        mainImgName <- strsplit(basename(imageName),"\\.")[[1]][1]
        
        stretchX <- ceiling(width(workingImage$origImg)/width(image))
        stretchY <- ceiling(height(workingImage$origImg)/height(image))
        
        xSpan <- c(max( stretchX*(min(blobCoordinates$x)-marginX) ,1),
                   min( stretchX*(max(blobCoordinates$x)+marginX) ,width(workingImage$origImg)) )
        ySpan <- c(max( stretchY*(min(blobCoordinates$y-1)-marginY) ,1),
                   min( stretchY*(max(blobCoordinates$y-1)+marginY) ,height(workingImage$origImg)))
        
        if(diff(xSpan) > minXY && diff(ySpan) > minXY)
        {
        save.image(suppressWarnings(as.cimg(workingImage$origImg[xSpan[1]:xSpan[2],
                                        ySpan[1]:ySpan[2],,]))  ,file=file.path(saveDir, paste0(mainImgName,"_",blobInc,".jpg")),1.0)
        }
        unlink( list.files(dirname(tempdir()), pattern = "\\.PPM$|\\.ppm$", full.names = T) )
      }
    }
  }
}