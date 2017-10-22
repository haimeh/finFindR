require("imager")

#helper function for cleaning up unwanted blobs
removeSparse <- function(blobs,img,minSize=1)
{
  margin <- 30
  for(blobValue in unique(blobs))
  {
    blobCoordinates <- get.locations(blobs,function(x){x==blobValue})
    if(any(blobCoordinates$x %in% c(1:floor((width(img)/margin))) |
           blobCoordinates$y %in% c(1:floor((height(img)/margin))) |
           blobCoordinates$x %in% c(width(img):width(img)-floor((width(img)/margin))) |
           blobCoordinates$y %in% c(height(img),height(img)-floor((height(img)/margin))) ) 
       || nrow(blobCoordinates)<=minSize)
    {
      blobs[blobs==blobValue] <- 0
    }
  }
  return(blobs)
}

cropFins <- function(imageName,minXY,saveDir)
{
  #evaluate for fins
  imageArray <- as.array(LBP(imageName,
                             150,
                             100,
                             -1,
                             8,
                             FALSE,
                             FALSE,
                             TRUE,
                             8))

  image <- medianblur( isoblur(as.cimg(predictLBPStack(imageArray,T)),.5) ,8,2)
  
  #find strong blobs
  blobs <- removeSparse(label(clean(fill(image>1,9),3),high_connectivity=F),image,25)
  edges <- clean(image>3,3)

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
      
      if(max(width,height) < 1.8*min(width,height))
      {
        # marginX <- ceiling((width)/5)+9
        # marginY <- ceiling((height)/5)+4
        marginX <- ceiling((width)/8)+4
        marginY <- ceiling((height)/8)+4
        
        mainImgName <- strsplit(basename(imageName),"\\.")[[1]][1]
    
        saveCrop(file.path(saveDir, paste0(mainImgName,"_",blobInc,".JPG")),
                 min(blobCoordinates$x)-marginX,
                 max(blobCoordinates$x)+marginX,
                 min(blobCoordinates$y)-marginY,
                 max(blobCoordinates$y)+marginY,
                 width(image),height(image),minXY)
      }
    }
  }
}