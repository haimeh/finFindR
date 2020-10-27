#' @title hashFromImage 
#' @usage curl -v http://localhost:8004/ocpu/library/finFindR/R/hashFromImage/json\
#' -F "imageobj=@C:/Users/jathompson/Documents/dolphinTestingdb/jensImgs/test2.jpg"
#' 
#' hashFromImage(imageobj = "yourfile1.jpg")
#' @details \code{traceFromImage} wrapper for use through opencpu.
#' opencpu passes temp object name to function followed by \code{traceToHash}
#' Processes an image(cimg) containing a fin. 
#' First the image undergoes cleanup through a variety of filters and glare removal via
#' \code{constrainSizeFinImage} and \code{fillGlare}
#' These processes help enhance edge clarity.
#' The trailing edge is highlighted via neural network. 
#' The image is then cropped down to the trailing edge for efficiency purposes.
#' The canny edges are then extracted from the crop and passed to 
#' \code{traceFromCannyEdges}
#' which isolates coordinates for the trailing edge. These coordinates are then passed to
#' \code{extractAnnulus}
#' which collects image data used for identification.
#' Both the coordinates and the image annulus are then returned.
#' @param imageobj character vector which denotes image file "directory/finImage.JPG"
#' @return Value of type list containing:
#' "hash" vector specifying an individual
#' "coordinates" a matrix of coordinates
#' @export

hashFromImage <- function(imageobj, pathNet=NULL, hashNet=NULL)
{
  if(class(imageobj)=="character")
  {
    if(length(imageobj)==1){
      traceResults <- traceFromImage(fin=load.image(imageobj),
                                     startStopCoords = NULL,
                                     pathNet = pathNet)
      if(is.null(traceResults[[1]]) | is.null(traceResults[[2]])){return(traceResults)}
      hashResult <- traceToHash(traceData=list(traceResults$annulus), mxnetModel=hashNet)
      trailingEdge <- traceResults$coordinates
      return(list("hash"=hashResult,"coordinates"=trailingEdge))
    }else{
      traceImg <- list()
      for (imageName in imageobj)
      {
        print(imageName)
        traceResults <- traceFromImage(fin=load.image(imageName),
                       startStopCoords = NULL,
                       pathNet = pathNet)
        traceImg <- append(traceImg,list(traceResults$annulus))
        
      }
      names(traceImg) <- as.character(imageobj)
      return(as.data.frame(traceToHash(traceData=traceImg, mxnetModel=hashNet)))
    }
  }else{stop()}
}

#' @title hashFromImageAndEdgeCoord 
#' @usage curl -v http://localhost:8004/ocpu/library/finFindR/R/hashFromImageAndEdgeCoord/json \
#' -F "imageobj=@C:/Users/jathompson/Documents/dolphinTestingdb/jensImgs/test2.jpg"\
#' -F "xvec=[6,7,8,7,6,5,5,6,7,8,9]"\
#' -F "yvec=[3,4,5,6,6,5,6,6,7,8,9]"
#' 
#' hashFromImageAndEdgeCoord(
#'   imageobj = "yourfile1.jpg",
#'   xvec=c(3,4,5,6,6,5,6,7,8),
#'   yvec=c(6,7,8,7,6,5,5,6,7)
#' )
#' @details \code{extractAnnulus} wrapper for use through opencpu.
#' if coordinates are generated from finFindR, \code{constrainSizeFinImage} 
#' should be called by setting boundResize = 1
#' opencpu passes temp object name to function followed by \code{traceToHash}
#' Coordinates should denote the pixels along the trailing edge of the fin
#' \code{extractAnnulus}
#' which collects image data used for identification.
#' Coordinates assume the upper left corner is denoted as 1,1 (recall, R is 1 indexed)
#' @param imageobj character vector which denots image file "directory/finImage.JPG"
#' @param xCoordinates x pixel coordinates for data extraction
#' @param yCoordinates y pixel coordinates for data extraction
#' @return hash assiciated with the provided image and trailing edge
#' @export

hashFromImageAndEdgeCoord <- function(imageobj,xvec,yvec,boundResize=F)
{
  if(boundResize)
  {
    finImg <- constrainSizeFinImage(load.image(imageobj),1800,750)
  }else{
    finImg <- load.image(imageobj)
  }
  annulus <- extractAnnulus(imageFromR=finImg,xCoordinates=xvec,yCoordinates=yvec)
  return(traceToHash(list(annulus)))
}
