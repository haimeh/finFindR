#' @title traceFinFromHttp 
#' @details \code{traceFromImage} wrapper for use through opencpu.
#' opencpu passes temp object name to function followed by \code{traceToHash}
#' curl -v http://localhost:8004/ocpu/library/finFindR/R/processFinFromHttp/json -F "imageobj=@C:/Users/jathompson/Documents/dolphinTestingdb/test2.jpg"
#' aka: traceFinFromHttp(imageobj = list("yourfile1.jpg","yourfile2.jpg"))
#'
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
#' @param fin Value of type cimg. Load the image via load.image("directory/finImage.JPG")
#' @param startStopCoords list of 3 coordinates: leadingEnd, startPoint, trailingEnd. If NULL, these points are estimated
#' @param pathNet mxnet model for isolating trailing edge
#' @return Value of type list containing:
#' "coordinates" a dataframe of coordinates
#' "annulus" a 3 channel image of isolated features
#' @export

processFinFromHttp <- function(imageobj)
{
  if(class(imageobj)=="character")
  {
    traceResults <- traceFromImage(fin=load.image(imageobj),
                                   startStopCoords = NULL,
                                   pathNet = NULL)
    return(as.data.frame(traceToHash(list(traceResults$annulus))))
  }else{
    traceImg <- list()
    for (imageName in imageobj)
    {
      traceResults <- traceFromImage(fin=load.image(imageName),
                     startStopCoords = NULL,
                     pathNet = NULL)
      traceImg <- append(traceImg,list(traceResults$annulus))
      
      names(traceImg) <- as.character(imageobj)
    }
    return(as.data.frame(traceToHash(traceImg)))
  }
}

