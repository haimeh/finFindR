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
#' @param imageobj character vector which denotes image file "directory/finImage.JPG" or a list of such to be processed in parallel
#' @param cores int indicating number of parallel workloads to be handled. Default: 8
#' @return Value of type list containing:
#' "hash" vector specifying an individual
#' "coordinates" a matrix of coordinates
#' @export

hashFromImage <- function(imageobj, cores=8, pathNet=NULL, hashNet=NULL)
{
  if(class(imageobj)=="character")
  {
    if(length(imageobj)==1){
      traceResults <- traceFromImage(fin=load.image(imageobj),
                                     startStopCoords = NULL,
                                     pathNet = pathNet)
      if(is.null(traceResults[[1]]) | is.null(traceResults[[2]])){return(traceResults)}
      hashResult <- traceToHash(traceData=list(traceResults$annulus), mxnetModel=hashNet)
      edgeCoords <- traceResults$coordinates
      return(list("hash"=hashResult,"coordinates"=edgeCoords))
    }else{
      annulus_coordinates = parallel::mclapply(imageobj, function(imageName){
          returnObj <- list()
          traceResults <- traceFromImage(fin=load.image(imageName),
                         startStopCoords = NULL,
                         pathNet = pathNet)
          returnObj[paste0(imageName,"_ann")] <- list(traceResults$annulus)
          returnObj[paste0(imageName,"_coo")] <- list(traceResults$coordinates)
          return(returnObj)
        }, mc.cores=cores)
      annulusImgs <- sapply(annulus_coordinates,function(x){x_tmp <- x[1]; names(x_tmp) <- substr(names(x_tmp), 0,nchar(names(x_tmp))-4); return(x_tmp)} )
      edgeCoords <- sapply(annulus_coordinates,function(x){x_tmp <- x[2]; names(x_tmp) <- substr(names(x_tmp), 0,nchar(names(x_tmp))-4); return(x_tmp)} )

      return(list("hash"=traceToHash(traceData=annulusImgs, mxnetModel=hashNet),
                  "coordinates"=edgeCoords))
    }
  }else{stop()}
}

hashesFromImages <- function(...){
  cores=8
  if(all(sapply(list(...),class)=="character")){
    annulus_coordinates = parallel::mclapply(list(...), function(imageName){
        returnObj <- list()
        traceResults <- traceFromImage(fin=load.image(imageName),
                       startStopCoords = NULL,
                       pathNet = pathNet)
        returnObj[paste0(imageName,"_ann")] <- list(traceResults$annulus)
        returnObj[paste0(imageName,"_coo")] <- list(traceResults$coordinates)
        return(returnObj)
      }, mc.cores=cores)
    annulusImgs <- sapply(annulus_coordinates,function(x)
                          {x_tmp <- x[1]; names(x_tmp) <- substr(names(x_tmp), 0,nchar(names(x_tmp))-4); return(x_tmp)} )
    edgeCoords <- sapply(annulus_coordinates,function(x)
                         {x_tmp <- x[2]; names(x_tmp) <- substr(names(x_tmp), 0,nchar(names(x_tmp))-4); return(x_tmp)} )
    return(list("hash"=traceToHash(traceData=annulusImgs, mxnetModel=hashNet),
                "coordinates"=edgeCoords))
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
    finImg <- constrainSizeFinImage(load.image(imageobj),2000,750)
  }else{
    finImg <- load.image(imageobj)
  }
  annulus <- extractAnnulus(imageFromR=finImg,xCoordinates=xvec,yCoordinates=yvec)
  return(traceToHash(list(annulus)))
}
