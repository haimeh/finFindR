
#' @title coordinateMatrix
#' @description Function to compress pixel coordinates for trailing edge
#' @param coordinateMatrix vector containing a hash for matching
#' @return A vector represenatation of an rle object with start ing pixel coordinates
#' @export 
encodePath <- function(coordinateMatrix)
{
  startXY <- as.integer(coordinateMatrix[1,])
  coordinateEncoding <- c(startXY, as.integer(unlist(
                            rle(as.integer(c(diff(coordinateMatrix[,1]),
                                             diff(coordinateMatrix[,2]))))
                            )))
  return(coordinateEncoding)
}

#' @title coordinateEncoding
#' @description Function to decompress rle + start coordinates into image pixel coordinates for trailing edge
#' @param coordinateEncoding rle object with start coordinates
#' @return matrix of x,y pixel coordinates
#' @export 
decodePath <- function(coordinateEncoding)
{
  x <- coordinateEncoding[1]
  y <- coordinateEncoding[2]
  rleMatrix <- matrix(coordinateEncoding[-c(1,2)],ncol=2,nrow=(length(coordinateEncoding)-2)/2)
  rleReconst <- list(lengths = rleMatrix[,1],values = rleMatrix[,2])
  class(rleReconst) <- "rle"
  pathDiffsAppended <- inverse.rle(rleReconst)
  pathDiffs <- apply(matrix(pathDiffsAppended, nrow=length(pathDiffsAppended)/2, ncol=2),2,cumsum)
  path <- rbind(c(x,y),t(t(pathDiffs)+c(x,y)))
  print("successful decompression")
  return(path)
}
