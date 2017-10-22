require("mxnet")

finFinder <- mx.model.load("LinRmodel_59",0500)
predictLBPStack <- function(imageArray,standardize=T)
{
  imgDim <- dim(imageArray)
  dim(imageArray) <- c(imgDim[1]*imgDim[2],imgDim[4])

  imagePrediction <- t(predict(finFinder,t(imageArray),array.layout = "colmajor"))
  
  if(standardize){imagePrediction <- (imagePrediction-mean(imagePrediction))/sd(imagePrediction)}
  dim(imagePrediction) <- c(imgDim[1],imgDim[2])
  
  return(imagePrediction)
}
