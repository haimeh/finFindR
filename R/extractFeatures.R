
# require("imager")
# require("Rcpp")
# require("mxnet")
#cimg.use.openmp("never")

###########################################################################################
# find the most efficient path and extract angles from it
#########################################################################################

#moving sum
msum <- function(x,n=5,sides=1){filter(x,rep(1,min(n,length(x))), sides=sides)}

filterFeatures <- function(pathMap,
                           angleGrad,
                           angleColor,
                           startPoint,
                           endPoint,
                           prox,
                           resizeRatio,
                           cropStart)
{
  if(!anyNA(c(startPoint,
              endPoint,
              prox)))
  {
    radiusLimit <- sqrt(sum((startPoint-endPoint)^2))
    
    xrange <- range(c(startPoint[1],endPoint[1],which(rowSums(pathMap)>.25)))
    
    yrange <- range(c(startPoint[2],endPoint[2],which(colSums(pathMap)>.25)))
    
    path <- findPath(pathMap,
                     startPoint[1],
                     startPoint[2],
                     endPoint[1],
                     endPoint[2],
                     minX=max(xrange[1]-1,1),
                     maxX=min(xrange[2]+1,nrow(pathMap)-1),
                     minY=max(yrange[1]-1,1),
                     maxY=min(yrange[2]+1,ncol(pathMap)-1),
                     radiusLimit/prox)#proximity for completion
    anglesByColor <- extractAngles(angleColor,
                                   path,
                                   startPoint[1],
                                   startPoint[2])
    anglesByGrad <- extractAngles(angleGrad,
                                  path,
                                  startPoint[1],
                                  startPoint[2])
    
    if(length(path)<100){return(list(NULL,NULL,NULL))}
    
    # this section transforms the path vector found by the findPath function into coordinates for r to plot
    stepX=c( 0, 1, 1, 1, 0, -1, -1, -1)
    stepY=c(-1,-1, 0, 1, 1,  1,  0, -1)
    
    pathLength <- length(path)
    plotpath <- matrix(0,nrow=pathLength,ncol=2)
    Xposition <- startPoint[1]
    Yposition <- startPoint[2]
    
    for(i in pathLength:1)
    {
      plotpath[i,1] <- Xposition
      plotpath[i,2] <- Yposition
      Xposition <- Xposition+stepX[path[i]+1]
      Yposition <- Yposition+stepY[path[i]+1]
    }
    plotpath <- plotpath[pathLength:1,]
    anglesByColor[is.na(anglesByColor)] <- pi/2
    anglesByGrad[is.na(anglesByGrad)] <- pi/2
    
    #make sure we select tiptop
    tipValues <- head(sort(plotpath[,2],decreasing = F),5)
    tipIndex <- max(which(plotpath[,2] %in% tipValues ))
    anglesByColor <- anglesByColor[tipIndex:nrow(plotpath)]
    anglesByGrad <- anglesByGrad[tipIndex:nrow(plotpath)]
    plotpath <- plotpath[tipIndex:nrow(plotpath),]
    
    #remove top sprew
    #startCut <- 1
    sprewX <- abs(diff(plotpath[2:nrow(plotpath),2]))
    sprewX[is.na(sprewX)] <- 0
    sprewY <- abs(diff(plotpath[2:nrow(plotpath),1]))
    sprewY[is.na(sprewY)] <- 0
    sprew <- sprewX|sprewY
    startCut <- min(which(msum(n=5,sprew)/5 > 0),na.rm = T)
    
    #remove bottom sprew
    sprew <- abs(diff(plotpath[seq_len(nrow(plotpath)-10),1]))
    sprew[is.na(sprew)] <- 0
    endCut <- max(which(msum(n=20,sprew)/20 > 0),na.rm = T)
    
    # plotpath <- round(approx(plotpath[startCut:endCut,],
    #                          n=round(diff(c(startCut,endCut))/resizeRatio ))$y)
    plotpath <- plotpath[startCut:endCut,]
    
    plotpath <- cbind(round(plotpath[,1]/resizeRatio+cropStart[1] ),
                      round(plotpath[,2]/resizeRatio+cropStart[2]))
    # plotpath <- cbind(round(approx(plotpath[,1]/resizeRatio,n=ceiling(pathLength/resizeRatio ))$y+cropStart[1] ),
    #                   round(approx(plotpath[,2]/resizeRatio,n=ceiling(pathLength/resizeRatio ))$y+cropStart[2]))
    anglesByColor <- anglesByColor[startCut:endCut]
    anglesByGrad <- anglesByGrad[startCut:endCut]
    
    
    #plot(anglesByGrad,pch=".",col="red");lines(anglesByColor,col="blue");lines(anglesByGrad,col="green")
    return(list(anglesByGrad,anglesByColor,plotpath))
  }else{
    return(list(NULL,NULL,NULL))
  }
}


########################################################################################
# Isolate and extract features for recognition
########################################################################################

#' @param fin Value of type cimg. Load the image via load.image("directory/finImage.JPG")
#' @param startStopCoords list of 3 coordinates: leadingEnd, startPoint, trailingEnd. If NULL, these points are estimated
#' @param pathNet mxnet model for isolating trailing edge
#' @return Value of type List containing vector of radians and dataframe of plotpath coordinates
#' @export
isolateCurve <- function(fin,
                         startStopCoords = NULL,
                         pathNet = NULL)
{
  if(!is.cimg(fin)){stop("fin must be of type cimg")}
  if(!("MXFeedForwardModel" %in% class(pathNet))){stop("network must be of class MXFeedForwardModel")}
  
  if(max(fin)>1){fin <- fin/255}
  #finOG <- fin
  
  #### --- Highlight Trailing Edge --- ####
  
  netIn <- resize(fin,size_x = 60,size_y = 48,interpolation_type = 2)
  estHighlight <- threshold(netIn,.97)
  
  cropRot <- dilate_square((netIn==0.0),5) | dilate_square((netIn==1.0),3)
  if(any(cropRot))
  {
    netIn[as.logical(cropRot)]<-0
    netIn <- fillGlare(netIn, get.locations(cropRot,function(x){x==TRUE})-1)
  }
  estExtractedSilhouette <- cimg(array(0,dim(netIn)))
  R(estExtractedSilhouette) <- G(netIn)-R(netIn)
  G(estExtractedSilhouette) <- B(netIn)-R(netIn)
  B(estExtractedSilhouette) <- B(netIn)-G(netIn)
  
  netIn <- as.array(netIn)
  
  #plot(as.cimg(netIn))
  netOutResizeFactors <- c(dim(fin)[1]/60,dim(fin)[2]/48)
  netIn <- append(netIn,netIn[60:1,,,])
  
  dim(netIn) <- c(60,48,3,2)
  
  netOut <- mxnet:::predict.MXFeedForwardModel(X=netIn,model=pathNet,ctx=mxnet::mx.cpu(),array.layout = "colmajor")
  dim(netOut) <- c(60,48,2)
  netOut <- parmax(list(as.cimg(netOut[,,1]),as.cimg(netOut[60:1,,2])))
  
  # --- if no fin found
  #plot(netOut>.5)
  if(!any(netOut>=.4)){print("No fin found");return(list(NULL,NULL,NULL))}
  
  netblb <- label(netOut>.4,high_connectivity = TRUE)
  counts <- table(netblb)
  initBlobIndex <- as.integer(names(which(counts<=5)))
  netblb[which(netblb %in% initBlobIndex)] <- 0
  
  xSpan <- as.numeric(rowSums( dilate_square((netblb>0),5) ))
  xSpan[is.na(xSpan)] <- 0
  xSpan <- range(which(xSpan>mean(xSpan)/2))
  ySpan <- as.numeric(colSums( dilate_square((netblb>0),5) ))
  ySpan[is.na(ySpan)] <- 0
  ySpan <- range(which(ySpan>mean(ySpan)/2))
  
  netOut <- as.cimg(netOut[c(xSpan[1]:xSpan[2]),c(ySpan[1]:ySpan[2]),,])
  
  
  # --- crop fin to edge
  
  resizeSpanX <- netOutResizeFactors[1]*xSpan
  resizeSpanY <- netOutResizeFactors[2]*ySpan
  
  # startPoint <- (startStopCoords[[1]]*resizeFactor)-(cumuResize*c(xSpan[1],ySpan[1]))
  # endPoint <- (startStopCoords[[2]]*resizeFactor)-(cumuResize*c(xSpan[1],ySpan[1]))
  
  if(!is.null(startStopCoords))
  {
    resizeSpanX <- c(min(startStopCoords[[1]][1]-1,startStopCoords[[2]][1]-1,floor(resizeSpanX[1])),
                     max(startStopCoords[[1]][1]+1,startStopCoords[[2]][1]+1,ceiling(resizeSpanX[2])))
    resizeSpanY <- c(min(startStopCoords[[1]][2]-1,startStopCoords[[2]][2]-1,floor(resizeSpanY[1])),
                     max(startStopCoords[[1]][2]+1,startStopCoords[[2]][2]+1,ceiling(resizeSpanY[2])))
  }
  fin <- suppressWarnings(as.cimg(fin[ floor(resizeSpanX[1]):ceiling(resizeSpanX[2]) ,
                                       floor(resizeSpanY[1]):ceiling(resizeSpanY[2]),,]))
  
  if(height(fin) > 2000)
  {
    print("Image too large... Resizing...")
    resizeFactor <- 2000/height(fin)
    
    fin <- resize(fin, size_x = round(2000*(width(fin)/height(fin))), size_y = 2000, interpolation_type = 6)
  }
  else if(height(fin) < 750)
  {
    print("Image too small... Resizing...")
    resizeFactor <- 750/height(fin)
    
    fin <- resize(fin, size_x = round(750*(width(fin)/height(fin))), size_y = 750, interpolation_type = 5)
  }else{
    resizeFactor <- 1
  }

  cumuResize <- (netOutResizeFactors*resizeFactor)
  
  edgeFilter <- resize( netOut ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)/max(netOut)
  
  if(!any(netOut>.5))
  {
    yRange <- round(diff(range(which((colSums(netOut/max(netOut) >.5)>0))))*cumuResize[2] )
  }else{
    yRange <- round(diff(range(which((colSums(netOut>.5)>0))))*cumuResize[2] )
  }
  
  
  
  dilateFactor <- ceiling(yRange/200)
  dilateFactor <- dilateFactor+ifelse(as.logical(dilateFactor%%2),0,1)
  
  
  glareBound <- (sum(!estHighlight)/prod(dim(estHighlight)))
  if(glareBound > .95)# && glareBound < 1)
  {
    print("removing glare")
    highlightBlob <-threshold(fin,.97)#90
    glare <- threshold(fin,.99)
    
    highlightBlob <- label(highlightBlob)
    keepers <- unique(highlightBlob*glare)
    highlightBlob[!(highlightBlob %in% keepers)] <- 0
    highlightBlob <- (highlightBlob==0)
    highlightBlob <- erode_square(highlightBlob,3)
    
    fin <- fin*highlightBlob
    fin <- fillGlare(fin, get.locations(highlightBlob,function(x){x==FALSE})-1)
    print("glare clear")
  }
  
  print("smoothing separation")
  fin <- erode_square(fin,dilateFactor )
  fin <- dilate_square(fin,dilateFactor )
  
  estImageMean <- mean(netIn)
  estImageSplit <- netIn>estImageMean
  blurFactor <- min(sd(netIn[estImageSplit]),sd(netIn[!estImageSplit]) )
  
  fin <- isoblur(fin,ceiling(yRange/800))
  fin <- medianblur(fin,ceiling(yRange/80),blurFactor)
  
  print("forground-background complete")

  ###############################################################################################
  # Determine if image has a strong silhouette(bimodal brightness)
  ###############################################################################################
  
  #for estimating bimodality
  silhouette = TRUE
  bimodalThreshold = .25#.2-.35
  
  finMean <- mean(netIn)
  separationEstimation <- as.logical(netIn>finMean)
  upperFinSD <- sd(netIn[separationEstimation])
  lowerFinSD <- sd(netIn[!separationEstimation])
  
  #we count how many pixels fall within the range
  bimodFin <- sum(netIn > finMean-lowerFinSD & netIn < finMean+upperFinSD)/length(netIn)

  #G(fin)-R(fin)-B(fin)/15
  extractedSilhouette <- cimg(array(0,dim(fin)))
  R(extractedSilhouette) <- G(fin)-R(fin)
  G(extractedSilhouette) <- B(fin)-R(fin)
  B(extractedSilhouette) <- B(fin)-G(fin)
  
  estimatedSilhouette <- parmax.abs(list(R(estExtractedSilhouette),
                                         G(estExtractedSilhouette),
                                         B(estExtractedSilhouette)))
  
  #hist(estimatedSilhouette,1000)
  estMiddle <- mean(estimatedSilhouette)#0
  estForgoundBackgroundSplit <- as.logical(estimatedSilhouette>estMiddle)
  upperExtractedSilhouetteSD <- sd(estimatedSilhouette[estForgoundBackgroundSplit])
  lowerExtractedSilhouetteSD <- sd(estimatedSilhouette[!estForgoundBackgroundSplit])
  
  # SilhouetteFactor <- min( upperExtractedSilhouetteSD,lowerExtractedSilhouetteSD )
  # extractedSilhouette <- isoblur(extractedSilhouette,ceiling(yRange/500))
  # extractedSilhouette <- medianblur(extractedSilhouette,ceiling(yRange/50),SilhouetteFactor)
  
  
  
  # now we find out if the brightness distribution is bimodal
  bimodExtractedSilhouette <- sum(estimatedSilhouette > estMiddle-lowerExtractedSilhouetteSD & 
                                  estimatedSilhouette < estMiddle+upperExtractedSilhouetteSD)/length(estimatedSilhouette)
  
  # after all this processing, if the estimatedSilhouette distribution is strongly bimodal and the origional image is not, we use estimatedSilhouette
  if (bimodExtractedSilhouette < bimodalThreshold && 
      bimodExtractedSilhouette < bimodFin)
  {
    silhouette = FALSE
  }
  
  
  
  
  ################################################################################
  # generate the canny edge image
  ################################################################################
  
  ##average
  ##parmax.abs
  flatten <- function(imgLst){return( parmax.abs(imgLst) )}
  
  dx <- imgradient(fin,"x")
  dx <- isoblur(dx,yRange/800)#700
  dx <- flatten(list(R(dx),G(dx),B(dx)))
  
  
  dy <- imgradient(fin,"y")
  dy <- isoblur(dy,yRange/800)
  dy <- flatten(list(R(dy),G(dy),B(dy)))
  
  angleGrad <- atan(dy/dx)
  
  sorbel <- abs(dx)+abs(dy)
  
  
    
  extractedSilhouetteDX <- imgradient(extractedSilhouette,"x")
  extractedSilhouetteDX <- isoblur(extractedSilhouetteDX,yRange/600)#400
  extractedSilhouetteDX <- flatten(list(R(extractedSilhouetteDX),G(extractedSilhouetteDX),B(extractedSilhouetteDX)))
  
  extractedSilhouetteDY <- imgradient(extractedSilhouette,"y")
  extractedSilhouetteDY <- isoblur(extractedSilhouetteDY,yRange/600)
  extractedSilhouetteDY <- flatten(list(R(extractedSilhouetteDY),G(extractedSilhouetteDY),B(extractedSilhouetteDY)))
  
  angleColor <- atan(extractedSilhouetteDY/extractedSilhouetteDX)
    
  if(silhouette == FALSE)
  {
    extractedSorbel <- abs(extractedSilhouetteDX)+abs(extractedSilhouetteDY)
    
    qExtractedSorbel <- quantile(extractedSorbel,.97)
    qSorbel <- quantile(sorbel,.97)
    
    dx <- average(list(extractedSilhouetteDX/qExtractedSorbel,dx/qSorbel))
    dy <- average(list(extractedSilhouetteDY/qExtractedSorbel,dy/qSorbel))
    
    sorbel <- average(list(sorbel/qSorbel,extractedSorbel/qExtractedSorbel))
  }
  
  angle <- atan(dy/dx)

  
  rawEdges <- extractEdgeMap(sorbel,angle)
  rawEdges[1,,,] <- 0
  rawEdges[,1,,] <- 0
  rawEdges[width(rawEdges),,,] <- 0
  rawEdges[,height(rawEdges),,] <- 0
  
  strong <- (rawEdges/max(rawEdges*(edgeFilter>.65))*(edgeFilter)) > .1# mean(netFilter)/20#+sd(netFilter)
  strong <- strong | rawEdges > quantile(rawEdges,.99)
  
  edgeBlobs <- label( (rawEdges>0) ,high_connectivity = T)
  keepers <- unique(edgeBlobs* strong )
  
  edgeBlobs[!(edgeBlobs %in% keepers)] <- 0
  minimalEdge <- (edgeBlobs>0)
  
  
  
  ########################################################################################
  # find a starting point
  ########################################################################################
  
  
  if(is.null(startStopCoords))
  {
    print("estimating start stop points")
    xDensity <- approx(as.numeric(msum(rowSums(netOut),7,2)) ,n=width(fin))$y
    startEstX <- which((xDensity)==max((xDensity),na.rm = TRUE))[1]
    
    #blobFilter <- resize(dilate_square(netOut,3),size_x = width(fin) , size_y = height(fin),interpolation_type = 2)/max(netOut)
    #splitBlobs <- ( (netOut/max(netOut))>.5 )
    
    netBlobOri <- dilate_rect( ((netOut/max(netOut))>.4) ,sx=3,sy=3)
    
    netBlobOri[1,,,] <- 0
    netBlobOri[,1,,] <- 0
    netBlobOri[width(netBlobOri),,,] <- 0
    netBlobOri[,height(netBlobOri),,] <- 0
    netBlobOri <- label( netBlobOri, high_connectivity = T)
    
    netBlob <- resize(netBlobOri,size_x = width(fin) , size_y = height(fin),interpolation_type = 1)
    
    
    
    blobLabelsOfInterest <- unique(netBlobOri)
    blobScore <- NULL
    for(labeledValue in blobLabelsOfInterest[-1])
    {
      blobScore <- append(blobScore,sum( netOut*(netBlobOri==labeledValue) ))
    }
    names(blobScore) <- blobLabelsOfInterest[-1]

    blobXposition <- NULL
    for(labeledValue in blobLabelsOfInterest[-1])
    {
      blobXposition <- append(blobXposition,abs(startEstX - mean(which(rowSums(netBlob==labeledValue)>0))))
    }
    blobXposition <- 1/(blobXposition)
    names(blobXposition) <- blobLabelsOfInterest[-1]
    
    targetBlob <- as.integer(names(which.max(blobScore*blobXposition)))
    
    
    constrainedHighlight <- edgeFilter*(netBlob==targetBlob)
    constrainedHighlight <- constrainedHighlight/max(constrainedHighlight)
    startHighlight <- as.array(constrainedHighlight * (sorbel*minimalEdge) )#dilate_square(sorbel*minimalEdge,3))
    
    thresh <- colSums(startHighlight)
    thresh <- mean(thresh[thresh>0])/3
    
    startY <- min(which(colSums(startHighlight) > thresh))
    startOptions <- startHighlight[,startY,,]
    startX <- which.max(startOptions)
    startPoint <- c(startX,startY)
    
    
    pathEdge <- as.matrix(startHighlight * ( (netBlob==targetBlob) & minimalEdge ) )
    edgeLim <- msum(colSums(pathEdge) ,yRange/25,1)
    edgeLim[is.na(edgeLim)] <- 0
    
    finMiddle <- ceiling(mean(which(edgeLim>0)))
    limitY <- finMiddle+min(which(edgeLim[finMiddle:length(edgeLim)]==0),na.rm = T)
    limitY <- min(limitY,height(fin),na.rm = T)
    
    endY <- max(which(colSums(pathEdge[,1:limitY])>thresh))
    endX <- round(mean(which.max(pathEdge[,endY])))
    endPoint <- c(round(endX),floor(endY))
    
    if(anyNA(startPoint) || anyNA(endPoint) || any(c(startPoint,endPoint)==0))
    {
      print(paste0("from: ",startPoint[1],",",startPoint[2]," -- to: ",endPoint[1],",",endPoint[2] ))
      return(list(NULL,NULL,NULL))
    }
    
    #plot(rawEdges);points(t(startPoint),col="green");points(t(endPoint),col="red")
    
    endProxRatio <- 10
  }else{
    print("using user provided start stops")
    print(startStopCoords)
    
    startPoint <- (startStopCoords[[1]]*resizeFactor)-c(resizeSpanX[1],resizeSpanY[1])*resizeFactor#(cumuResize*c(xSpan[1],ySpan[1])) #-round(cumuResize[1]*xSpan[1])
    endPoint <- (startStopCoords[[2]]*resizeFactor)-c(resizeSpanX[1],resizeSpanY[1])*resizeFactor#(cumuResize*c(xSpan[1],ySpan[1])) #-round(cumuResize[1]*xSpan)
    
    endProxRatio <- 10
  }
  
  
  ########################################################################################
  # execute trace
  ########################################################################################
  
  print("Creating Trailing Edge Path...")
  print(startPoint)
  pathMap <- minimalEdge*edgeFilter*sorbel
  
  # resizeSpanX <- netOutResizeFactors[1]*xSpan
  # resizeSpanY <- netOutResizeFactors[2]*ySpan
  
  #affineFactor <- netOutResizeFactors*c(xSpan[1],ySpan[1])
  affineFactor <- c(resizeSpanX[1],resizeSpanY[1])
  
  trailingFeatues <- filterFeatures(as.matrix(parmax(list(pathMap/max(pathMap),as.cimg(pathMap>(mean(pathMap[pathMap>0.0])/2.0) ))) ), #as.matrix(pathMap+pathMap>mean(pathMap[pathMap>0]) ),
                                    angleGrad,
                                    angleColor,
                                    round(startPoint),
                                    round(endPoint),
                                    endProxRatio,
                                    resizeFactor,
                                    affineFactor)
  print("Trace Complete")
  # plot(rawEdges);points(t(startPoint),col="green");points(t(endPoint),col="red")
  # trail <- cbind(
  #   round((trailingFeatues[[3]][,1]*(resizeFactor)) -restoreSizer[1]),
  #   round((trailingFeatues[[3]][,2]*(resizeFactor)) -restoreSizer[2])
  # )
  # points(trail,col="yellow",pch=".")
  #plot(grayscale(finOG));points(trailingFeatues[[3]],col="red",pch=".")
  
  return(list(trailingFeatues))
}
