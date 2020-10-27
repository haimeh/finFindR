
#' @title msum 
#' @description helper function for smoothing a sequence
#' @param n width of kernel
#' @param sides should the moving average be calculated in middle, or starting from one side
#' @return smoothed sequence
msum <- function(x,n=5,sides=1){filter(x,rep(1,min(n,length(x))), sides=sides)}

#' @title constrainSizeFinImage 
#' @details Processes an image(cimg) containing a fin. 
#' constrains image to a size range that balances preserving detail and efficiency
#' @param fin Value of type cimg. Load the image via load.image("directory/finImage.JPG")
#' @return Value of type list containing a scalar resize factor and a resized cimg
#' @export
constrainSizeFinImage <- function(fin, maxDim, minDim)
{
  shrinkFactors <- c(w2h=height(fin)/width(fin),h2w=width(fin)/height(fin) )

  domDim <- which.max(dim(fin))

  newDim <- dim(fin)[1:2]
  if(dim(fin)[domDim] > maxDim)
  {
    print("Image too large... Resizing...")
    if(domDim == 1){
      newDim[1] <- maxDim
      newDim[2] <- round(maxDim * shrinkFactors[1])
    }else{
      newDim[2] <- maxDim
      newDim[1] <- round(maxDim * shrinkFactors[2])
    }
  }
  else if(dim(fin)[domDim] < minDim)
  {
    print("Image too small... Resizing...")
    if(domDim == 1){
      newDim[1] <- minDim
      newDim[2] <- round(minDim * shrinkFactors[1])
    }else{
      newDim[2] <- minDim
      newDim[1] <- round(minDim * shrinkFactors[2])
    }
  }else{
    return(fin)
  }
    
  return( resize(im=fin,interpolation_type=3,size_x=newDim[1],size_y=newDim[2]) )
}



#' @title shrinkDomDim 
#' @details shrink largest dim to maxDim in ratio preserving way
#' @param image image to resize
#' @param maxDim maximum dim
#' @return resizeed image
shrinkDomDim <- function(image, maxDim){
  shrinkFactors <- c(w2h=height(image)/width(image),h2w=width(image)/height(image) )

  domDim <- which.max(dim(image))
  newDim <- c(0,0)
  if(domDim == 1){
    newDim[1] <- maxDim#dim(fin)[0] * shrinkFactors[1]
    newDim[2] <- round(maxDim * shrinkFactors[1])
  }else{
    newDim[2] <- maxDim#dim(fin)[0] * shrinkFactors[1]
    newDim[1] <- round(maxDim * shrinkFactors[2])
  }

  netIn <- resize(im=image,interpolation_type=5,size_x=newDim[1],size_y=newDim[2])
  return(netIn)
}


#' @title knn 
#' @description rough fit of gaussians via knn
#' @param X vector to fit
#' @param k number of distributions
#' @return list of distribution paramiters, assignments and density
knn <- function(X, k){
  Delta <- 1
  iter <- 0
  n = length(X)
  while(Delta > 1e-4 && iter <= 20){
    # initiation
    if(iter == 0){
      mu = mean(X)
      dev = sd(X)
      centroid <- seq(from=mu-dev,to=mu+dev,length.out=k)
      deviation <- rep(dev,k)
      centroid_mem <- centroid
      deviation_mem <- deviation
    }
    # equivalent to E-step
    d <- sapply(1:k, function(c) sapply(1:n, 
      function(i) sum(dnorm(X[i], centroid[c], deviation[c])) ))
    cluster <- apply(d, 1, which.max)
    
    # equivalent to M-step
    centroid <- t(sapply(1:k, function(c) {mean(X[cluster == c])}))
    deviation <- t(sapply(1:k, function(c) {sd(X[cluster == c])}))

    Delta <- sum((centroid - centroid_mem)^2 + (deviation-deviation_mem)^2)
    iter <- iter + 1
    centroid_mem <- centroid
    deviation_mem <- deviation
  }

  density <- as.numeric(apply(d, 1, max))
  return(list(params = data.frame(mu=t(centroid),sd=t(deviation)), cluster = cluster, density=density))
}


#' @title gausskld 
#' @description kl divergence between 2 gaussians
#' @param P dataframe (2 rows only) containing gaussian params. Colnames should be mu and sd
#' @return kl divergence
gausskld <- function(P){
  log(P$sd[2]/P$sd[1]) + ( P$sd[1]^2 + (P$mu[1] - P$mu[2])^2 )/(2*(P$sd[2]^2)) - 1/2
}


###########################################################################################
# find the best edge trace
#########################################################################################

#' @title traceFromCannyEdges 
#' @details Isolates trailing edge of a fin given a canny edge image(cimg) and trace constraints.
#' Starting with a weighted canny edge matrix and start/stop points, an optimal path traversal is found via
#' \code{findPath}
#' After some basic cleanup, the resulting pixel coordinates for the optimal edge trace are returned.
#' @param pathMap matrix of type numeric. Used for weighted astar path finding
#' @param startPoint vector of type numeric indicating the x and y position for initializing trace
#' @param endPoint value of type numeric indicating the x and y position for terminating trace
#' @param prox value of type numeric for setting degree of wiggle room for trace termination
#' @return Value of type dataframe containing plotpath coordinates
traceFromCannyEdges <- function(pathMap,
                                startPoint,
                                endPoint,
                                prox)
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
    
    
    if(length(path)<100){Warning("Path length FAILURE");return(list(NULL,NULL))}
    
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
    
    #make sure we select tiptop
    #tipValues <- head(sort(plotpath[,2],decreasing = F),5)
    #tipIndex <- max(which(plotpath[,2] %in% tipValues ))
    #plotpath <- plotpath[tipIndex:nrow(plotpath),]
    
    ##remove top sprew
    startCut <- 5
    #sprewX <- abs(diff(plotpath[2:nrow(plotpath),2]))
    #sprewX[is.na(sprewX)] <- 0
    #sprewY <- abs(diff(plotpath[2:nrow(plotpath),1]))
    #sprewY[is.na(sprewY)] <- 0
    #sprew <- sprewX|sprewY
    #startCut <- min(which(msum(n=5,sprew)/5 > 0),na.rm = T)
    
    ##remove bottom sprew
    sprew <- abs(diff(plotpath[seq_len(nrow(plotpath)-10),1]))
    sprew[is.na(sprew)] <- 0
    endCut <- max(which(msum(n=20,sprew)/20 > 0),na.rm = T)
    
    plotpath <- plotpath[startCut:endCut,]
    
    return(plotpath)
  }else{
    return(NULL)
  }
}


########################################################################################
# Isolate and extract features for recognition
########################################################################################

#' @title traceFromImage 
#' @details Processes an image(cimg) containing a fin. 
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
#' "annulus" a 3 channel cimg of isolated features
#' @export
traceFromImage <- function(fin,
                           startStopCoords = NULL,
                           pathNet = NULL,
			   trailing = T)
{
  require("mxnet")
  if(is.null(pathNet))(pathNet <- mxnet::mx.model.load(file.path(system.file("extdata", package="finFindR"),'SWA_finTrace_fin'), 1000))
  if(!is.cimg(fin)){stop("fin must be Jpeg of type cimg")}
  if(!("MXFeedForwardModel" %in% class(pathNet))){stop("network must be of class MXFeedForwardModel")}
  
  if(max(fin)>1){fin <- fin/255}
  finOri <- fin
  
  #### --- Highlight Trailing Edge --- ####
  netIn <- shrinkDomDim(fin,200)
  newDim <- dim(netIn)
  netOutResizeFactors <- c(dim(fin)[1]/newDim[1],dim(fin)[2]/newDim[2])

  estHighlight <- threshold(netIn,.97)
  
  cropRot <- dilate_square((netIn==0.0),5) | dilate_square((netIn==1.0),3)
  if(any(cropRot))
  {
    netIn[as.logical(cropRot)]<-0
    netIn <- fillGlare(netIn, get.locations(cropRot,function(x){x==TRUE})-1)
  }
  
  netIn <- as.array(netIn)
 

  # non mirrored input
  dim(netIn) <- c(newDim[1],newDim[2],3,1)
  finImIter <- mx.io.arrayiter(netIn,
                  label=0,
                  batch.size=1)
  netOutRaw <- mxnet:::predict.MXFeedForwardModel(X=finImIter,model=pathNet,ctx=mxnet::mx.cpu(),array.layout = "colmajor")
  
  #netOutFlat <- parmax(list(as.cimg(netOutMirror[,,,1]),as.cimg(netOutMirror[newDim[1]:1,,,2])))

  # channel 1 : all
  # channel 2 : trailingEdge
  # channel 3 : leadingEdge

  if(trailing){
    edgeChan <- 2
    notEdgeChan <- 3
  }else{
    edgeChan <- 3
    notEdgeChan <- 2
  }



  ###########################################################################################
  # get fin edge directions
  #########################################################################################

  netFiltered <- netOutRaw
  netFiltered[,,1,] <- 1-netFiltered[,,1,] 
  diffNotChan <- apply(get.locations(dilate_square(as.cimg(netFiltered[,,notEdgeChan,]>.35),9),as.logical)[,1:2],2,sd)
  diffChan <- apply(get.locations(dilate_square(as.cimg(netFiltered[,,edgeChan,]>.35),9),as.logical)[,1:2],2,sd)
  dilationFactor <- ceiling(sqrt(sum((diffChan-diffNotChan)^2)))
  netFocus <- dilate_square(netFiltered[,,edgeChan,,drop=F]>.35, round(dilationFactor*1.5))
  netFocus[1,,,] <- 0
  netFocus[,1,,] <- 0
  netFocus[width(netFocus),,,] <- 0
  netFocus[,height(netFocus),,] <- 0

  netFocus <- label( netFocus ,high_connectivity = F)
  labelCounts <- table(netFocus)[-1]
  
  netFocus[netFocus != which.max(labelCounts)] <- 0
  netFocus <- erode_square(netFocus,dilateFactor)

  if(!any(netFocus>0)){warning("NO FIN FOUND");return(list(NULL,NULL))}
  
  ###########################################################################################
  # crop fin to edge
  #########################################################################################
  xSpan <- as.numeric(rowSums( round(netFocus) ))
  if(length(xSpan)==0 | all(xSpan==0) | all(xSpan>100) | any(is.infinite(xSpan)) | any(is.na(xSpan)) | any(is.nan(xSpan)))browser()
  xSpan[is.na(xSpan)] <- 0
  xSpan <- range(which(xSpan>1))
  ySpan <- as.numeric(colSums( netFocus))
  ySpan[is.na(ySpan)] <- 0
  ySpan <- range(which(ySpan>1))
  
  netFiltered <- netFiltered[c(xSpan[1]:xSpan[2]),c(ySpan[1]:ySpan[2]),,]
  

  ###########################################################################################
  # resize trim color
  #########################################################################################

  resizeSpanX <- netOutResizeFactors[1]*xSpan
  resizeSpanY <- netOutResizeFactors[2]*ySpan
  
  if(!is.null(startStopCoords))
  {
    resizeSpanX <- c(min(startStopCoords[[1]][1]-1,startStopCoords[[2]][1]-1,floor(resizeSpanX[1])),
                     max(startStopCoords[[1]][1]+1,startStopCoords[[2]][1]+1,ceiling(resizeSpanX[2])))
    resizeSpanY <- c(min(startStopCoords[[1]][2]-1,startStopCoords[[2]][2]-1,floor(resizeSpanY[1])),
                     max(startStopCoords[[1]][2]+1,startStopCoords[[2]][2]+1,ceiling(resizeSpanY[2])))
  }
  finCropped <- suppressWarnings(as.cimg(fin[ ceiling(resizeSpanX[1]):floor(resizeSpanX[2]) ,
                                       ceiling(resizeSpanY[1]):floor(resizeSpanY[2]),,]))
  
  fin <- constrainSizeFinImage(finCropped,2000,750)
  resizeFactor <- mean((dim(finCropped)/dim(fin))[1:2])

  #resizeFactor <- 2000/height(fin)
  #resizeFactor <- 750/height(fin)

  #resizeFactor <- resizedFin$resizeFactor
  #fin <- resizedFin$fin
  
  cumuResize <- (netOutResizeFactors*resizeFactor)
  
  edgeFilter <- resize( as.cimg(netFiltered[,,1]) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)/max(netFiltered)
  
  if(!any(netFiltered>.5))
  {
    yRange <- round(diff(range(which((colSums(netFiltered/max(netFiltered) >.5)>0))))*cumuResize[2] )
  }else{
    yRange <- round(diff(range(which((colSums(netFiltered>.5)>0))))*cumuResize[2] )
  }

  # here we create the canny weighting image to constrain what edges are preserved
  #finCut <- shrinkDomDim(fin,150)
  #dim(finCut) <- dim(finCut)[c(1,2,4,3)]
  #finCutIter <- mx.io.arrayiter(finCut,
  #                label=0,
  #                batch.size=1)
  #netOut <- mxnet:::predict.MXFeedForwardModel(X=finCutIter,model=pathNet,ctx=mxnet::mx.cpu(),array.layout = "colmajor")
  #NOTE: temporary solution
  #cannyFilter <- isoblur(resize( netOut-.25 ,size_x = width(fin) , size_y = height(fin),interpolation_type = 6),yRange/1000)/max(netOut)
  #cannyFilter <- resize( as.cimg(netOut) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)
  #cannyFilter <- resize( as.cimg((netFiltered[,,1]) - (netFiltered[,,4]) + (netFiltered[,,edgeChan]*netFiltered[,,4]) ) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)
  cannyFilterSmall <- netFiltered[,,c(1,4)]
  cannyFilterSmall[,,2] <- netFiltered[,,1] - netFiltered[,,4]
  cannyFilterAll <- resize( as.cimg(cannyFilterSmall) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)
  cannyFilterTip <- as.cimg(cannyFilterAll[,,1,])
  cannyFilter <- as.cimg(cannyFilterAll[,,2,])
  #cannyFilter <- resize( as.cimg((netFiltered[,,1]) - (netFiltered[,,4]) ) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)
  #cannyFilterTip <- resize( as.cimg(netFiltered[,,1]) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)
  cannyFilterMax <- max(cannyFilter)
  if(max(cannyFilter)<.9){cannyFilter <- cannyFilter/cannyFilterMax}

  
  
  dilateFactor <- ceiling(yRange/200)
  dilateFactor <- dilateFactor+ifelse(as.logical(dilateFactor%%2),0,1)
  
  
  glareBound <- (sum(!estHighlight)/prod(dim(estHighlight)))
  if(glareBound > .95)# && glareBound < 1)
  {
    print("removing glare")
    highlightBlob <- threshold(fin,.97)#90
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
  #fin <- erode_square(fin,dilateFactor )
  #fin <- dilate_square(fin,dilateFactor )
  
  #estImageMean <- mean(netIn)
  #estImageSplit <- netIn>estImageMean
  #blurFactor <- min(sd(netIn[estImageSplit]),sd(netIn[!estImageSplit]) )
  #
  fin <- isoblur(fin,ceiling(yRange/800))
  #fin <- medianblur(fin,ceiling(yRange/80),blurFactor)
  
  print("forground-background complete")

  ###############################################################################################
  # Determine if image has a strong silhouette(bimodal brightness)
  ###############################################################################################
  
  edgeIndex <- as.logical(resize(dilate_square(as.cimg(netFiltered[,,edgeChan]),7), size_x=width(netIn), size_y=height(netIn), interpolation_type=2))

  dim(netIn) <- dim(netIn)[c(1,2,4,3)]
  Gfin <- G(netIn)[edgeIndex]
  Bfin <- B(netIn)[edgeIndex]
  Rfin <- R(netIn)[edgeIndex]


  diff1 <- Gfin-Rfin
  diff2 <- Bfin-Rfin
  diff3 <- Bfin-Gfin

  estimatedSilhouette <- as.numeric(apply(cbind(diff1,diff2,diff3), 1, max)) 
  #hist(estimatedSilhouette,1000)

  colTest = knn(as.numeric(estimatedSilhouette),2)
  lumTest = knn(as.numeric(netIn[rep(edgeIndex,3) ]),2)
  #plot(as.numeric(estimatedSilhouette),col=colTest$cluster,pch='.')
  
  #silhouette = sum(colTest$density) < sum(lumTest$density)
  silhouette = (gausskld(colTest$params)+gausskld(colTest$params[c(2,1),])) < (gausskld(lumTest$params)+gausskld(lumTest$params[c(2,1),]))
  print(paste("silhouette:",silhouette))
  

  ################################################################################
  # generate the canny edge image
  ################################################################################
  
  ##average
  ##parmax.abs
  flatten <- function(imgLst){return( parmax.abs(imgLst) )}
  
  gradis <- get_gradient(fin,"xy",2)
  dx <- gradis[[1]]
  dy <- gradis[[2]]

  #dx <- imgradient(fin,"x")
  dx <- isoblur(dx,yRange/800)#700
  dx <- flatten(list(R(dx),G(dx),B(dx)))
  #dy <- imgradient(fin,"y")
  dy <- isoblur(dy,yRange/800)
  dy <- flatten(list(R(dy),G(dy),B(dy)))
  
  angleGrad <- atan(dy/dx)
  sorbel <- abs(dx)+abs(dy)
  
  
    
  if(!silhouette)
  {
    extractedSilhouette <- cimg(array(0,dim(fin)))
    R(extractedSilhouette) <- G(fin)-R(fin)
    G(extractedSilhouette) <- B(fin)-R(fin)
    B(extractedSilhouette) <- B(fin)-G(fin)

    extrGradis <- get_gradient(fin,"xy",2)
    extractedSilhouetteDX <- extrGradis[[1]]
    extractedSilhouetteDY <- extrGradis[[2]]
    
    #extractedSilhouetteDX <- imgradient(extractedSilhouette,"x")
    extractedSilhouetteDX <- isoblur(extractedSilhouetteDX,yRange/600)#400
    extractedSilhouetteDX <- flatten(list(R(extractedSilhouetteDX),G(extractedSilhouetteDX),B(extractedSilhouetteDX)))
    
    #extractedSilhouetteDY <- imgradient(extractedSilhouette,"y")
    extractedSilhouetteDY <- isoblur(extractedSilhouetteDY,yRange/600)
    extractedSilhouetteDY <- flatten(list(R(extractedSilhouetteDY),G(extractedSilhouetteDY),B(extractedSilhouetteDY)))
    
    angleColor <- atan(extractedSilhouetteDY/extractedSilhouetteDX)

    #####################

    extractedSorbel <- abs(extractedSilhouetteDX)+abs(extractedSilhouetteDY)
    
    qExtractedSorbel <- quantile(extractedSorbel,.97)
    qSorbel <- quantile(sorbel,.97)
    
    dx <- average(list(extractedSilhouetteDX/qExtractedSorbel, dx/qSorbel))
    dy <- average(list(extractedSilhouetteDY/qExtractedSorbel, dy/qSorbel))
    
    sorbel <- average(list(sorbel/qSorbel, extractedSorbel/qExtractedSorbel))
  }


  sorbelOri <- sorbel
  #sorbel <- (1/(1+exp(-10*as.cimg(cannyFilter[,,1,1]) )))*sorbel
  #sorbel <- (1/(1+exp(-10* cannyFilter )))*sorbel
  sorbel <- (cannyFilterTip * sorbel) + (.1*cannyFilter) + (.1*sorbel)
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
    print("finding start stop")

    #cannyFilterThresh <- (cannyFilter > .35)


    ## START ###########################
    # --- find start point
    netFiltered[1,,] <- 0
    netFiltered[,1,] <- 0
    netFiltered[width(netFiltered),,] <- 0
    netFiltered[,height(netFiltered),] <- 0
    netFilteredThresh <- netFiltered>.35

    startRegion <- dilate_square(as.cimg(netFilteredThresh[,,4]),9)
    startRegion[1,,,] <- 0
    startRegion[,1,,] <- 0
    startRegion[width(netFiltered),,,] <- 0
    startRegion[,height(netFiltered),,] <- 0

    startBlobs <- label(startRegion)
    if(any(startRegion)){
      blobScore <- list()
      for(i in unique(as.integer(startBlobs))[-1]){
      	blobScore[i] <- sum((netFiltered[,,4])[startBlobs == i])
      }

      startBlobs[startBlobs!=which.max(as.numeric(blobScore))] <- 0
      candidateStarts <- get.locations(startRegion,as.logical)[c(1,2)]

      startVals <- (netFiltered[,,edgeChan]+netFiltered[,,1])[data.matrix(candidateStarts)]
      startPointSmall <- as.integer(round(colSums(t(t(candidateStarts)*startVals))/sum(startVals)))
      #startPointSmall <- colSums(t(t(candidateStarts)*startVals))/sum(startVals)

    }else{
      print("Nerual Net failed to find start, assuming at the top of edges")
      trail <- get.locations(as.cimg(netFiltered[,,2]>.35),as.logical)
      lead <- get.locations(as.cimg(netFiltered[,,3]>.35),as.logical)
      trailTarget <- trail[which(trail$y < (min(trail$y)+2)),]
      leadTarget <- lead[which(trail$y < (min(trail$y)+2)),]
      if(trailing){
      	candidateStarts <- rbind(trailTarget, leadTarget,leadTarget,leadTarget)
      }else{
      	candidateStarts <- rbind(trailTarget,trailTarget,trailTarget,leadTarget)
      }
      startPointSmall <- as.integer(round(colMeans(candidateStarts,na.rm=T))[1:2])
      #startPointSmall <- colMeans(candidateStarts,na.rm=T)[1:2]
    }



    startMap <- array(0,dim(netFiltered)[1:2])
    startMap[startPointSmall[1],startPointSmall[2]] <- 1
    startMapFull <- resize( as.cimg(startMap) ,size_x = width(minimalEdge) , size_y = height(minimalEdge),interpolation_type = 3)
    targetPointVal = max(startMapFull)  
    startPoint <- as.integer(colMeans(get.locations(startMapFull,function(x)x>=targetPointVal)[1:2]))
    
    #startPoint <- as.integer(round(startPointSmall * netOutResizeFactors * (dim(fin)/dim(finOri))[1:2]))



    ## END #############################
    # --- find end point
    endBlobs <- resize( as.cimg(netFiltered[,,edgeChan]) ,size_x = width(minimalEdge) , size_y = height(minimalEdge),interpolation_type = 1)
    candidateEnds <- get.locations((endBlobs>.3)*minimalEdge,as.logical)[1:2]

    edgeLoc = as.matrix(get.locations(as.cimg(netFilteredThresh[,,edgeChan]),as.logical)[c(1,2)])
    edgeVal <- (netFiltered[,,edgeChan])[edgeLoc]
    edgeLimitSmall <- colSums(t(t(edgeLoc)*edgeVal))/sum(edgeVal)
    edgeLimit <- edgeLimitSmall * netOutResizeFactors * (dim(fin)/dim(finOri))[1:2]  #cumuResize#*rev(netOutResizeFactors)


    #otherEdgeLoc = as.matrix(get.locations(as.cimg(netFilteredThresh[,,notEdgeChan]),as.logical)[c(1,2)])
    #otherEdgeVal <- (netFiltered[,,notEdgeChan])[otherEdgeLoc]
    #otherEdgeLimitSmall <- colSums(t(t(otherEdgeLoc)*otherEdgeVal))/sum(otherEdgeVal)
    #otherEdgeLimit <- otherEdgeLimitSmall * netOutResizeFactors * (dim(fin)/dim(finOri))[1:2]#cumuResize#rev(netOutResizeFactors)

    otherEdgeLoc = as.matrix(get.locations(as.cimg(netOutRaw[,,notEdgeChan,]>.35),as.logical)[c(1,2)])
    otherEdgeVal <- (netOutRaw[,,notEdgeChan,])[otherEdgeLoc]
    otherEdgeLimitSmall <- colSums(t(t(otherEdgeLoc)*otherEdgeVal))/sum(otherEdgeVal) - c(xSpan[1],ySpan[2])
    otherEdgeLimit <- otherEdgeLimitSmall * netOutResizeFactors * (dim(fin)/dim(finOri))[1:2]


    endPoint <- as.integer(candidateEnds[which.max(  
						   1 * sqrt(rowSums(t(t(candidateEnds)-otherEdgeLimit)^2)) + 
						   1 * sqrt(rowSums(t(t(candidateEnds)-startPoint)^2))  - 
						   1 * sqrt(rowSums(t(t(candidateEnds)-edgeLimit)^2))
						   ),])

    startPoint <- pmax(pmin(startPoint,dim(sorbel)[1:2]-5),c(5,5))
    endPoint <- pmax(pmin(endPoint,dim(sorbel)[1:2]-5),c(5,5))
    
    if(anyNA(startPoint) || anyNA(endPoint) || any(c(startPoint,endPoint)==0))
    {
      Warning(paste0("startPoint FAILURE; from: ",startPoint[1],",",startPoint[2],"   to: ",endPoint[1],",",endPoint[2] ))
      return(list(NULL,NULL))
    }
    
    endProxRatio <- 10
    #endProxRatio <- 10
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
  # pathMap <- minimalEdge*edgeFilter*sorbel
  pathMap <- minimalEdge*(cannyFilterTip+(.2*sorbelOri))  #sorbel
  
  affineFactor <- c(resizeSpanX[1],resizeSpanY[1])
  
  # pathDF <- traceFromCannyEdges(as.matrix(parmax(list(pathMap/max(pathMap),as.cimg(pathMap>(mean(pathMap[pathMap>0.0])/2.0) ))) ), #as.matrix(pathMap+pathMap>mean(pathMap[pathMap>0]) ),
  #browser()
  #pathMap[pathMap>1] <- 1
  #pathDF <- traceFromCannyEdges(as.matrix((pathMap/(1.6*median(pathMap[pathMap>0])))), #as.matrix(pathMap+pathMap>mean(pathMap[pathMap>0]) ),
  pathDF <- traceFromCannyEdges(as.matrix(pathMap), 
                                round(startPoint),
                                round(endPoint),
                                endProxRatio)
  #meh = try(pathDF[,1])
  #if(class(meh)=="try-error")browser()
  
  annulus <- extractAnnulus(fin,pathDF[,1],pathDF[,2])
  #annulus <- NULL
  plotpath <- cbind(round(pathDF[,1]/resizeFactor+affineFactor[1] ),
                    round(pathDF[,2]/resizeFactor+affineFactor[2]))
  
  # pathMap <<- pathMap
  # plot(pathMap)
  # par(new=TRUE)
  # points(pathDF[,1],pathDF[,2],pch=".",col='red', ann=FALSE, asp = 0)
  
  traceData <- list(annulus,plotpath,dim(fin))
  names(traceData) <- c("annulus","coordinates","dim")
  return(traceData)
}

