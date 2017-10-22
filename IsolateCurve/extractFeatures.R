
require("imager")
require("Rcpp")


###########################################################################################
# helper
#########################################################################################
msum <- function(x,n=5,sides=1){filter(x,rep(1,min(n,length(x))), sides=sides)}

###########################################################################################
# find the most efficient path and extract angles from it
#########################################################################################
filterFeatures <- function(pathMap,
                           angleMap,
                           start,
                           left,
                           right,
                           endLeft,
                           endRight)
{
  # orient and extract
  if (left>right)
  {
    print("endRight")
    path <- findPath(pathMap,
                     start[1],
                     start[2],
                     endRight[1],
                     endRight[2],
                     minX=1,#start[1]-ceiling(nrow(pathMap)/5),
                     maxX=nrow(pathMap)-2,
                     minY=1,
                     maxY=ncol(pathMap)-2,
                     max(nrow(pathMap),ncol(pathMap)),#radius
                     nrow(pathMap),#radiusMid X
                     0,#radiusMid Y
                     nrow(pathMap)/5)#proximity for completion
    angles <- extractAngles(angleMap,
                            path,
                            start[1],
                            start[2])
  }else{
    print("endLeft")
    path <- findPath(pathMap,
                     start[1],
                     start[2],
                     endLeft[1],
                     endLeft[2],
                     minX=1,
                     maxX=nrow(pathMap)-2,#start[1]+floor(nrow(pathMap)/5),
                     minY=1,
                     maxY=ncol(pathMap)-2,
                     max(nrow(pathMap),ncol(pathMap)),#radius
                     0,#radiusMid X
                     0,#radiusMid Y
                     nrow(pathMap)/5)#proximity for completion
    angles <- -1*extractAngles(angleMap,
                               path,
                               start[1],
                               start[2])
  }
  #if(length(angles)<150){stop("Poor trace")}
  if(length(angles)<150){return(NULL)}
  
  # this section transforms the path vector found by the findPath function into coordinates for r to plot
  # it serves no other purpose besides illustration
  stepX=c( 0, 1, 1, 1, 0, -1, -1, -1)
  stepY=c(-1,-1, 0, 1, 1,  1,  0, -1)
  
  pathLength <- length(path)
  plotpath <- matrix(0,nrow=pathLength,ncol=2)
  Xposition <- start[1]
  Yposition <- start[2]
  
  for(i in pathLength:1)
  {
    plotpath[i,1] <- Xposition
    plotpath[i,2] <- Yposition
    Xposition <- Xposition+stepX[path[i]+1]
    Yposition <- Yposition+stepY[path[i]+1]
  }
  plotpath <- plotpath[pathLength:1,]
  
  
  ##################################################################################
  # trim any noisy data off the ends
  ###################################################################################
  
  #make sure we select tiptop
  tipIndex <- which.min(plotpath[,2])[1]
  angles <- angles[tipIndex:length(angles)]
  plotpath <- plotpath[tipIndex:nrow(plotpath),]
  
  print("trimming")
  scanLength <- ceiling(length(angles)/15)
  
  #I want to ignore the first few below the lower thresh since that is where I want them 
  noiseMeasure <- append(abs(diff(angles)),0)
  
  expectedRangeMeasure <- rev(msum(  rev(angles) > 1.3 | rev(angles) <= -.8 ,scanLength))/scanLength
  expectedRangeIndex <- which(expectedRangeMeasure > .6)

  noiseMeasure <- noiseMeasure > spline((noiseStats$mean+2.5*noiseStats$sd),n=length(noiseMeasure))$y
  noiseMeasure <- rev(msum(rev(noiseMeasure),scanLength))/scanLength

  noiseIndex <- which(noiseMeasure>.2)#.5

  
  ##plot(angles)
  #plot(noiseMeasure)
  #lot(expectedRangeMeasure)
  #plot(cumsum(noiseMeasure));abline(h=mean(cumsum(noiseMeasure),na.rm = T)-sd(cumsum(noiseMeasure),na.rm = T)/3)
  #plot(cumsum(expectedRangeMeasure));abline(h=mean(cumsum(expectedRangeMeasure),na.rm = T)-sd(cumsum(expectedRangeMeasure),na.rm = T)/3)
  
  
  start0Lim <- max(min(which(noiseMeasure==0),na.rm = TRUE),
                   min(which(expectedRangeMeasure==0 & 
                               noiseMeasure==0),na.rm = TRUE),
                   1,na.rm = TRUE)
  
  if(is.infinite(start0Lim) || is.na(start0Lim)){start0Lim <- 1}
  
  #for each end lim we use the start0Lim as lower bound
  boundNoise <- msum(expectedRangeMeasure[-c(1:start0Lim)]==0 & noiseMeasure[-c(1:start0Lim)]==0 ,n = length(angles)/10, sides = 2)
  
  
  cumSumMin <- min(
    which(cumsum(noiseMeasure)<mean(cumsum(noiseMeasure),na.rm = T)-sd(cumsum(noiseMeasure),na.rm = T)/3),
    which(cumsum(expectedRangeMeasure)<mean(cumsum(expectedRangeMeasure),na.rm = T)-sd(cumsum(expectedRangeMeasure),na.rm = T)/3)
  )
  end0Lim <- start0Lim+max(cumSumMin,max(which(boundNoise==max(boundNoise,na.rm = TRUE))),na.rm = TRUE)
  
  if(is.infinite(end0Lim) || is.na(end0Lim)){end0Lim <- length(angles)}
  #plot(boundNoise==max(boundNoise,na.rm = TRUE))
  
  print(paste("starting length:",length(angles)))
  print(paste('start-stop limiters:',start0Lim,end0Lim))
  
  startCut <- max(1, 
                  noiseIndex[which(noiseIndex < start0Lim)],
                  expectedRangeIndex[which(expectedRangeIndex < start0Lim)] )
  
  endCut <- min(length(angles), 
                noiseIndex[which(noiseIndex > end0Lim)],
                expectedRangeIndex[which(expectedRangeIndex > end0Lim)] )
  print(paste('start-stop cuts:',startCut,endCut))
  #if(endCut-startCut<150){stop("Poor trace")}
  if(endCut-startCut<100){return(NULL)}
  
  
  print("smoothing trim")
  angles <- angles[startCut:endCut]
  plot(angles)
  
  angles[is.na(angles)] <- 0
  plotpath <- plotpath[startCut:endCut,]
  #plotpath <- plotpath[(startCut:endCut)-startCut+(pathLength-endCut),]
  
  return(list(angles,plotpath))
}


########################################################################################
# Isolate and extract features for recognition
########################################################################################

#fin: Value should be of type cimg. Load the image via load.image("directory/finImage.JPG")
#test <- load.image("L:/861-finFindR/AletaHohnCatalogs/Main Catalog_Beaufort NC/0041_26_Oct_06_AMG_sighting50027.JPG")
#curve <- isolateCurve(test);##plot(as.numeric(unlist(curve[[1]][1])))

#' Parameter initialization
#' fin The fin JPG
#' includeLeadingEdge Attempt to trace leading edge
#' @export
isolateCurve <- function(fin,
                         startStopCoords = NULL,
                         includeLeadingEdge = F)
{
  if(!is.cimg(fin)){stop("fin must be of type cimg")}
  if(width(fin) > 1800)
  {
    print("Image too large... Resizing...")
    fin <- resize(fin, size_x = 1800, size_y = round(1800*(height(fin)/width(fin))), interpolation_type = 5)
  }
  
  #remove glare
  highlightBlob <-label(threshold(fin,.95))#90
  glare <- threshold(fin,.99)
  keepers <- unique(highlightBlob*glare)
  highlightBlob[!(highlightBlob %in% keepers)] <- 0
  highlightBlob <- erode_square((highlightBlob==0),5)
  fin <- cleanEnhance(fin, highlightBlob, get.locations(highlightBlob,function(x){x==FALSE})-1 )
  ###plot(fin)
  
  ###############################################################################################
  # Determine if image has a strong silhouette(bimodal brightness)
  ###############################################################################################
  
  #for estimating bimodality
  silhouette = TRUE
  bimodalThreshold = .30#.20
  
  finMean <- mean(fin)
  upperFinSD <- sd(fin[fin>finMean])
  lowerFinSD <- sd(fin[fin<finMean])
  fin <- medianblur(fin,ceiling(width(fin)/100),sd(fin)/2)
  #plot(fin)
  
  #we count how many pixels fall within the range
  bimodFin <- sum(fin > finMean-lowerFinSD & fin < finMean+upperFinSD)/length(fin)
  
  # if brightness values are within the given range of the mean we extract an implied silhouette
  if (bimodFin > bimodalThreshold)
  {
    finExtract <- isoblur(fin,ceiling(width(fin)/1200))
    extractedSilhouette <- G(finExtract)-R(finExtract)-B(finExtract)/15
    
    extractedSilhouetteMean <- mean(extractedSilhouette)
    upperExtractedSilhouetteSD <- sd(extractedSilhouette[extractedSilhouette>extractedSilhouetteMean])
    lowerExtractedSilhouetteSD <- sd(extractedSilhouette[extractedSilhouette<extractedSilhouetteMean])
    
    # now we find out if the brightness distribution is bimodal
    bimodExtractedSilhouette <- sum(extractedSilhouette > extractedSilhouetteMean-lowerExtractedSilhouetteSD & 
                                      extractedSilhouette < extractedSilhouetteMean+upperExtractedSilhouetteSD)/length(extractedSilhouette)
    
    # after all this processing, if the extractedSilhouette distribution is strongly bimodal and the origional image is not, we use extractedSilhouette
    if (bimodExtractedSilhouette < bimodalThreshold && 
        bimodExtractedSilhouette < bimodFin)
    {
      silhouette = FALSE
    }
    extractedSilhouette <- medianblur(extractedSilhouette,ceiling(width(fin)/100),sd(extractedSilhouette)/2)
    ###plot(extractedSilhouette)
  }
  
  
  
  ################################################################################
  # generate the canny edge image
  ################################################################################
  #fin <- imsharpen(fin,1)
  ##plot(fin)
  
  dx <- imgradient(fin,"x")
  dx <- average(list(R(dx),G(dx),B(dx)))
  dx <- isoblur(dx,width(fin)/700)#700
  dx <- (dx-mean(dx))/sd(dx)
  
  dy <- imgradient(fin,"y")
  dy <- average(list(R(dy),G(dy),B(dy)))
  dy <- isoblur(dy,width(fin)/700)
  dy <- (dy-mean(dy))/sd(dy)
  
  if(silhouette == FALSE)
  {
    print("Extract Silhouette")
    ###plot(extractedSilhouette)
    extractedSilhouetteDX <- imgradient(extractedSilhouette,"x")
    extractedSilhouetteDX <- isoblur(extractedSilhouetteDX,width(fin)/400)#400
    extractedSilhouetteDX <- (extractedSilhouetteDX-mean(extractedSilhouetteDX))/sd(extractedSilhouetteDX)
    
    extractedSilhouetteDY <- imgradient(extractedSilhouette,"y")
    extractedSilhouetteDY <- isoblur(extractedSilhouetteDY,width(fin)/400)
    extractedSilhouetteDY <- (extractedSilhouetteDY-mean(extractedSilhouetteDY))/sd(extractedSilhouetteDY)
    
    dx <- average(list(dx,1.25*extractedSilhouetteDX))
    dy <- average(list(dy,1.25*extractedSilhouetteDY))
  }
  
  
  sorbel <- abs(dx)+abs(dy);
  angle <- atan(dy/dx)
  is.na(angle) <- 0
  
  
  rawEdges <- extractEdgeMap(sorbel,angle)
  strong <- rawEdges > quantile(rawEdges,.99)
  
  edgeBlobs <- label(fill( rawEdges>mean(rawEdges[rawEdges>0]), 2),high_connectivity = TRUE)
  keepers <- unique(edgeBlobs*strong)
  
  edgeBlobs[!(edgeBlobs %in% keepers)] <- 0
  
  #weight edges based on net evaluation
  # edgeEval <- as.cimg(predictLBPStack(imageArray,F))
  
  # connectFactor <- 3
  # edge <- ( fill(edgeBlobs>0,connectFactor) )*(((sorbel-min(sorbel))/(max(sorbel)-min(sorbel)))) #*resize(edgeEval,width(fin),height(fin),interpolation_type=5)^2
  
  edge <- (edgeBlobs>0)*(((sorbel-min(sorbel))/(max(sorbel)-min(sorbel)))) #*resize(edgeEval,width(fin),height(fin),interpolation_type=5)^2
  
  
  ########################################################################################
  # find a starting point
  ########################################################################################
  
  # the function traceStartSegments adds some random variation to x and finds points along the edge to determine orientation
  # this compares random trials and find a minimum that occures most 
  
  if(is.null(startStopCoords))
  {
    
    regionSpanThreshold <- ceiling(width(fin)/100)#.75
    strongCleanAngle <- clean( threshold(1-isoblur((angle^2),regionSpanThreshold/3))*threshold(dilate_square(isoblur(sorbel,regionSpanThreshold/2),regionSpanThreshold)),regionSpanThreshold*1.5)
    plot(strongCleanAngle)
    
    yTargetEsts <- colSums(strongCleanAngle,na.rm = TRUE)
    
    xOrientation <- rowSums(strongCleanAngle,na.rm = TRUE)
    # xOrientation <- msum(xOrientation,regionSpanThreshold,2)/regionSpanThreshold
    xOrientation <- msum(xOrientation,2,2)
    xOrientation[is.na(xOrientation)] <- 0
    startSmooth <- msum(xOrientation,ceiling(width(fin)/30),2)/ceiling(width(fin)/20)
    startSmooth[is.na(startSmooth)] <- mean(startSmooth,na.rm = TRUE)
    xOrientationSD <- sum(xOrientation>mean(xOrientation)+2*sd(xOrientation))
    
    
    startEstX <- which((xOrientation-startSmooth)==max((xOrientation-startSmooth),na.rm = TRUE))[1]
    
    
    
    
    plot(xOrientation);abline(h=mean(xOrientation)*.5);abline(v=startEstX)
    orientationScoreLeft <- sum( (xOrientation[1:max(2,startEstX-xOrientationSD)] > mean(xOrientation)*.5))
    orientationScoreRight <- sum( (xOrientation[min(startEstX+xOrientationSD,(width(fin)-1)):width(fin)] > mean(xOrientation)*.5))
    
    print(paste("orientation:",round(orientationScoreLeft,0) ,round(orientationScoreRight,0) ))
    
    edgeStartMap <- as.matrix(edge*((strongCleanAngle+2)))
    edgeStartMap <- edgeStartMap/max(edgeStartMap)
    #plot(as.cimg(edgeStartMap))
    
    starts <- matrix(0,ncol=2,nrow=5000)
    n <- 10
    strengthThreshold <- mean(edge[edge>0])
    
    i=1
    counter = 0
    while (i < (nrow(starts)-n) & counter<8000)
    {try({
      counter <- counter+1
      #print(counter)
      startOptions <- traceStartSegments(edgeStartMap, startEstX, 
                                         orientationScoreLeft,#check right
                                         orientationScoreRight,#check left 
                                         width(fin), strengthThreshold)
      
      startOptions <- startOptions[which(rowSums(startOptions) > 0),]#remove rows left with all 0
      
      if(!is.null(nrow(startOptions)))
      {
        if(nrow(startOptions)>1 && ncol(startOptions)==2)
        {
          #make sure we are not tracing a horisontal line through the water
          if(diff(range(startOptions[,1]))/diff(range(startOptions[,2])) < 2)
          {
            if(any(startOptions[,2]<10 | startOptions[,1]<10 | startOptions[,1]>width(fin)-10))
            {
              inflectionIndex <- tail(which(startOptions[-1,2]<startOptions[-nrow(startOptions),2]),1)
              if(length(inflectionIndex)>0)
              {
                startOptions <- startOptions[1:inflectionIndex+1,]
              }else{
                startOptions <- head(startOptions,1)
              }
            }
            
            if(!is.null(nrow(startOptions)))
            {
              minimas <- startOptions[which(startOptions[,2] %in% head(sort(unique(startOptions[,2])),n) ),]
              
              if(nrow(startOptions)>width(fin)/15)
              {
                # we only want to consider the point if it wasnt running off the edge
                minimas <- unique(minimas[which(minimas[,2]>5 & minimas[,1]>5 & minimas[,1]<width(fin)-4),])
                if(!is.null(nrow(minimas)))
                {
                  if(nrow(minimas)>0 & (i+nrow(minimas))<nrow(starts))
                  {
                    for(j in 1:nrow(minimas))
                    {
                      #in case we run out of table
                      starts[i+j,1] <- minimas[j,1]
                      starts[i+j,2] <- minimas[j,2]
                    }
                    i <- i+j
                  }
                }
              }
            }
          }
        }
      }
    })}
    
    # remove all rows with only 0
    starts <- starts[which(rowSums(starts) > 0),]
    #if(nrow(starts)<1){stop("Unable to find start")}
    if(nrow(starts)<1){return(NULL)}
    
    startTableY <- table(c(starts[,2], 1:height(fin) ))-1
    regionMinY <- msum(startTableY,regionSpanThreshold,2)#yspan should be smaller?
    regionMinY <- regionMinY*(rev((1:height(fin))^3))#weight for top preference
    #plot(regionMinY)
    regionMinY <- regionMinY/max(regionMinY,na.rm = TRUE)
    
    startTableX <- table(c(starts[,1], 1:width(fin) ))-1
    regionMinX <- msum(startTableX,regionSpanThreshold,2)
    
    startEstWeight <- msum(xOrientation,ceiling(width(fin)/75),2)
    #startEstWeight <- msum(xOrientation,regionSpanThreshold,2)
    #startEstWeight <- startEstWeight/norm(startEstWeight[!is.na(startEstWeight)],"2")
    # startEstWeight <- startEstWeight-min(startEstWeight,na.rm = TRUE)
    # startEstWeight[is.na(startEstWeight)] <- 0
    #plot(startEstWeight)
    
    regionMinX <- regionMinX*startEstWeight #startEstWeight
    regionMinX <- regionMinX/max(regionMinX,na.rm = TRUE)
    
    ##test <- as.cimg(regionMinX %*% t(regionMinY))
    ##plot(test)
    
    #score all starting candidates
    start <- starts[which.max(regionMinX[starts[,1]]+regionMinY[starts[,2]]),]
    
    
    ##plot(edge);points(starts,col="red");abline(h=start[2],v=start[1],col="red")
    print(paste0("starting point: x=",start[1],", y=",start[2]))
    
    
    ########################################################################################
    # Estimate end points
    ########################################################################################
    
    # here we set the target points to be about a tenth of the way outward from the lower corners
    # but we can set them by other criteria
    
    yEst <- which(yTargetEsts>(mean(yTargetEsts,na.rm = TRUE)*.75))
    yTarget <- floor(mean(yEst,na.rm = TRUE)+1*sd(yEst,na.rm = TRUE))
    #plot(yTargetEsts);abline(h=mean(yTargetEsts,na.rm = TRUE)*.9);abline(v=yTarget)
    #plot(yTargetEsts);abline(v=yTarget)
    
    #if(yTarget<height(fin)/3 || yTarget>height(fin)){yTarget <- floor(height(fin)*.9)}
    print(paste("endHeight:",yTarget))
    
    xEst <- which(regionMinX>(mean(regionMinX,na.rm = T)*.5))
    endLeft <- c(floor(mean(xEst,na.rm = T)-3*sd(xEst,na.rm = T)),yTarget)
    endRight <- c(ceiling(mean(xEst,na.rm = T)+3*sd(xEst,na.rm = T)),yTarget)
    print(paste("endLeft:",endLeft[1]," -- ","endRight:",endRight[1]))
  
    
  #if start points are provided, use these instead
  }else{
    start <- startStopCoord[[1]]
    trailingEnd <- startStopCoord[[2]]
    leadingEnd <- startStopCoord[[3]]
    
    if(leadingEnd>trailingEnd)
    {
      endLeft <- trailingEnd
      endRight <- leadingEnd
        
      orientationScoreLeft <- 0
      orientationScoreRight <- 1
    }else{
      endLeft <- leadingEnd
      endRight <- trailingEnd
        
      orientationScoreLeft <- 1
      orientationScoreRight <- 0
    }
  }
  
  
  ########################################################################################
  # execute trace
  ########################################################################################
  
  print("Creating Trailing Edge Path...")
  trailingFeatues <- filterFeatures(as.matrix(parmax(list(as.cimg(strong),edge))),
                                    angle,
                                    start,
                                    orientationScoreLeft,
                                    orientationScoreRight,
                                    endLeft,
                                    endRight)
  if(includeLeadingEdge)
  {
    print("Creating Leading Edge Path...")
    leadingFeatures <- filterFeatures(as.matrix(parmax(list(as.cimg(strong),edge))),
                                      angle,
                                      start, 
                                      orientationScoreLeft,
                                      orientationScoreRight,
                                      endRight, 
                                      endLeft)
    print("Trace Complete")
    return(list(trailingFeatues,leadingFeatues))
  }
  print("Trace Complete")
  return(list(trailingFeatues))
}


