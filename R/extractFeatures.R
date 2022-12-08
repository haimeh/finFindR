
mx.model.init.params_tst <- function (symbol, input.shape, fixed.shape, output.shape, initializer, ctx)
{

    if (!mxnet:::is.MXSymbol(symbol))
        stop("symbol needs to be MXSymbol")
    arg_lst <- list(symbol = symbol)
    arg_lst <- append(arg_lst, input.shape)
    arg_lst <- append(arg_lst, output.shape)
    arg_lst <- append(arg_lst, fixed.shape)

    slist <- do.call(mxnet:::mx.symbol.infer.shape, arg_lst)
    if (is.null(slist)) stop("Not enough information to get shapes")

    arg.params <- mxnet:::mx.init.create(initializer, slist$arg.shapes, ctx, skip.unknown = TRUE)
    #arg.params <- mx.init.create(initializer,  
    #                             slist$arg.shapes[-which(names(slist$arg.shapes) %in% names(fixed.shape))]
    #                             ,ctx, skip.unknown = TRUE)
       aux.params <- mxnet:::mx.init.create(initializer, slist$aux.shapes,ctx, skip.unknown = FALSE)
    return(list(arg.params = arg.params, aux.params = aux.params, slist))
}


mx.simple.bind_tst <- function(symbol, ctx, dtype ,grad.req = "null", fixed.param = NULL, slist, ...) {


  if (!mxnet:::is.MXSymbol(symbol)) stop("symbol need to be MXSymbol")
  #slist <- symbol$infer.shape(list(...))

  if (is.null(slist)) {
    stop("Need more shape information to decide the shapes of arguments")
  }
  print(slist$arg.shapes)
  if ( any(sapply(slist$arg.shapes,anyNA)) ) browser()

  arg.arrays <- sapply(slist$arg.shapes, function(shape) {
    mx.nd.array(array(0,shape), ctx)
  }, simplify = FALSE, USE.NAMES = TRUE)
  aux.arrays <- sapply(slist$aux.shapes, function(shape) {
    mx.nd.array(array(0,shape), ctx)
  }, simplify = FALSE, USE.NAMES = TRUE)
  grad.reqs <- lapply(names(slist$arg.shapes), function(nm) {
    if (nm %in% fixed.param) {
      print("found fixed.param")
      "null"
    } else if (!endsWith(nm, "label") && !endsWith(nm, "data")) {
      grad.req
    } else {
      "null"
    }
  })
  print("BOUND")
  return(mxnet:::mx.symbol.bind(symbol, ctx,
                 arg.arrays=arg.arrays,
                 aux.arrays=aux.arrays,
                 grad.reqs = grad.reqs))
}
predict.MXFeedForwardModel_tst <- function (model, X, ctx = NULL, array.batch.size = 128, array.layout = "auto",
    allow.extra.params = TRUE)
{
    if (is.serialized(model))
        model <- mxnet:::mx.unserialize(model)
    if (is.null(ctx))
        ctx <- mxnet:::mx.ctx.default()
    if (is.array(X) || is.matrix(X)) {
        if (array.layout == "auto") {
            array.layout <- mxnet:::mx.model.select.layout.predict(X,
                model)
        }
        if (array.layout == "rowmajor") {
            X <- t(X)
        }
    }
    X <-mxnet:::mx.model.init.iter(X, NULL, batch.size = array.batch.size,
        is.train = FALSE)
    X$reset()
    if (!X$iter.next())
        stop("Cannot predict on empty iterator")
    dlist = X$value()
    ## extract shape based on symbol name
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #namedShapes <- lapply(model$symbol$arguments,function(x){
	#			  as.integer(strsplit(gsub("\\(([^()]*)\\)|.", "\\1", x, perl=T),",")[[1]])
	#   }
    #)
    #names(namedShapes) <- model$symbol$arguments
    #fixed.shapes <- namedShapes[sapply(namedShapes,function(x)length(x)!=0)]
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	fixed.shapes <- append(sapply(model$arg.params,dim),sapply(model$aux.params,dim))

    input.names <- names(dlist)
    input.shape <- sapply(input.names, function(n){dim(dlist[[n]])}, simplify = FALSE)
    input.shape <- input.shape[1]
    #fixed.shapes <- append(namedShapes,lapply(fixed.param,dim))
    initialized <- mx.model.init.params_tst(symbol=model$symbol, 
				   input.shape=input.shape, 
				   fixed.shape=fixed.shapes, 
				   output.shape=NULL, 
				   initializer=mx.init.uniform(0), 
				   ctx=ctx)
    params <- initialized[1:2]
    slist <- initialized[[3]]
    arg_lst <- list(symbol = model$symbol, ctx = ctx, data = dim(dlist$data), grad.req = "null", slist=slist)

    pexec <- do.call(mx.simple.bind_tst, arg_lst)
    if (allow.extra.params) {
        model$arg.params[!names(model$arg.params) %in% arguments(model$symbol)] <- NULL
    }
	mxnet:::mx.exec.update.arg.arrays(pexec, model$arg.params, match.name = TRUE)
    mxnet:::mx.exec.update.aux.arrays(pexec, model$aux.params, match.name = TRUE)
    packer <- mxnet:::mx.nd.arraypacker()
    X$reset()
    while (X$iter.next()) {
        dlist = X$value()
        mxnet:::mx.exec.update.arg.arrays(pexec, list(data = dlist$data),
            match.name = TRUE)
        mxnet:::mx.exec.forward(pexec, is.train = FALSE)
        out.pred <- mxnet:::mx.nd.copyto(pexec$ref.outputs[[1]], mx.cpu())
        padded <- X$num.pad()
        oshape <- dim(out.pred)
        ndim <- length(oshape)
        packer$push(mxnet:::mx.nd.slice(out.pred, 0, oshape[[ndim]] - padded))
    }
    X$reset()
    return(packer$get())
}

#' @title msum 
#' @description helper function for smoothing a sequence
#' @param n width of kernel
#' @param sides should the moving average be calculated in middle, or starting from one side
#' @return smoothed sequence
msum <- function(x,n=5,sides=1){filter(x,rep(1,min(n,length(x))), sides=sides)}

#' @title locThresh
#' @description default function used in get.locations
#' @param image
#' @return binary img coordinates
locThresh <- function(x,a=.35){return(x>a)}

#' @title pClip
#' @description slopy version of clamp
#' @param x is value to clip
#' @param a is min
#' @param b is max
#' @return clipped x
pClip <- function(x, a, b){pmax(a, pmin(x, b) )}

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
			dev = sd(X)+1e-4
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
		
		
		if(length(path)<100){print("Path length FAILURE");return(list(annulus=NULL,coordinates=NULL,dim=NULL))}
		
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
		
		##remove top sprew
		preSegments <- diff(plotpath[,1])
		segments <- (cumsum(preSegments[-1]!=preSegments[-length(preSegments)])==0) & (preSegments==0)[-1]
		if(any(segments)){
			startCut <- max(which(segments))
		}else{
			startCut <- 5
		}
		
		
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
#' @param edgeChan int indicating fin component: 2=peduncle 3=trailing 5=leading
#' @return Value of type list containing:
#' "coordinates" a dataframe of coordinates
#' "annulus" a 3 channel cimg of isolated features
#' "dim" a vector denoting the x and y dims the coordinates are for
#' @export

traceFromImage <- function(fin,
							startStopCoords = NULL,
							pathNet = NULL,
							edgeChan = 3,
							justStartCoord = NULL,
							userNetOut = NULL)
{
	if(is.null(userNetOut)){require("mxnet")}
			# channel 1 : all
			# channel 2 : peduncle
			# channel 3 : trans
			# channel 4 : trailing
			# channel 5 : tip
			# channel 6 : leading
			#selectedChan <- c("Peduncle"=2,"Trailing"=4,"Leading"=6)
	if(!(edgeChan %in% c(2,4,6))){stop("Not a valid channel")}
	if(is.null(pathNet) & is.null(userNetOut))(pathNet <- mxnet::mx.model.load(file.path(system.file("extdata", package="finFindR"),'SWA_finTrace_fin'), 1000))
	if(!is.cimg(fin)){stop("fin must be Jpeg of type cimg")}
	if(!("MXFeedForwardModel" %in% class(pathNet)) & is.null(userNetOut)){stop("network must be of class MXFeedForwardModel")}
	
	if(max(fin)>10){fin <- fin/255}
	if(dim(fin)[4] == 1){
		finVal <- rep(fin,3)
		dim(finVal) <- c(dim(fin)[1:2],1,3)
		fin <- finVal 
		rm(finVal)
	}
	finOri <- fin
	

	## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	# --- generate the input for NN
	## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	netIn <- shrinkDomDim(fin,200)
	newDim <- dim(netIn)
	oriToNetResizeFactors <- c(dim(fin)[1]/newDim[1],dim(fin)[2]/newDim[2])
	netIn <- as.array(netIn)
 

	dim(netIn) <- c(newDim[1],newDim[2],3,1)

	if(is.null(userNetOut)){
		finImIter <- mx.io.arrayiter(netIn,
		#finImIter <- mx.io.arrayiter(floor(netIn*255),
										label=0,
										batch.size=1)
		#netOutRaw <- mxnet:::predict.MXFeedForwardModel(X=finImIter,model=pathNet,ctx=mxnet::mx.cpu(),array.layout = "colmajor")
		netOutRaw <- predict.MXFeedForwardModel_tst(X=finImIter,model=pathNet,ctx=mxnet::mx.cpu(),array.layout = "colmajor")

		# sometimes the images are poorly cropped and so we check if we want to process a sub image
		## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		finRange <- apply(get.locations(dilate_square(as.cimg(netOutRaw[,,1,]) < .75,5), as.logical)[,1:2],2,range)
		finSpan <- finRange[2,]-finRange[1,]
		# if we dont cover at least 60% of the image..
		if(any((dim(netOutRaw)[1:2] - finSpan) > (dim(netOutRaw)[1:2]*.4))){
			#netOutRawOri <- netOutRaw
			finInEnlargeRatio<- 200/max(finSpan)
			
			netIn <- shrinkDomDim(fin, round(200*finInEnlargeRatio) )#200
			newDim <- dim(netIn)
			oriToNetResizeFactors <- c(dim(fin)[1]/newDim[1],dim(fin)[2]/newDim[2])
			netIn <- as.array(netIn)
			dim(netIn) <- c(newDim[1],newDim[2],3,1)
			
			finRangeEnlarged <- round(finInEnlargeRatio*finRange)
			finRangeEnlarged[,"x"] <- pClip(finRangeEnlarged[,"x"],1,dim(netIn)[1])
			finRangeEnlarged[,"y"] <- pClip(finRangeEnlarged[,"y"],1,dim(netIn)[2])

			netInReduced <- netIn[finRangeEnlarged[1,"x"]:finRangeEnlarged[2,"x"], 
														finRangeEnlarged[1,"y"]:finRangeEnlarged[2,"y"],,,drop=F]

			# we want to increase the size of the netIn so that the sub image is the target of shrinkDomDim
			# then crop it and run it, and then buffer the netOut to be like the netIn
			finImIter <- mx.io.arrayiter(netInReduced,
											label=0,
											batch.size=1)
			netOutRawReduced <- mxnet:::predict.MXFeedForwardModel(X=finImIter,model=pathNet,ctx=mxnet::mx.cpu(),array.layout = "colmajor")
			netOutRaw <- as.array(resize(netOutRaw, interpolation_type=3, size_x=newDim[1], size_y=newDim[2]))
			#netOutRaw <- array(c(rep(1,prod(dim(netIn)[1:2])), rep(0,prod(dim(netIn)[1:2])*(dim(netOutRawReduced)[3]-1) )),
			#										c(dim(netIn)[1:2],dim(netOutRawReduced)[3],1))
			netOutRaw[finRangeEnlarged[1,"x"]:finRangeEnlarged[2,"x"], 
						finRangeEnlarged[1,"y"]:finRangeEnlarged[2,"y"],,] <- netOutRawReduced
		}
	}else{
		netOutRaw <- userNetOut
	}

	estHighlight <- threshold(netIn,.97)
	cropRot <- dilate_square((netIn==0.0),5) | dilate_square((netIn==1.0),3)
	if(any(cropRot)){
		netIn[as.logical(cropRot)]<-0
		netIn <- fillGlare(netIn, get.locations(cropRot,as.logical)-1)
	}
	netIn <- as.array(netIn)


	if(edgeChan==2){
		tipChan <- 3
	}else{
		tipChan <- 5
	}
	if(is.null(startStopCoords))
	{
		# channel 1 : all
		# channel 2 : peduncle
		# channel 3 : trans
		# channel 4 : trailing
		# channel 5 : tip
		# channel 6 : leading
		
		#if(trailing){
		#  edgeChan <- 2
		#  notEdgeChan <- 3
		#}else{
		#  edgeChan <- 3
		#  notEdgeChan <- 2
		#}
		#if(edgeChan==2){
		#	tipChan <- 3
		#}else{
		#	tipChan <- 4
		#}
		#notEdgeChan <- c("1"=NA, "2"=3, "3"=5, "4"=NA, "5"=3)[[edgeChan]]
		notEdgeChan <- c("1"=NA, "2"=4, "3"=NA, "4"=6, "5"=NA, "6"=4)[[edgeChan]]

		edgeDilateFactor <- max(ceiling(sum(netOutRaw[,,edgeChan,])/20),5)
		
		###########################################################################################
		# get fin edge directions
		#########################################################################################


		
		netFiltered <- netOutRaw
		netFiltered[,,1,] <- 1-netFiltered[,,1,]
		netFilteredThreshPre <- netFiltered > .35
		netFilteredThreshPre[,,edgeChan,] <- netFilteredThreshPre[,,edgeChan,] | netFilteredThreshPre[,,tipChan,]
		diffNotChan <- apply(get.locations(dilate_square(as.cimg(netFilteredThreshPre[,,notEdgeChan,]),edgeDilateFactor),as.logical)[,1:2],2,mean)
		diffChan <- apply(get.locations(dilate_square(as.cimg(netFilteredThreshPre[,,edgeChan,]),edgeDilateFactor),as.logical)[,1:2],2,mean)
		dilationFactor <- ceiling(sqrt(sum((diffChan-diffNotChan)^2)))
		netFocus <- dilate_square(netFilteredThreshPre[,,edgeChan,,drop=F], round(dilationFactor))
		netFocus[1,,,] <- 0
		netFocus[,1,,] <- 0
		netFocus[width(netFocus),,,] <- 0
		netFocus[,height(netFocus),,] <- 0
		
		if(!any(netFocus>0)){print("NO FIN EDGE FOUND");return(list(annulus=NULL,coordinates=NULL,dim=NULL,netOut=NULL))}
		
		netFocus <- label( netFocus ,high_connectivity = F)
		labelCounts <- table(netFocus)[-1]
		
		#plot(as.cimg(netFocus))
		#plot(as.cimg(netOutRaw[,,-1,]))
		
		netFocus[netFocus != which.max(labelCounts)] <- 0
		netFocus <- dilate_square(netFilteredThreshPre[,,edgeChan,,drop=F], edgeDilateFactor) * dilate_square(netFocus,3)

		
		if(!any(netFocus>0)){print("NO FIN EDGE FOUND");return(list(annulus=NULL,coordinates=NULL,dim=NULL,netOut=NULL))}
		
		##########################################################################################
		# crop fin to edge
		#########################################################################################

		if(!is.null(justStartCoord)){
			#justStartPointLarge <- justStartCoord
			justStartCoordSmall <- round(justStartCoord/oriToNetResizeFactors)
			justStartCoordSmall[1] <- pClip(justStartCoordSmall[1],1,dim(netIn)[1])
			justStartCoordSmall[2] <- pClip(justStartCoordSmall[2],1,dim(netIn)[2])
		}
		xSpan <- as.numeric(rowSums( round(netFocus) ))
		if(length(xSpan)==0 | all(xSpan==0) | all(xSpan>100) | any(is.infinite(xSpan)) | any(is.na(xSpan)) | any(is.nan(xSpan)))browser()
		if(!is.null(justStartCoord)){
			xSpan[max(1,justStartCoordSmall[1]-2):min(width(netFocus),justStartCoordSmall[1]+2)] <- 2
		}
		xSpan[is.na(xSpan)] <- 0
		xSpan <- range(which(xSpan>1))
		ySpan <- as.numeric(colSums( netFocus))
		if(!is.null(justStartCoord)){
			ySpan[max(1,justStartCoordSmall[2]-2):min(height(netFocus),justStartCoordSmall[2]+2)] <- 2
		}
		ySpan[is.na(ySpan)] <- 0
		ySpan <- range(which(ySpan>1))
		rm(netFocus)
		rm(netFilteredThreshPre)
		
		netFiltered <- netFiltered[c(xSpan[1]:xSpan[2]), c(ySpan[1]:ySpan[2]),,]
		netInFiltered <- netIn[c(xSpan[1]:xSpan[2]), c(ySpan[1]:ySpan[2]),,]

	}else{

		seBu <- sqrt(sum(startStopCoords[,1]-startStopCoords[,2])^2)/2
		xSpan <- range(startStopCoords[1,]) + c(-seBu,seBu)
		ySpan <- range(startStopCoords[2,]) + c(-seBu,seBu)

		xSpan <- pClip(xSpan,1,dim(fin)[1]) / oriToNetResizeFactors
		ySpan <- pClip(ySpan,1,dim(fin)[2]) / oriToNetResizeFactors


		netFiltered <- netOutRaw
		netFiltered[,,1,] <- 1-netFiltered[,,1,]
		netFiltered <- netFiltered[c(xSpan[1]:xSpan[2]), c(ySpan[1]:ySpan[2]),,]
		netInFiltered <- netIn[c(xSpan[1]:xSpan[2]), c(ySpan[1]:ySpan[2]),,]
	}
	

	###########################################################################################
	# resize trim color
	#########################################################################################

	resizeSpanX <- oriToNetResizeFactors[1]*xSpan
	resizeSpanY <- oriToNetResizeFactors[2]*ySpan
	resizeSpanX <- pClip(resizeSpanX,1,dim(fin)[1])
	resizeSpanY <- pClip(resizeSpanY,1,dim(fin)[2])
	
	finCropped <- suppressWarnings(as.cimg(fin[ ceiling(resizeSpanX[1]):floor(resizeSpanX[2]),
												ceiling(resizeSpanY[1]):floor(resizeSpanY[2]),,]))
	
	fin <- constrainSizeFinImage(finCropped,2000,750)
	resizeFactor <- mean((dim(finCropped)/dim(fin))[1:2])

	edgeFilter <- resize( as.cimg(netFiltered[,,1]) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)/max(netFiltered)
	
	#NOTE: This is a repeat
	lengthEdgeEst <- sum(netFiltered[,,1])/2.5

	cannyFilterSmall <- netFiltered[,,c(1,tipChan)]
	#cannyFilterSmall[,,2] <- netFiltered[,,1] - netFiltered[,,4]
	cannyFilterAll <- resize( as.cimg(cannyFilterSmall) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3)
	#cannyFilterAll <- isoblur(resize( as.cimg(cannyFilterSmall) ,size_x = width(fin) , size_y = height(fin),interpolation_type = 3),lengthEdgeEst/100)
	cannyFilter <- as.cimg(cannyFilterAll[,,2,])
	cannyFilterTip <- as.cimg(cannyFilterAll[,,1,])
	cannyFilterMax <- max(cannyFilter)
	if(max(cannyFilter)<.8){cannyFilter <- cannyFilter/cannyFilterMax}

	
	
	dilateFactor <- ceiling(lengthEdgeEst/200)
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
		#fin <- fillGlare(fin, get.locations(highlightBlob,function(x){x==FALSE})-1)
		fin <- fillGlare(fin, get.locations(highlightBlob,function(x){!as.logical(x)})-1)
		print("glare clear")
	}
	
	print("forground-background complete")

	###############################################################################################
	# Determine if image has a strong silhouette(bimodal brightness)
	###############################################################################################
	
	edgeIndex <- as.logical(dilate_square(as.cimg(netFiltered[,,1]),lengthEdgeEst)>.35)

	#dim(netInFiltered) <- dim(netInFiltered)[c(1,2,3)]
	netInFiltered <-suppressWarnings(as.cimg(netInFiltered))
	Gfin <- G(netInFiltered)[edgeIndex]
	Bfin <- B(netInFiltered)[edgeIndex]
	Rfin <- R(netInFiltered)[edgeIndex]


	diff1 <- Gfin-Rfin
	diff2 <- Bfin-Rfin
	diff3 <- Bfin-Gfin

	estimatedSilhouette <- as.numeric(apply(cbind(diff1,diff2,diff3), 1, max)) 
	#hist(estimatedSilhouette,1000)
	if(all(estimatedSilhouette==0)){silhouette=T}else{
			colTest = knn(as.numeric(estimatedSilhouette),2)
			lumTest = knn(as.numeric(netInFiltered[rep(edgeIndex,3) ]),2)
			#plot(as.numeric(estimatedSilhouette),col=colTest$cluster,pch='.')
			#silhouette = sum(colTest$density) < sum(lumTest$density)
			silhouette = (gausskld(colTest$params)+gausskld(colTest$params[c(2,1),])) < (gausskld(lumTest$params)+gausskld(lumTest$params[c(2,1),]))
	}
	print(paste("silhouette:",silhouette))
	

	################################################################################
	# generate the canny edge image
	################################################################################
	
	##average
	##parmax.abs
	flatten <- function(imgLst){return( parmax.abs(imgLst) )}
	blurFactor <- max(lengthEdgeEst/800,.5)


	#print("%%%%%%%% artifactRemover %%%%%%%%%%%")
	#print(range(isoblur(fin,1)-fin))
	finSoft <- fin+(10*(isoblur(fin,.5)-fin))
	finSoft <- isoblur(finSoft,blurFactor)
	gradis <- get_gradient(finSoft,"xy",2)
	#gradis <- get_gradient(fin,"xy",2)

	dx <- gradis[[1]]
	dy <- gradis[[2]]


	dx <- flatten(list(R(dx),G(dx),B(dx)))
	dx <- dx+2*(isoblur(dx,.6)-dx)
	dx <- isoblur(dx,blurFactor)

	dy <- flatten(list(R(dy),G(dy),B(dy)))
	dy <- dy+2*(isoblur(dy,.6)-dy)
	dy <- isoblur(dy,blurFactor)
	
	angleGrad <- atan(dy/dx)
	sorbel <- abs(dx)+abs(dy)
	sorbelOri <- sorbel
	qSorbel <- quantile(sorbel,.97)
	sorbel <- sorbel/qSorbel
	
	
		
	if(!silhouette)
	{
		extractedSilhouette <- cimg(array(0,dim(finSoft)))
		R(extractedSilhouette) <- G(finSoft)-R(finSoft)
		G(extractedSilhouette) <- B(finSoft)-R(finSoft)
		B(extractedSilhouette) <- B(finSoft)-G(finSoft)
		extractedSilhouette <- extractedSilhouette*4
		#extractedSilhouetteSoft <- extractedSilhouette+(5*(isoblur(extractedSilhouette,.6)-extractedSilhouette))
		#extractedSilhouetteSoft <- isoblur(extractedSilhouetteSoft,blurFactor)

		extrGradis <- get_gradient(extractedSilhouette,"xy",2)
		extractedSilhouetteDX <- extrGradis[[1]]
		extractedSilhouetteDX <- extractedSilhouetteDX+2*(isoblur(extractedSilhouetteDX,.65)-extractedSilhouetteDX)
		extractedSilhouetteDY <- extrGradis[[2]]
		extractedSilhouetteDY <- extractedSilhouetteDY+2*(isoblur(extractedSilhouetteDY,.65)-extractedSilhouetteDY)
		
		extractedSilhouetteDX <- isoblur(extractedSilhouetteDX,blurFactor+.1)#400
		extractedSilhouetteDX <- flatten(list(R(extractedSilhouetteDX),G(extractedSilhouetteDX),B(extractedSilhouetteDX)))
		
		extractedSilhouetteDY <- isoblur(extractedSilhouetteDY,blurFactor+.1)
		extractedSilhouetteDY <- flatten(list(R(extractedSilhouetteDY),G(extractedSilhouetteDY),B(extractedSilhouetteDY)))
		
		angleColor <- atan(extractedSilhouetteDY/extractedSilhouetteDX)
		#####################

		extractedSorbel <- abs(extractedSilhouetteDX)+abs(extractedSilhouetteDY)
		
		qExtractedSorbel <- quantile(extractedSorbel,.97)
		#qExtractedSorbel <- max(extractedSorbel)*.97
		#qSorbel <- max(sorbel)*.97
		
		dx <- average(list(extractedSilhouetteDX/qExtractedSorbel, dx/qSorbel))
		dy <- average(list(extractedSilhouetteDY/qExtractedSorbel, dy/qSorbel))
		
		sorbel <- average(list(sorbel, extractedSorbel/qExtractedSorbel))
	}


	sorbel <- (cannyFilterTip+sorbel)/2
	angle <- atan(dy/dx)

	rawEdges <- extractEdgeMap(sorbel,angle)
	rawEdges[1,,,] <- 0
	rawEdges[,1,,] <- 0
	rawEdges[width(rawEdges),,,] <- 0
	rawEdges[,height(rawEdges),,] <- 0
	
	#strong <- (rawEdges/max(rawEdges*(edgeFilter>.65))*(edgeFilter)) > .1# mean(netFilter)/20#+sd(netFilter)
	strong <- (rawEdges/(max(rawEdges)*.99)*(edgeFilter)) > .65# mean(netFilter)/20#+sd(netFilter)
	#strong <- strong | (rawEdges > quantile(rawEdges,.99))
	strong <- strong | (rawEdges > quantile(rawEdges,.982))
	
	edgeBlobs <- label( (rawEdges>0) ,high_connectivity = T)
	keepers <- unique(edgeBlobs * strong)
	
	edgeBlobs[!(edgeBlobs %in% keepers)] <- 0
	minimalEdge <- (edgeBlobs>0)
	
	
	
 
 
	 if(is.null(startStopCoords))
	 {
		edgeLoc <- as.matrix(get.locations(as.cimg(netFiltered[,,edgeChan]>.35), as.logical)[c(1,2)])
		edgeVal <- (netFiltered[,,edgeChan])[edgeLoc]
		edgeLimitSmall <- colSums(t(t(edgeLoc)*edgeVal))/sum(edgeVal)

		otherEdgeLoc = as.matrix(get.locations(as.cimg(netOutRaw[,,notEdgeChan,]>.35),as.logical)[c(1,2)])
		otherEdgeVal <- (netOutRaw[,,notEdgeChan,])[otherEdgeLoc]
		otherEdgeLimitSmall <- colSums(t(t(otherEdgeLoc)*otherEdgeVal))/sum(otherEdgeVal) - (c(xSpan[1],ySpan[1])-1)


		startRegionWithoutDilation <- as.cimg(netFiltered[,,tipChan] > .35)
		startRegion <- dilate_square(startRegionWithoutDilation, 5)
		candidateStarts <- get.locations(startRegion,as.logical)[c(1,2)]



			# channel 1 : all
			# channel 2 : peduncle
			# channel 3 : trans
			# channel 4 : trailing
			# channel 5 : tip
			# channel 6 : leading
		print("finding start stop")

		## START #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		# --- find start point
		## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		if(is.null(justStartCoord)){
			if(any(startRegion) & (edgeChan==3 | edgeChan==5) ){
				startRegion[1,,,] <- 0
				startRegion[,1,,] <- 0
				startRegion[width(netFiltered),,,] <- 0
				startRegion[,height(netFiltered),,] <- 0
				startBlobs <- label(startRegion)

				blobScore <- list()
				for(i in unique(as.integer(startBlobs))[-1]){
					blobScore[i] <- sum((netFiltered[,,tipChan])[startBlobs == i])
				}

				startBlobs[startBlobs!=which.max(as.numeric(blobScore))] <- 0

				startVals <- (netFiltered[,,edgeChan]+netFiltered[,,1])[data.matrix(candidateStarts)]
				startPointByVal <- as.integer(round(colSums(t(t(candidateStarts)*startVals))/sum(startVals)))


				#TODO: start Region farthest from edge limits 
				startPointByDist <- as.integer(candidateStarts[which.max(
								 sqrt(rowSums(t(t(candidateStarts)-otherEdgeLimitSmall)^2)) + 
								 sqrt(rowSums(t(t(candidateStarts)-edgeLimitSmall)^2))
								 ),])
				startPointSmall <- (startPointByVal+startPointByDist)/2

			}else{
				print("Nerual Net failed to find start, assuming at the top of edges")
				trail <- get.locations(as.cimg(netFiltered[,,edgeChan]>.35),as.logical)
				lead <- get.locations(as.cimg(netFiltered[,,notEdgeChan]>.35),as.logical)
				trailTarget <- trail[which(trail$y < (min(trail$y)+2)),]
				leadTarget <- lead[which(trail$y < (min(trail$y)+2)),]
				#if(trailing){
				if(edgeChan==3){
					candidateStarts <- rbind(trailTarget, leadTarget,leadTarget,leadTarget)
				}else{
					candidateStarts <- rbind(trailTarget,trailTarget,trailTarget,leadTarget)
				}
				startPointSmall <- as.integer(round(colMeans(candidateStarts,na.rm=T))[1:2])
			}
			startPointSmall <- pClip(startPointSmall, c(2,2), dim(netFiltered)[1:2])
			startPoint <- as.integer(round(((startPointSmall * ((dim(fin)[1:2]/dim(netFiltered)[1:2])) )))) #(dim(fin)/dim(finCropped))[1:2]))#* cumuResize))
		}else{
			print("using just user provided start")
		#browser()
			#justStartPointLarge <- justStartCoord
			#justStartCoord <- round(justStartCoord/oriToNetResizeFactors)
			#justStartCoord[1] <- pClip(justStartCoord[1],1,dim(netIn)[1])
			#justStartCoord[2] <- pClip(justStartCoord[2],1,dim(netIn)[2])

			#startPoint <- ((startStopCoords[[1]])-c(resizeSpanX[1],resizeSpanY[1]))/resizeFactor
			#startPoint <- justStartPointLarge
			#startPointSmall <- justStartCoord

			startPoint <- ((justStartCoord)-c(resizeSpanX[1],resizeSpanY[1]))/resizeFactor
			startPointSmall <- startPoint / ((dim(fin)[1:2]/dim(netFiltered)[1:2]))
		}





		## END #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		# --- find end point
		## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


		# TODO: get the points alligned right ugh
		candidateEndsSmall <- get.locations(as.cimg(netFiltered[,,edgeChan]>.35),as.logical)[c(1,2)]# * oriToNetResizeFactors 
		endPointSmall <- as.integer(candidateEndsSmall[which.max(
							 sqrt(rowSums(t(t(candidateEndsSmall)-otherEdgeLimitSmall)^2)) + 
							 sqrt(rowSums(t(t(candidateEndsSmall)-startPointSmall)^2)) - 
							 sqrt(rowSums(t(t(candidateEndsSmall)-edgeLimitSmall)^2))
							 ),])
		endPoint <- as.integer(round(((endPointSmall * (dim(fin)[1:2]/dim(netFiltered)[1:2]) )))) #(dim(fin)/dim(finCropped))[1:2]))#* cumuResize))
	 
		if(anyNA(startPoint) || anyNA(endPoint) || any(c(startPoint,endPoint)==0))
		{
			print(paste0("startPoint FAILURE; from: ",startPoint[1],",",startPoint[2],"		to: ",endPoint[1],",",endPoint[2] ))
			return(list(annulus=NULL,coordinates=NULL,dim=NULL,netOut=NULL))
		}
		print(cbind(startPoint,endPoint))
		
	}else{

		print("using user provided start stops")
		print(startStopCoords)
		
		startPoint <- ((startStopCoords[[1]])-c(resizeSpanX[1],resizeSpanY[1]))/resizeFactor
		endPoint <- ((startStopCoords[[2]])-c(resizeSpanX[1],resizeSpanY[1]))/resizeFactor
	}
		
	## DONE #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	# --- cleanup and such
	## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	startPoint <- pClip(startPoint, c(2,2), dim(sorbel)[1:2]-2)
	endPoint <- pClip(endPoint, c(2,2), dim(sorbel)[1:2]-2)
	endProxRatio <- 10



	
	
	########################################################################################
	# execute trace
	########################################################################################
	
	print("Creating Trailing Edge Path...")
	print(startPoint)
	pathMap <- minimalEdge*sorbel

	#NOTE: the resized junk path applies only to fin, first move then cumu resize?
	pathDF <- traceFromCannyEdges(as.matrix(pathMap), 
								round(startPoint),
								round(endPoint),
								endProxRatio)
	#meh = try(pathDF[,1])
	#if(class(meh)=="try-error")browser()
	
	annulus <- extractAnnulus(fin,pathDF[,1],pathDF[,2])
	#annulus <- NULL
	plotpath <- cbind(round(pathDF[,1]*resizeFactor+resizeSpanX[1] ),
						round(pathDF[,2]*resizeFactor+resizeSpanY[1]))
	
	 #plot(pathMap)
	 #par(new=TRUE)
	 #points(pathDF[,1],pathDF[,2],pch=".",col='red', ann=FALSE, asp = 0)
	
	traceData <- list(annulus,plotpath,dim(pathMap)[1:2],netOutRaw)
	names(traceData) <- c("annulus","coordinates","dim","netOut")
	return(traceData)
}

