
// [[Rcpp::depends(imager)]]
#include <imager.h>
#include <vector>
#include <algorithm>
#include <math.h> 


Rcpp::IntegerVector which(const Rcpp::NumericVector &x) {
  Rcpp::IntegerVector v = Rcpp::seq(0, x.size()-1);
  return v[x];
}
Rcpp::LogicalVector isIn(const Rcpp::IntegerVector &a, 
                         const Rcpp::IntegerVector &b){
  Rcpp::LogicalVector contained;
  for(NumericVector::iterator ait = a.begin(); ait != a.end(); ++ait)
  {
    for(NumericVector::iterator it = a.begin(); it != a.end(); ++it)
    {
      contained |= *it;
    }
  }
}

//' traceFromCannyEdges 
//' 
//' Isolates trailing edge of a fin given a canny edge image(cimg) and trace constraints.
//' Starting with a weighted canny edge matrix and start/stop points, an optimal path traversal is found via
//' \code{findPath}
//' After some basic cleanup, the resulting pixel coordinates for the optimal edge trace are returned.
//' @param pathMap matrix of type numeric. Used for weighted astar path finding
//' @param startPoint vector of type numeric indicating the x and y position for initializing trace
//' @param endPoint value of type numeric indicating the x and y position for terminating trace
//' @param prox value of type numeric for setting degree of wiggle room for trace termination
//' @return Value of type dataframe containing plotpath coordinates
// [[Rcpp::export]]
Rcpp::IntegerMatrix traceFromCannyEdges(const Rcpp::NumericVector pathMap, 
                                        const int startPoint [2], 
                                        const int endPoint [2],
                                        const int prox)
{
  const double radiusLimit = sqrt(pow(
    (startPoint[0]-endPoint[0])+
    (startPoint[1]-endPoint[1]),2));
  
    int xrange[2];
    int yrange[2];
    
    Rcpp::IntegerVector xValues = which(rowSums(pathMap)>.25);
    Rcpp::IntegerVector yValues = which(colSums(pathMap)>.25);
    
    xValues.push_back(xValues,startPoint[0]);
    xValues.push_back(xValues,endPoint[0]);
    yValues.push_back(yValues,startPoint[1]);
    yValues.push_back(yValues,endPoint[1]);

    xrange[0] = min(xValues);
    xrange[1] = max(xValues);
    yrange[0] = min(yValues);
    yrange[1] = max(yValues);
    
    Rcpp::IntegerVector path = findPath(pathMap,
                                        startPoint[0],
                                        startPoint[1],
                                        endPoint[0],
                                        endPoint[1],
                                        minX=max(xrange[0]-1,1),
                                        maxX=min(xrange[1]+1,nrow(pathMap)-1),
                                        minY=max(yrange[0]-1,1),
                                        maxY=min(yrange[1]+1,ncol(pathMap)-1),
                                        radiusLimit/prox)//proximity for completion
    //this section transforms the path vector found by the findPath function into coordinates for r to plot
    const Rcpp::IntegerVector stepX = IntegerVector::create( 0, 1, 1, 1, 0, -1, -1, -1)
    const Rcpp::IntegerVector stepY = IntegerVector::create(-1,-1, 0, 1, 1,  1,  0, -1)
      
    int pathLength = path.length();
    Rcpp::IntegerMatrix plotpath(0,pathLength,2);
    int Xposition = startPoint[0];
    int Yposition = startPoint[1];
    
    for(int i = 0; i<pathLength; i++)
    {
      plotpath[i,0] = Xposition;
      plotpath[i,0] = Yposition;
      Xposition = Xposition+stepX[path[i]];
      Yposition = Yposition+stepY[path[i]];
    }
    plotpath = plotpath[Rcpp::Range(pathLength-1,0),];
    
    //make sure we select tiptop
    
    Rcpp::IntegerVector y = plotpath( _ , 1);
    std::sort(y.begin(), y.end());
    tipValues = std::copy_n(plotpath,5,plotpath.begin());
      tipIndex = max(which(plotpath[,2] %in% tipValues ))
      plotpath = plotpath[tipIndex:nrow(plotpath),]
    
    //remove top sprew
    sprewX = abs(diff(plotpath[2:nrow(plotpath),2]))
    sprewX[is.na(sprewX)] = 0
    sprewY = abs(diff(plotpath[2:nrow(plotpath),1]))
    sprewY[is.na(sprewY)] = 0
    sprew = sprewX|sprewY
    startCut = min(which(msum(n=5,sprew)/5 > 0),na.rm = T)
      
      //remove bottom sprew
      sprew <- abs(diff(plotpath[seq_len(nrow(plotpath)-10),1]))
      sprew[is.na(sprew)] <- 0
      endCut <- max(which(msum(n=20,sprew)/20 > 0),na.rm = T)
        
      plotpath <- plotpath[startCut:endCut,]
      
      return plotpath;
}







