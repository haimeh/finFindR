
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <math.h> 

//' msum 
//' 
//' helper function for smoothing a sequence
//' @param n width of kernel
//' @param sides should the moving average be calculated in middle, or starting from one side
//' @return smoothed sequence
Rcpp::NumericVector = msum(x,int n,sides=1)
{
  std::vector<double> window(n); 
  filter(x,rep(1,min(n,length(x))), sides=sides)
}


Rcpp::IntegerVector which(const Rcpp::NumericVector &x)
{
  Rcpp::IntegerVector v = Rcpp::seq(0, x.size()-1);
  return v[x];
}

Rcpp::LogicalVector isIn(const Rcpp::IntegerVector &a, 
                         const Rcpp::IntegerVector &b){
  Rcpp::LogicalVector contained(a.length());
  int i = 0;
  for(Rcpp::NumericVector::iterator ait = a.begin(); ait != a.end(); ++ait)
  {
    contained[i] = std::find(b.begin(),b.end(),*ait);
    i++;
  }
  return contained;
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
Rcpp::IntegerMatrix traceFromCannyEdges(const Rcpp::NumericVector &pathMap, 
                                        const int startPoint [2], 
                                        const int endPoint [2],
                                        const int &prox)
{
  const double radiusLimit = sqrt(pow(
  (startPoint[0]-endPoint[0])+
  (startPoint[1]-endPoint[1]),2));

  int xseq[2];
  int yseq[2];
  
  Rcpp::IntegerVector xValues = which(Rcpp::rowSums(pathMap)>.25);
  Rcpp::IntegerVector yValues = which(Rcpp::colSums(pathMap)>.25);
  
  xValues.push_back(xValues,startPoint[0]);
  xValues.push_back(xValues,endPoint[0]);
  yValues.push_back(yValues,startPoint[1]);
  yValues.push_back(yValues,endPoint[1]);

  xseq[0] = Rcpp::min(xValues);
  xseq[1] = Rcpp::max(xValues);
  yseq[0] = Rcpp::min(yValues);
  yseq[1] = Rcpp::max(yValues);
  
  Rcpp::IntegerVector path = findPath(pathMap,
                                      startPoint[0],
                                      startPoint[1],
                                      endPoint[0],
                                      endPoint[1],
                                      minX=max(xseq[0]-1,1),
                                      maxX=min(xseq[1]+1,nrow(pathMap)-1),
                                      minY=max(yseq[0]-1,1),
                                      maxY=min(yseq[1]+1,ncol(pathMap)-1),
                                      radiusLimit/prox)//proximity for completion
  //this section transforms the path vector found by the findPath function into coordinates for r to plot
  const Rcpp::IntegerVector stepX = Rcpp::IntegerVector::create( 0, 1, 1, 1, 0, -1, -1, -1)
  const Rcpp::IntegerVector stepY = Rcpp::IntegerVector::create(-1,-1, 0, 1, 1,  1,  0, -1)
    
  int pathLength = path.length();
  Rcpp::IntegerMatrix plotpath(0,pathLength,2);
  int Xposition = startPoint[0];
  int Yposition = startPoint[1];
  
  for(int i = 0; i<pathLength; i++)
  {
    plotpath(i,0) = Xposition;
    plotpath(i,1) = Yposition;
    Xposition = Xposition+stepX[path[i]];
    Yposition = Yposition+stepY[path[i]];
  }
  plotpath = plotpath(Rcpp::seq(pathLength-1,0),_);
  
  //make sure we select tiptop
  Rcpp::IntegerVector y = plotpath( _ , 1);
  std::sort(y.begin(), y.end());
  tipValues = std::copy_n(plotpath,5,plotpath.begin());
  
  tipIndex = Rcpp::max(which(Rcpp::is_na(plotpath(_,2),tipValues) ));
  plotpath = plotpath(Rcpp::seq(tipIndex,nrow(plotpath)),_);
  
  //remove top sprew
  Rcpp::IntegerVector sprewX = Rcpp::abs(Rcpp::diff(plotpath(Rcpp::seq(1,nrow(plotpath)),1) ));
  Rcpp::IntegerVector sprewY = Rcpp::abs(Rcpp::diff(plotpath(Rcpp::seq(1,nrow(plotpath)),0) ));

  //we want to remove the zeros
  Rcpp::LogicalVector sprew = ifelse(Rcpp::is_na(sprewX) | (sprewX>0),0,1) | 
    ifelse(Rcpp::is_na(sprewY) | (sprewY>0),0,1);
  startCut = Rcpp::min(which(msum(n=5,sprew)/5 > 0),na.rm = T);
  
  //remove bottom sprew
  sprew = Rcpp::abs(Rcpp::diff(plotpath(Rcpp::seq_len(nrow(plotpath)-10),1)) );
  sprew = Rcpp::ifelse(Rcpp::is_na(sprew),0,sprew);
  
  endCut = Rcpp::max(which(msum(n=20,sprew)/20 > 0),na.rm = T);
    
  plotpath = plotpath(Rcpp::seq(startCut,endCut),_);
  
  return plotpath;
}







