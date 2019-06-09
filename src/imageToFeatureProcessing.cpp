#define cimg_display 0
// [[Rcpp::depends(imager)]]
#include <imager.h>
#include <vector>
#include <algorithm>


//' fillGlare
//' 
//' This function returns a cimg wrapped as NumeriMatrix.
//' Using a smoothing kernel, highlights are filled in based on the neighboring values.
//' Iteratively fills in from outer edge inward.
//' Image should have glare cliped to 0 already
//'
//' @param imageFromR cimg wraped as NumericVector
//' @param highlightCoordinates Coordinates to fill in
//' @param aveThresh Weighted sum, representing minimum number of neighboring non 0 pixels
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector fillGlare(const Rcpp::NumericVector imageFromR, 
                              const Rcpp::DataFrame highlightCoordinates, 
                              const int aveThresh=7)
{
  const int gaussianMask[7][7] =
  {
    0, 1, 1, 2, 1, 1, 0,
    1, 1, 2, 2, 2, 1, 1,
    1, 2, 2, 3, 2, 2, 1,
    2, 2, 3, 3, 3, 2, 2,
    1, 2, 2, 3, 2, 2, 1,
    1, 1, 2, 2, 2, 1, 1,
    0, 1, 1, 2, 1, 1, 0,
  };
  
  cimg_library::CImg<double> image = Rcpp::as< cimg_library::CImg<double> >(imageFromR);
  
  double gausSum = 0.0;
  double regionSample = 0.0;
  
  cimg_library::CImg<double> newFill(image.width(),image.height(),1,image.spectrum(),0);
  
  Rcpp::NumericVector glareX = highlightCoordinates["x"];
  Rcpp::NumericVector glareY = highlightCoordinates["y"];
  Rcpp::NumericVector glareC = highlightCoordinates["cc"];
  
  Rcpp::NumericVector::iterator xit = glareX.begin();
  Rcpp::NumericVector::iterator yit = glareY.begin();
  Rcpp::NumericVector::iterator cit = glareC.begin();
  
  bool changed = true;
  while (changed)
  {
    changed = false;
    xit = glareX.begin();
    yit = glareY.begin();
    cit = glareC.begin();
    while (xit != glareX.end() &&
           yit != glareY.end() &&
           cit != glareC.end())
    {
      if (image(*xit,*yit,0,*cit) == 0)
      {
        //sample values from region to sellect fill choice
        for (int i=-3; i != 4; i++)
        {
          for (int j =-3; j != 4; j++)
          {
            if(*xit+i < image.width() &&
               *xit+i >= 0 &&
               *yit+j < image.height() &&
               *yit+j >= 0)
            {
              regionSample += image(*xit+i,*yit+j,0,*cit)*gaussianMask[3+i][3+j];
              gausSum += gaussianMask[3+i][3+j]*(image(*xit+i,*yit+j,0,*cit)>0.0);
            }
          }
        }
        
        //get average of available surrounding values
        if(gausSum>aveThresh)
        {
          newFill(*xit,*yit,0,*cit) = regionSample/gausSum; 
          changed=true;
        }
        gausSum = 0;
        regionSample = 0.0;
      }
      xit++;
      yit++;
      cit++;
    }
    //fill in glare region
    if(changed)
    {
      image += newFill;
      newFill*=0;
    }
  }
  
  return Rcpp::wrap(image);
}



//' simplifyAngles
//' 
//' This function returns a cimg of type int.
//' Descritizes vectors represented by their angle into one of 4 sectors.
//' Best visualized has the right half of the unit circle devided in 4
//'
//' @param ang cimg reference of type double
cimg_library::CImg<int> simplifyAngles(const cimg_library::CImg<double> ang)
{
  cimg_library::CImg<int> simplifiedAngles(ang.width(),ang.height(),1,1,0);
  int approxAng = 0;
  const double pi = 3.1415926535897932;
  cimg_forXY(ang,x,y)
  {
         if (ang(x,y) <= ( 4*pi/8) && ang(x,y) > ( 2*pi/8)){approxAng = 1;}
    else if (ang(x,y) <= ( 2*pi/8) && ang(x,y) > ( 0.0   )){approxAng = 2;}
    else if (ang(x,y) <= ( 0.0   ) && ang(x,y) > (-2*pi/8)){approxAng = 3;}
    else {approxAng = 0;}
    
    simplifiedAngles(x,y) = approxAng;
  }
  return simplifiedAngles;
}

//' nonMaxSuppress
//' 
//' This function returns a cimg of type double.
//' Verifies the allignment between
//' The apex of the gradient magnitude of x and y in the 3x3 region around each pixel,
//' with the angle range estimated from the \code{simplifyAngles} function.
//' Allignment is soft, in that overlap is allowed.
//' 
//' @param edge cimg reference of type double
//' @param ang cimg reference of type int
cimg_library::CImg<double> nonMaxSuppress(const cimg_library::CImg<double>& edge, 
                    const cimg_library::CImg<int>& ang)
{
  cimg_library::CImg<double> newedge(edge.width(),edge.height() ,1,1,0);
  double targetPixel = 0.0;
  
  cimg_for_insideXY(edge,col,row,1)
  {
    targetPixel = edge(col,row);
    switch(ang(col,row))
    {
    case 1:
      if(((edge(col  ,row-1) < targetPixel) &&
          (edge(col  ,row+1) < targetPixel)) ||
         ((edge(col+1,row+1) < targetPixel) &&
          (edge(col-1,row-1) < targetPixel)))
        {newedge(col,row)=targetPixel;}
      break;
    
    case 2:
      if(((edge(col+1,row+1) < targetPixel) && 
          (edge(col-1,row-1) < targetPixel)) ||
         ((edge(col+1,row  ) < targetPixel) && 
          (edge(col-1,row  ) < targetPixel)))
        {newedge(col,row)=targetPixel;}
      break;
    
    case 3:
      if(((edge(col+1,row  ) < targetPixel) &&
          (edge(col-1,row  ) < targetPixel)) ||
         ((edge(col+1,row-1) < targetPixel) &&
          (edge(col-1,row+1) < targetPixel)))
        {newedge(col,row)=targetPixel;}
      break;
      
    case 0:
      if(((edge(col+1,row-1) < targetPixel) &&
          (edge(col-1,row+1) < targetPixel)) ||
         ((edge(col  ,row-1) < targetPixel) &&
          (edge(col  ,row+1) < targetPixel)))
        {newedge(col,row)=targetPixel;}
      break;
    }
  }
  return newedge;
}

//' extractEdgeMap
//' 
//' This function returns a cimg wrapped as NumeriMatrix.
//' using the gradient magnitude in the x and y directions,
//' estimated by the imager function \code{imgradient}
//' and the greadient angles
//' estimated as atan(dy/dx) and subsequently simplified and discretized
//' the canny edges are calculated, 
//' defined as the points in the gradient magnitude map that are 
//' greater than the 4(of the 8 posible) neighbors, orthogonal to the estimated angle
//'
//' @param gradientFromR cimg wraped as NumericVector
//' @param anglesFromR cimg wraped as NumericVector
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector extractEdgeMap(const Rcpp::NumericVector gradientFromR, 
                                   const Rcpp::NumericVector anglesFromR)
{
  cimg_library::CImg<double> gradient = Rcpp::as< cimg_library::CImg<double> >(gradientFromR);
  cimg_library::CImg<double> angles = Rcpp::as< cimg_library::CImg<double> >(anglesFromR);
  return Rcpp::wrap(nonMaxSuppress(gradient,simplifyAngles(angles)));
}
