#define cimg_display 0
// [[Rcpp::depends(imager)]]
#include <imager.h>
#include <vector>
#include <algorithm>


//' extractAnnulus
//' 
//' This function returns a cimg wrapped as NumeriMatrix.
//' Contains simplified image reduced to relevant identifying features.
//' Features represent 16 point ring from around each coordinate
//' The x component in the returned cimg represents each coordinate
//' The y component from the returned cimg represents each sample from the 16 point ring
//'
//' @param imageFromR cimg wraped as NumericVector
//' @param xCoordinates x pixel coordinates for data extraction
//' @param yCoordinates y pixel coordinates for data extraction
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector extractAnnulus(const Rcpp::NumericVector imageFromR, 
                                   const Rcpp::IntegerVector xCoordinates, 
                                   const Rcpp::IntegerVector yCoordinates)
{
  if(xCoordinates.size() != yCoordinates.size())
  {
    Rcpp::Rcout << "Coordinate vectors are different lengths. Subsetting by Min length" << std::endl;
  }
  int traceLength = std::min(xCoordinates.size(),yCoordinates.size());
  
  cimg_library::CImg<double> image = Rcpp::as< cimg_library::CImg<double> >(imageFromR);
  cimg_library::CImg<double> chromaAnnulus(traceLength,16,1,3,0);
  
  const double pi = 3.1415926535897932;
  double radius = static_cast<int>(traceLength)/130.0;
  
  for(int n=0; n<16; n++)
  {
    for(int color=0; color<3; color++)
    {
      // restart
      int tracePosition = 0;
      
      // find displacement
      int Xdisplacement = static_cast<int>(ceil((radius * std::cos(2.0*pi*n/16.0))-.5));
      int Ydisplacement = static_cast<int>(ceil((radius * std::sin(2.0*pi*n/16.0))-.5));
      
      while(tracePosition < traceLength)
      {
        int x = xCoordinates[tracePosition];
        int y = yCoordinates[tracePosition];
        
        double chromaSampleSum = 0.0;
        double chromaSampleDeviation = 0.0;
        int chromaSampleCount = 0;
        
        int chromaNegCount = 0;
        int chromaPosCount = 0;
        double chromaPosDeviation = 0.0;
        double chromaNegDeviation = 0.0;
        
        double chromaSampleTest = 0.0;
        
        double chromaMem[15][15]={0.0};
        double chromaMean = 0.0;
        
        // initial sample to estimate distribution
        for (int i=-7; i != 8; i++)
        {
          for (int j =-7; j != 8; j++)
          {
            if(x+i+Xdisplacement < image.width() &&
               x+i+Xdisplacement >= 0 &&
               y+j+Ydisplacement < image.height() &&
               y+j+Ydisplacement >= 0)
            {
              chromaMem[7+i][7+j] = image(x+i+Xdisplacement,y+j+Ydisplacement,color);
              chromaMean += chromaMem[7+i][7+j];
              ++chromaSampleCount;
            }
          }
        }
        chromaMean /= static_cast<double>(chromaSampleCount);
        chromaSampleCount = 0.0;
        
        // determine threshold for estimator
        for (int i=0; i != 15; i++)
        {
          for (int j=0; j != 15; j++)
          {
            chromaSampleDeviation = chromaMean-chromaMem[i][j];
            if(chromaSampleDeviation <= 0.0)
            {
              chromaNegDeviation -= chromaSampleDeviation;
              ++chromaNegCount;
            }else{
              chromaPosDeviation += chromaSampleDeviation;
              ++chromaPosCount;
            }
          }
        }
        
        chromaNegDeviation /= static_cast<double>(chromaNegCount);
        chromaPosDeviation /= static_cast<double>(chromaPosCount);
        if(chromaNegDeviation>chromaPosDeviation)
        {
          chromaSampleDeviation = chromaNegDeviation;
        }else{
          chromaSampleDeviation = chromaPosDeviation;
        }
        // save final estimation with aggressive outlier removal
        for (int i=0; i != 15; i++)
        {
          for (int j =0; j != 15; j++)
          {
            chromaSampleTest = std::abs(chromaMem[i][j]-chromaMean);
            if(chromaSampleTest<=chromaSampleDeviation)
            {
              chromaSampleSum += chromaMem[i][j];
              ++chromaSampleCount;
            }
          }
        }
        chromaAnnulus(tracePosition,n,0,color) = chromaSampleSum/chromaSampleCount;
        
        // prep to calculate next sample point
        ++tracePosition;
      }
    }
  }
  Rcpp::Rcout << "finalizing" << std::endl;
  return Rcpp::wrap(chromaAnnulus);
}


