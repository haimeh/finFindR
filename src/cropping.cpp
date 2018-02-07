// [[Rcpp::depends(imager)]]
#include <imager.h>

cimg_library::CImg<double> origImg;

// [[Rcpp::export]]
Rcpp::NumericVector initializeImg(const std::string imageName,
                                        const int newX,
                                        const int newY)
{
  //Rcpp::Rcout<<imageName<<std::endl;
  cimg_library::CImg<double> img = origImg.load(imageName.c_str());
  return(Rcpp::wrap(img.get_resize(newX,newY,-100,-100,2)));
}

// [[Rcpp::export]]
void saveCrop(const std::string saveName, 
              int xMin, 
              int xMax, 
              int yMin, 
              int yMax, 
              const int spanX, 
              const int spanY,
              const int thresh)
{
  int stretch = ceil(((origImg.width()/spanX)+(origImg.height()/spanY))/2);
  xMin *= stretch;
  xMax *= stretch;
  yMin *= stretch;
  yMax *= stretch;
  
  if((xMax-xMin > thresh) &
     (yMax-yMin > thresh))
  {
    origImg.get_crop(xMin < 1?1:xMin,
                     yMin < 1?1:yMin,
                            0,
                            0,
                            xMax < origImg.width()-1?xMax:origImg.height()-2,
                                   yMax < origImg.height()-1?yMax:origImg.height()-2,
                                          0,
                                          2).save_jpeg(saveName.c_str());
  }
}