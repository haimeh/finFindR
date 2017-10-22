// [[Rcpp::depends(imager)]]
#include <imager.h>
#include <cmath>
#include <map>
#include <vector>
#include <iostream>

static float pi = 3.14159265358979323846;
std::vector<int> lookupTable;
cimg_library::CImg<double> origImg;

static int gaussianMask[7][7] =
{ 
  0 ,1, 1, 1, 1, 1, 0,
  1 ,1, 2, 3, 2, 1, 1,
  1 ,2, 4, 7, 4, 2, 1,
  1 ,3, 7, 8, 7, 3, 1,
  1 ,2, 4, 7, 4, 2, 1,
  1, 1, 2, 3, 2, 1, 1,
  0 ,1, 1, 1, 1, 1, 0
};


int rightshift(int num, const int neighbors)
{
  //left shift numbers and mask the MSB for bits equal to the number of neighbors
  return (num >> 1) | ((num << (neighbors - 1) & static_cast<int>(std::pow(2,neighbors)-1)));
}

int countSetBits(const int code)
{
  int count = 0;
  int v = code;
  //clears the LSB
  for(count = 0; v; count++){v &= v-1;}
  return count;
}

bool checkUniform(const int num, const int neighbors)
{
  //mask the BOI to exclude non topoligical info
  int BOI = num & static_cast<int>(std::pow(2,neighbors)-1);
  
  int b = rightshift(BOI,neighbors);
  int c = BOI ^ b;
  int count = countSetBits(c);
  if (count <= 2)
  {
    return true;
  }else{
    return false;
  }
}

int initUniform(const int neighbors, const int spectrum)
{
  int tableSize = std::pow(2,neighbors+spectrum)-1;
  lookupTable.resize(tableSize);
  int index=1;
  for(int i=0; i <= tableSize; i++)
  {
    bool status=checkUniform(i,neighbors);
    if(status==true)
    {
      lookupTable[i]=index;
      index++;
    }else{
      lookupTable[i]=0;
    }
    //std::cout<<lookupTable[i]<<std::endl;
  }
  return lookupTable[tableSize]+1;
}

int makeUniform(cimg_library::CImg<int>& lbpMap, const int neighbors = 8, const int spectrum = 3)
{
  int patternCount = initUniform(neighbors,spectrum);
  cimg_forXYZC(lbpMap,x,y,z,c)
  {
    //std::cout<<lbpMap(x,y,z,c)<<"->";
    lbpMap(x,y,z,c) = lookupTable[lbpMap(x,y,z,c)];
    //std::cout<<lbpMap(x,y,z,c)<<std::endl;
  }
  return patternCount;
}

cimg_library::CImg<double> calculateLBP(cimg_library::CImg<double> img,
                                        const int outX = 150,
                                        const int outY = 100,
                                        double radius = -1,
                                        int neighbors = 8,
                                        const bool colorLBP = false,
                                        const bool directedLBP = false,
                                        const bool uniformLBP = true,
                                        const int boxPartition = 1)
{
  if(radius == -1){radius = floor(std::max(1.0,std::min((static_cast<double>(img.width()/boxPartition)
                                              /(2.0*static_cast<double>(outX)+1.0))
                                             ,
                                           (static_cast<double>(img.height()/boxPartition)
                                              /(2.0*static_cast<double>(outY)+1.0)))));}
  
  
  cimg_library::CImg<int> lbpMap(floor(((static_cast<double>(img.width())
                                           /radius)-1.0)/2.0),
                                 floor(((static_cast<double>(img.height())
                                           /radius)-1.0)/2.0),
                                           img.depth(),1,0);
  std::cout<<radius<<':'<<lbpMap.width()<<','<<lbpMap.height()<<std::endl;
  
  //if we want the pattern returned in batch format
  int partX = ceil(static_cast<double>(lbpMap.width())/static_cast<double>(boxPartition));
  int partY = ceil(static_cast<double>(lbpMap.height())/static_cast<double>(boxPartition));
  
  std::cout<<boxPartition<<','<<boxPartition<<','<<partX<<','<<partY<<std::endl;
  
  cimg_library::CImg<int> lbpBatch(boxPartition,boxPartition,partX,partY,0);

  
  if(colorLBP)
  {
    cimg_library::CImg<double> colorImg = origImg.get_resize(lbpMap.width(),lbpMap.height(),1,origImg.spectrum(),2);
    
    cimg_forC(colorImg,c)
    {
      cimg_library::CImg<double> standardize = colorImg.get_shared_channel(c);
      standardize -= standardize.mean();
      standardize /= standardize.variance();
    }
    cimg_forC(colorImg,c)
    {
      for (int cc=1; cc<=floor(origImg.spectrum()/2); cc++)
      {
        if (origImg.spectrum()%2!=0 || c<=floor(origImg.spectrum()/2))
        {
          cimg_forXYZ(lbpMap,x,y,z)
          {
            if(boxPartition == 1)
            {
              lbpMap(x,y,z) += (colorImg(x,y,z,c) > colorImg(x,y,z,((c+1)%origImg.spectrum()))) << (neighbors+c);
            }else{
              lbpBatch(x % boxPartition,
                       y % boxPartition,
                       static_cast<int>(floor(static_cast<double>(x)/static_cast<double>(boxPartition))),
                       static_cast<int>(floor(static_cast<double>(y)/static_cast<double>(boxPartition))))
             
                += (colorImg(x,y,z,c) > colorImg(x,y,z,((c+1) % origImg.spectrum()))) << (neighbors+c);
            }
          }
        }
      }
    }
  }
  
  int neighborDiv = 1;
  for(int n=0; n<neighbors; n++)
  {
    // find displacement
    if(directedLBP){neighborDiv = 2;}else{neighborDiv = 1;}
    double X = static_cast<double>(-radius) * sin(2.0*pi*n/static_cast<double>(neighbors*neighborDiv));
    double Y = static_cast<double>(radius) * cos(2.0*pi*n/static_cast<double>(neighbors*neighborDiv));
    
    // relative indices
    int fx = static_cast<int>(floor(X));
    int fy = static_cast<int>(floor(Y));
    int cx = static_cast<int>(ceil(X));
    int cy = static_cast<int>(ceil(Y));
    // fractional part
    double ty = Y - fy;
    double tx = X - fx;
    // set interpolation weights
    double w1 = (1 - tx) * (1 - ty);
    double w2 =      tx  * (1 - ty);
    double w3 = (1 - tx) *      ty;
    double w4 =      tx  *      ty;
    
    cimg_forXY(lbpMap,x,y)
    {
      int bigX = floor(radius*((static_cast<double>(x)*2.0)+1.0));
      int bigY = floor(radius*((static_cast<double>(y)*2.0)+1.0));
      // calculate interpolated value
      double t = w1*img(bigX+fx,bigY+fy) + w2*img(bigX+fx,bigY+cy) + w3*img(bigX+cx,bigY+fy) + w4*img(bigX+cx,bigY+cy);
      // shift the "yes" or "no" answer to the apropriate sample interval for the binary representation
      if(!directedLBP)
      {
        if(boxPartition==1)
        {
          lbpMap(x,y) += (t > img(bigX,bigY)) << n;
        }else{
          lbpBatch(x % boxPartition,
                   y % boxPartition,
                   static_cast<int>(floor(static_cast<double>(x)/static_cast<double>(boxPartition))),
                   static_cast<int>(floor(static_cast<double>(y)/static_cast<double>(boxPartition))))
          
          += (t > img(bigX,bigY)) << n;
        }
      }else{
        double u = w1*img(bigX-fx,bigY-fy) + w2*img(bigX-fx,bigY-cy) + w3*img(bigX-cx,bigY-fy) + w4*img(bigX-cx,bigY-cy);
        
        if(boxPartition==1)
        {
          lbpMap(x,y) += (t >= u) << n;;
        }else{
          lbpBatch(x % boxPartition,
                   y % boxPartition,
                   static_cast<int>(floor(static_cast<double>(x)/static_cast<double>(boxPartition))),
                   static_cast<int>(floor(static_cast<double>(y)/static_cast<double>(boxPartition))))
          += (t >= u) << n;
        }
      }
    }
  }
  if(boxPartition==1)
  {
    if(uniformLBP){makeUniform(lbpMap,
                               neighbors,
                               colorLBP?origImg.spectrum():0);}
    
    //here we return the raw lbp
    cimg_library::CImg<double> toR = ((outX>1) & (outY>1))?lbpMap.get_resize(outX, outY,lbpMap.depth(),1) : lbpMap;
    return toR;
    
  }else{
    int patternSize = std::pow( 2, colorLBP?neighbors+origImg.spectrum() : neighbors );
    if(uniformLBP){patternSize = makeUniform(lbpBatch,
                                             neighbors,
                                             colorLBP?origImg.spectrum():0);}
    
    //we can partition the image into normed subregions and stack them for a batched classification
    cimg_library::CImg<double> toR(lbpBatch.depth(),lbpBatch.spectrum(),1,patternSize,0);
    cimg_library::CImg<double> patternFrequencyMap = toR;
    cimg_library::CImg<double> l2Norm(lbpBatch.depth(),lbpBatch.spectrum(),1,1,0);
    
    
    //pattern frequency for single region
    cimg_forXY(patternFrequencyMap,xPrime,yPrime)
    {
      cimg_forXY(lbpBatch,xSubregion,ySubregion)
      {
        ++patternFrequencyMap(xPrime,yPrime,0,lbpBatch(xSubregion,ySubregion,xPrime,yPrime));
      }
    }
    
    //pattern frequency weighted by proximity via gaussian Mask
    cimg_forXYC(patternFrequencyMap,x,y,c)
    {
      for (int i=-2; i != 3; i++)
      {
        for (int j =-2; j != 3; j++)
        {
          if(x+i < patternFrequencyMap.width() &&
             x+i >= 0 &&
             y+j < patternFrequencyMap.height() &&
             y+j >= 0)
          {
            //we center the gaussianMask at 2,2
            toR(x,y,c) += patternFrequencyMap(x+i,y+j,c)*gaussianMask[3+i][3+j];
          }
        }
      }
      l2Norm(x,y) += std::pow(toR(x,y,c),2);
    }
    //finalize the l2 normalized vector
    if((outX>1) & (outY>1))
    {
      return toR.get_div(l2Norm.sqrt()).get_resize(outX, outY, 1, toR.spectrum(),5);
    }else{
      return toR.get_div(l2Norm.sqrt());
    }
  }
}

// [[Rcpp::export]]
Rcpp::NumericVector LBP(const SEXP imageFromR,
                        const int outX = 150,
                        const int outY = 100,
                        double radius = -1,
                        const int neighbors = 8,
                        const bool colorLBP = false,
                        const bool directedLBP = false,
                        const bool uniformLBP = true,
                        const int boxPartition = 1)
{
  switch (TYPEOF(imageFromR))
  {
    case REALSXP:
    {
      cimg_library::CImg<double> img = origImg.assign(Rcpp::as< cimg_library::CImg<double> >(imageFromR)).get_RGBtoHSV().channel(2);
      return Rcpp::wrap(calculateLBP(img,
                                     outX,
                                     outY,
                                     radius,
                                     neighbors,
                                     colorLBP,
                                     directedLBP,
                                     uniformLBP,
                                     boxPartition));
    }
    case STRSXP:
    {
      std::string imageName = CHAR(STRING_ELT(imageFromR, 0));
      cimg_library::CImg<double> img = origImg.load(imageName.c_str()).get_RGBtoHSL().channel(2);
      return Rcpp::wrap(calculateLBP(img,
                                     outX,
                                     outY,
                                     radius,
                                     neighbors,
                                     colorLBP,
                                     directedLBP,
                                     uniformLBP,
                                     boxPartition));
    }
  }
  std::cout<<"Must be of type string(to load image) or cimg(to wrap image)"<<std::endl;
  return 0;
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
                     2).save_bmp(saveName.c_str());
  }
}