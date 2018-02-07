
// [[Rcpp::depends(imager)]]
#include <imager.h>
#include <vector>
#include <algorithm>


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

//image should have glare cliped to 0 already
// [[Rcpp::export]]
Rcpp::NumericVector fillGlare(const Rcpp::NumericVector imageFromR, 
                              const Rcpp::DataFrame highlightCoordinates, 
                              const int aveThresh=7)
{
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


// [[Rcpp::export]]
Rcpp::NumericVector extractEdgeMap(const Rcpp::NumericVector gradientFromR, 
                                   const Rcpp::NumericVector anglesFromR)
{
  cimg_library::CImg<double> gradient = Rcpp::as< cimg_library::CImg<double> >(gradientFromR);
  cimg_library::CImg<double> angles = Rcpp::as< cimg_library::CImg<double> >(anglesFromR);
  return Rcpp::wrap(nonMaxSuppress(gradient,simplifyAngles(angles)));
}

// [[Rcpp::export]]
Rcpp::NumericVector extractAngles(const Rcpp::NumericVector angFromR, 
                                  const Rcpp::IntegerVector backpath, 
                                  const int startx, 
                                  const int starty)
{
  cimg_library::CImg<double> ang = Rcpp::as< cimg_library::CImg<double> >(angFromR);
  const int dx[8]={ 0, 1, 1, 1, 0, -1, -1, -1};
  const int dy[8]={-1,-1, 0, 1, 1,  1,  0, -1};
  
  double angleDXMem[15][15]={0.0};
  double angleDYMem[15][15]={0.0};
  
  Rcpp::Rcout << "extracting angles" << std::endl;
  Rcpp::IntegerVector path = Rcpp::rev(backpath);
  int x = startx;
  int y = starty;
  
  double angleVal = 0.0;
  
  double angleDXMean = 0.0;
  double angleDYMean = 0.0;
  
  double angleDXSum = 0.0;
  double angleDYSum = 0.0;
  int angleDXCount = 0;
  int angleDYCount = 0;
  
  int angleInsideCount = 0;
  int angleDXPosCount = 0;
  int angleDYNegCount = 0;
  int angleDXNegCount = 0;
  int angleDYPosCount = 0;
  
  double angleDeviationDX = 0.0;
  double angleDeviationDY = 0.0;
  double angleDXTest = 0.0;
  double angleDYTest = 0.0;
  
  double angleNegDXSD = 0.0;
  double angleNegDYSD = 0.0;
  double anglePosDXSD = 0.0;
  double anglePosDYSD = 0.0;
  
  std::vector<double> angles;
  Rcpp::IntegerVector::iterator it;
  for(it = path.begin(); it != path.end(); ++it)
  {
    angleDXMean = 0.0;
    angleDYMean = 0.0;
    
    angleDXSum = 0.0;
    angleDYSum = 0.0;
    angleDXCount = 0;
    angleDYCount = 0;
    
    angleDXPosCount = 0;
    angleDYNegCount = 0;
    angleDXNegCount = 0;
    angleDYPosCount = 0;
    angleInsideCount = 0;
    
    angleNegDXSD = 0.0;
    angleNegDYSD = 0.0;
    anglePosDXSD = 0.0;
    anglePosDYSD = 0.0;
    
    for (int i=-7; i != 8; i++)
    {
      for (int j =-7; j != 8; j++)
      {
        if(x+i < ang.width() &&
           x+i >= 0 &&
           y+j < ang.height() &&
           y+j >= 0)
        {
          angleDYMem[7+i][7+j] = std::sin(ang(x+i,y+j));
          angleDXMem[7+i][7+j] = std::cos(ang(x+i,y+j));
          
          angleDYMean += angleDYMem[7+i][7+j];
          angleDXMean += angleDXMem[7+i][7+j];
          
          ++angleInsideCount;
        }
      }
    }
    
    angleDYMean /= static_cast<double>(angleInsideCount) ;
    angleDXMean /= static_cast<double>(angleInsideCount);
    
    for (int i=0; i != 14; i++)
    {
      for (int j =0; j != 14; j++)
      {
        angleDeviationDX = angleDXMean-angleDXMem[i][j];
        angleDeviationDY = angleDYMean-angleDYMem[i][j];
        if(angleDeviationDX <= 0.0)
        {
          angleNegDXSD -= angleDeviationDX;
          ++angleDXNegCount;
        }else{
          anglePosDXSD += angleDeviationDX;
          ++angleDXPosCount;
        }
        if(angleDeviationDY <= 0.0)
        {
          angleNegDYSD -= angleDeviationDY;
          ++angleDYNegCount;
        }else{
          anglePosDYSD += angleDeviationDY;
          ++angleDYPosCount;
        }
      }
    }
    
    if(angleNegDXSD>anglePosDXSD)
    {
      angleDeviationDX = angleNegDXSD/static_cast<double>(angleDXNegCount);
    }else{
      angleDeviationDX = anglePosDXSD/static_cast<double>(angleDXPosCount);
    }
    if(angleNegDYSD>anglePosDYSD)
    {
      angleDeviationDY = angleNegDYSD/static_cast<double>(angleDYNegCount);
    }else{
      angleDeviationDY = anglePosDYSD/static_cast<double>(angleDYPosCount);
    }
    
    for (int i=0; i != 14; i++)
    {
      for (int j =0; j != 14; j++)
      {
        angleDXTest = std::abs(angleDXMem[i][j]-angleDXMean);
        angleDYTest = std::abs(angleDYMem[i][j]-angleDYMean);
        
        if(angleDXTest<=angleDeviationDX)
        {
          angleDXSum += angleDXMem[i][j];
          ++angleDXCount;
        }
        if(angleDYTest<=angleDeviationDY)
        {
          angleDYSum += angleDYMem[i][j];
          ++angleDYCount;
        }
      }
    }
    angleVal = atan2(angleDYSum/angleDYCount,
                     angleDXSum/angleDXCount);
    angles.push_back(std::atan(angleVal));
    x = x+dx[*it];
    y = y+dy[*it];
  }
  Rcpp::NumericVector anglevector = Rcpp::wrap(angles);
  Rcpp::Rcout << "finalizing" << std::endl;
  return anglevector;
}


