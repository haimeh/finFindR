
// [[Rcpp::depends(imager)]]
#include <imager.h>
#include <vector>


cimg_library::CImg<int> simplifyAngles(const cimg_library::CImg<double> ang)
{
  cimg_library::CImg<int> simplifiedAngles(ang.width(),ang.height(),1,1,0);
  int approxAng = 0;
  const double pi = 3.1415926535897932;
  cimg_forXY(ang,x,y)
  {
    if (ang(x,y) > ( 5*pi/12))approxAng = 0;
    if (ang(x,y) < ( 5*pi/12))approxAng = 1;
    if (ang(x,y) < ( 4*pi/12))approxAng = 2;
    if (ang(x,y) < ( 2*pi/12))approxAng = 3;
    if (ang(x,y) < ( 1*pi/12))approxAng = 4;
    if (ang(x,y) < (-1*pi/12))approxAng = 5;
    if (ang(x,y) < (-2*pi/12))approxAng = 6;
    if (ang(x,y) < (-4*pi/12))approxAng = 7;
    if (ang(x,y) < (-5*pi/12))approxAng = 0;
    
    simplifiedAngles(x,y) = approxAng;
  }
  return simplifiedAngles;
}

void nonMaxSuppress(cimg_library::CImg<double>& edge, 
                    const cimg_library::CImg<double> ang)
{
  cimg_library::CImg<double> newedge = edge;
  
  cimg_for_insideXY(edge,col,row,1)
  {
    if (ang(col,row) == 0)
    {
      if((edge(col,row+1) > edge(col,row)) || (edge(col,row-1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
    if (ang(col,row) == 1)
    {
      if((edge(col,row+1) > edge(col,row)) || (edge(col,row-1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
      if((edge(col+1,row+1) > edge(col,row)) || (edge(col-1,row-1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
    if (ang(col,row) == 2)
    {
      if((edge(col+1,row+1) > edge(col,row)) || (edge(col-1,row-1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
    if (ang(col,row) == 3)
    {
      if((edge(col+1,row+1) > edge(col,row)) || (edge(col-1,row-1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
      if((edge(col+1,row) > edge(col,row)) || (edge(col-1,row) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
    if (ang(col,row) == 4)
    {
      if((edge(col+1,row) > edge(col,row)) || (edge(col-1,row) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
    if (ang(col,row) == 5)
    {
      if((edge(col+1,row) > edge(col,row)) || (edge(col-1,row) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
      if((edge(col+1,row-1) > edge(col,row)) || (edge(col-1,row+1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
    if (ang(col,row) == 6)
    {
      if((edge(col+1,row-1) > edge(col,row)) || (edge(col-1,row+1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
    if (ang(col,row) == 7)
    {
      if((edge(col+1,row-1) > edge(col,row)) || (edge(col-1,row+1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
      if((edge(col,row+1) > edge(col,row)) || (edge(col,row-1) > edge(col,row))){newedge(col,row)=0;}else{newedge(col,row)=edge(col,row);}
    }
  }
  cimg_for_insideXY(edge,col,row,1)
  {
    edge(col,row)=newedge(col,row);
  }
}

//image should have glare cliped to 0 already
void fillGlare(cimg_library::CImg<double>& image, const Rcpp::DataFrame highlightCoordinates)
{
  const int collectX[4] = 
  { 0, 1, 0, -1}; 
  const int collectY[4] = 
  {-1, 0, 1,  0};
  
  float fillThreshold = image.mean();
  float regionSample = 0.0;
  int c = 0;
  float collected = 0.0;
  
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
        for (int h = 0; h < 4; h++)
        {
          if (*xit+collectX[h] >= 0 && 
              *yit+collectY[h] >= 0 && 
              *xit+collectX[h] < image.width() && 
              *yit+collectY[h] < image.height())
          {
            //store value to add later
            regionSample = image(*xit+collectX[h],*yit+collectY[h],0,*cit);
            if(regionSample > 0 && regionSample > fillThreshold)
            {
              c++;
              collected += regionSample;
            }
          }
        }
        //get average of available surrounding values
        if(c>0)
        {
          newFill(*xit,*yit,0,*cit) = collected/c; 
          changed=true;
          c = 0;
          collected = 0.0;
        }
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
}

// [[Rcpp::export]]
Rcpp::NumericVector cleanEnhance(const Rcpp::NumericVector imageFromR, 
                                 const Rcpp::NumericVector glareFromR,
                                 const Rcpp::DataFrame glareCoordinates)
{
  cimg_library::CImg<double> image = Rcpp::as< cimg_library::CImg<double> >(imageFromR);
  cimg_library::CImg<bool> glare = Rcpp::as< cimg_library::CImg<bool> >(glareFromR);
  
  std::cout<<"Glare: "<<glare.mean()<<std::endl;
  if(glare.mean()>.94)
  {
    fillGlare(image.mul(glare),glareCoordinates);
  }
  
  // int iterationsCount = 0;
  // while(iterationsCount < ceil(image.width()/300))//160
  // {
  //   image.erode(2);
  //   iterationsCount++;
  // }
  // for(int i = iterationsCount; i > 1; i--)
  // {
  //   image.dilate(2);
  // }
  return Rcpp::wrap(image);
}

// [[Rcpp::export]]
Rcpp::NumericVector extractEdgeMap(const Rcpp::NumericVector gradientFromR, 
                                   const Rcpp::NumericVector anglesFromR)
{
  cimg_library::CImg<double> gradient = Rcpp::as< cimg_library::CImg<double> >(gradientFromR);
  cimg_library::CImg<double> angles = Rcpp::as< cimg_library::CImg<double> >(anglesFromR);
  nonMaxSuppress(gradient,simplifyAngles(angles));
  
  return Rcpp::wrap(gradient);
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
  const int gaussianMask[5][5] =
  {
    0, 1, 1, 1, 0,
    1, 1, 2, 1, 1,
    1, 2, 2, 2, 1,
    1, 1, 2, 1, 1,
    0, 1, 1, 1, 0,
  };
  
  std::cout << "extracting angles" << std::endl;
  Rcpp::IntegerVector path = Rcpp::rev(backpath);
  int x = startx;
  int y = starty;
  double angVal = 0;
  double gausSum = 0.0;
  std::vector<double> angles;
  Rcpp::IntegerVector::iterator it;
  for(it = path.begin(); it != path.end(); ++it)
  {
    gausSum = 0.0;
    angVal = 0.0;
    for (int i=-2; i != 3; i++)
    {
      for (int j =-2; j != 3; j++)
      {
        if(x+i < ang.width() &&
           x+i >= 0 &&
           y+j < ang.height() &&
           y+j >= 0)
        {
          angVal += std::tan(ang(x+i,y+j))*gaussianMask[2+i][2+j];
          gausSum += gaussianMask[2+i][2+j];
        }
      }
    }
    angles.push_back(std::atan(angVal/gausSum));
    x = x+dx[*it];
    y = y+dy[*it];
  }
  Rcpp::NumericVector anglevector = Rcpp::wrap(angles);
  std::cout << "finalizing" << std::endl;
  return anglevector;
}


