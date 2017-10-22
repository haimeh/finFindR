#include <Rcpp.h>

void constrainInside(const Rcpp::NumericMatrix& map,
                     const unsigned int buffer,
                     int& count,
                     int& x,
                     int& y,
                     const bool justCheck = false)
{
  int width0=buffer;
  int height0=buffer;
  int width=map.nrow()-buffer-1;
  int height=map.ncol()-buffer-1;
  
  if(x<=width0)
  {
    if(!justCheck){x=width0;}
    count++;
  }
  if(y<=height0)
  {
    if(!justCheck){y=height0;}
    count++;
  }
  if(x>=width)
  {
    if(!justCheck){x=width;}
    count++;
  }
  if(y>=height)
  {
    if(!justCheck){y=height;}
    count++;
  }
}

void resetXY(const Rcpp::NumericMatrix& map,
             const unsigned int buffer,
             int& count,
             int& x,
             int& y,
             const int width0,
             const int height0,
             const int width,
             const int height,
             bool triggerIfOutside = false)
{
  if(triggerIfOutside)
  {
    int wasTriggered = 0;
    constrainInside(map,buffer,wasTriggered,x,y,true);
    if(wasTriggered>0)
    {
      y = floor(R::runif(height0,height)+0.5);
      x = floor(R::runif(width0,width)+0.5);
      
      constrainInside(map,buffer,count,x,y);
    }
  }else{
    y = floor(R::runif(height0,height)+0.5);
    x = floor(R::runif(width0,width)+0.5);
    
    constrainInside(map,buffer,count,x,y);
  }
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix traceStartSegments(const Rcpp::NumericMatrix edge,
                              const int xEst,
                              const int check_right, 
                              const int check_left, 
                              const int numSteps,
                              const double edgeDetect = 0.0)
{
  int buffer = 6;
  int y = floor(edge.ncol()*0.7);
  int x = floor(.5+((xEst + check_left>check_right?(edge.nrow()-1):0)/2));
  Rcpp::IntegerMatrix startOptions(numSteps,2);
  Rcpp::IntegerMatrix fail(1,2);
  // list for determining orientation and starting options
  Rcpp::List startMeasurements;
  
  
  // having these two things ready makes checking the surrounding pixels for an edge to follow easier
  const int a[12] = {-1,0,0,0,0,1,
                     3,1,2,3,4,3};
  const int b[12] = { 1,1,2,2,2,3, 
                      1,2,1,2,3,2};
  
  const double skip = 0.5;
  int tries = 0;
  int loopTrack = 0;
  const int loopLim = edge.ncol()+edge.nrow();
  
  int footprint=4;
  int newfootprint=4;
  double neighbors[8]={0.0};
 
  if (check_left > check_right)
  {
    newfootprint=3;
    while (tries < 5 && 
           R::runif(0.0,1.0) > (edge(x,y)*skip) &&
           loopTrack < loopLim)
    {
      loopTrack++;
      if(R::runif(0.0,1.0)>0.6){x--;}
      y--;
      resetXY(edge,buffer,tries, 
              x,y, 
              xEst,floor(edge.ncol()*.4),
              floor(edge.nrow()*.9),ceil(edge.ncol()*.9),
              true);
    }
  }else{
    newfootprint=5;
    while (tries < 5 && 
           R::runif(0.0,1.0) > (edge(x,y)*skip) &&
           loopTrack < loopLim)
    {
      loopTrack++;
      if(R::runif(0.0,1.0)>0.6){x++;}
      y--;
      resetXY(edge,buffer,tries, 
              x,y, 
              ceil(edge.nrow()*.1),floor(edge.ncol()*.4),
              xEst,ceil(edge.ncol()*.9),
              true);
    }
  }
  if(!(tries<5) || !(loopTrack < loopLim))
  {
    return fail;
  }
  
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  // once we reach the edge we check the edge orientation to make sure we are set to follow it correctly

  for (int i = 0; i != 8; i++){neighbors[i] = 0.0;}// reset the surroundings
  for (int i = 0; i != 6; i++)// fill with new local values
  {
    neighbors[0] += edge(x+a[i],y-a[i+6]);
    neighbors[1] += edge(x+b[i],y-b[i+6]);
    neighbors[2] += edge(x+a[i+6],y+a[i]);
    neighbors[3] += edge(x+b[i],y+b[i+6]);
    neighbors[4] += edge(x+a[i],y+a[i+6]);
    neighbors[5] += edge(x-b[i],y+b[i+6]);
    neighbors[6] += edge(x-a[i+6],y+a[i]);
    neighbors[7] += edge(x-b[i],y-b[i+6]);
  }
  
  // here we correct the orientation if it needs it
  if (neighbors[0]+neighbors[4] >= neighbors[6]+neighbors[2]){newfootprint=4;}
  if (neighbors[1]+neighbors[5] > neighbors[3]+neighbors[7] && newfootprint == 3){newfootprint=5;}
  if (neighbors[3]+neighbors[7] > neighbors[1]+neighbors[5] && newfootprint == 5){newfootprint=3;}

  // from here we follow the edge and collect the coordinates to send to R for finding the tip
  // at each coordinate update the algorithm checks only a region away from the last step it took
  // it only changes direction if the score skews to one side of the direction it was heading

  tries = 0;
  for (int pathStep = 0; pathStep < numSteps; pathStep++)
  {
    footprint = newfootprint;
    for (int i = 0; i != 8; i++){neighbors[i] = 0.0;}
    for (int i = 0; i != 6; i++)
    {
      neighbors[0] += edge(x+a[i],y-a[i+6]);
      neighbors[1] += edge(x+b[i],y-b[i+6]);
      neighbors[2] += edge(x+a[i+6],y+a[i]);
      neighbors[3] += edge(x+b[i],y+b[i+6]);
      neighbors[4] += edge(x+a[i],y+a[i+6]);
      neighbors[5] += edge(x-b[i],y+b[i+6]);
      neighbors[6] += edge(x-a[i+6],y+a[i]);
      neighbors[7] += edge(x-b[i],y-b[i+6]);
    }
    
    if (footprint == 0)
    {
      if(neighbors[3] >= neighbors[4] && neighbors[3] >= neighbors[5]){x++;y++;if(edge(x,y)>edgeDetect){newfootprint=7;}}
      else if(neighbors[5] >= neighbors[4] && neighbors[5] >= neighbors[3]){x--;y++;if(edge(x,y)>edgeDetect){newfootprint=1;}}
      else{y++;}
    }
    else if (footprint == 1)
    {
      if(neighbors[4] >= neighbors[5] && neighbors[4] >= neighbors[6]){y++;if(edge(x,y)>edgeDetect){newfootprint=0;}}
      else if(neighbors[6] >= neighbors[5] && neighbors[6] >= neighbors[4]){x--;if(edge(x,y)>edgeDetect){newfootprint=2;}}
      else{x--;y++;}
    }
    else if (footprint == 2)
    {
      if(neighbors[5] >= neighbors[6] && neighbors[5] >= neighbors[7]){x--;y++;if(edge(x,y)>edgeDetect){newfootprint=1;}}
      else if(neighbors[7] >= neighbors[6] && neighbors[7] >= neighbors[5]){x--;y--;if(edge(x,y)>edgeDetect){newfootprint=3;}}
      else{x--;}
    }
    else if (footprint == 3)
    {
      if(neighbors[6] >= neighbors[7] && neighbors[6] >= neighbors[0]){x--;if(edge(x,y)>edgeDetect){newfootprint=2;}}
      else if(neighbors[0] >= neighbors[7] && neighbors[0] >= neighbors[6]){y--;if(edge(x,y)>edgeDetect){newfootprint=4;}}
      else{x--;y--;}
    }
    else if (footprint == 4)
    {
      if(neighbors[7] >= neighbors[0] && neighbors[7] >= neighbors[1]){x--;y--;if(edge(x,y)>edgeDetect){newfootprint=3;}}
      else if(neighbors[1] >= neighbors[0] && neighbors[1] >= neighbors[7]){x++;y--;if(edge(x,y)>edgeDetect){newfootprint=5;}}
      else{y--;}
    }
    else if (footprint == 5)
    {
      if(neighbors[0] >= neighbors[1] && neighbors[0] >= neighbors[2]){y--;if(edge(x,y)>edgeDetect){newfootprint=4;}}
      else if(neighbors[2] >= neighbors[1] && neighbors[2] >= neighbors[0]){x++;if(edge(x,y)>edgeDetect){newfootprint=6;}}
      else{x++;y--;}
    }
    else if (footprint == 6)
    {
      if(neighbors[1] >= neighbors[2] && neighbors[1] >= neighbors[3]){x++;y--;if(edge(x,y)>edgeDetect){newfootprint=5;}}
      else if(neighbors[3] >= neighbors[2] && neighbors[3] >= neighbors[1]){x++;y++;if(edge(x,y)>edgeDetect){newfootprint=7;}}
      else{x++;}
    }
    else if (footprint == 7)
    {
      if(neighbors[2] >= neighbors[3] && neighbors[2] >= neighbors[4]){x++;if(edge(x,y)>edgeDetect){newfootprint=6;}}
      else if(neighbors[4] >= neighbors[3] && neighbors[4] >= neighbors[2]){y++;if(edge(x,y)>edgeDetect){newfootprint=0;}}
      else{x++;y++;}
    }
    
    if(tries<5)
    {
      constrainInside(edge,buffer, tries, x,y);
    }else{
      return startOptions;
    }
    
    //stop if we run out of line
    if(neighbors[0]+neighbors[1]+neighbors[2]+neighbors[3]+neighbors[4]+neighbors[5]+neighbors[6]+neighbors[7]==0)
    {
      if(tries>5)
      {
        return startOptions;
      }
      tries++;
      //perturbe the next attempt to follow
      footprint = (footprint + R::runif(0.0,1.0)>0.5?1:8) % 8;
      
      //jump a few steps
      if (footprint == 0){y+=3;}
      if (footprint == 1){x-=3;y+=3;}
      if (footprint == 2){x-=3;}
      if (footprint == 3){x-=3;y-=3;}
      if (footprint == 4){y-=3;}
      if (footprint == 5){x+=3;y-=3;}
      if (footprint == 6){x+=3;}
      if (footprint == 7){x+=3;y+=3;}
      
      //keep inside bounds
      constrainInside(edge,buffer, tries, x,y);
      
      for (int i = 0; i != 8; i++){neighbors[i] = 0.0;}
      for (int i = 0; i != 6; i++)// fill with new local values
      {
        neighbors[0] += edge(x+a[i],y-a[i+6]);
        neighbors[1] += edge(x+b[i],y-b[i+6]);
        neighbors[2] += edge(x+a[i+6],y+a[i]);
        neighbors[3] += edge(x+b[i],y+b[i+6]);
        neighbors[4] += edge(x+a[i],y+a[i+6]);
        neighbors[5] += edge(x-b[i],y+b[i+6]);
        neighbors[6] += edge(x-a[i+6],y+a[i]);
        neighbors[7] += edge(x-b[i],y-b[i+6]);
      }
      
      if(neighbors[0]+neighbors[1]+neighbors[2]+neighbors[3]+neighbors[4]+neighbors[5]+neighbors[6]+neighbors[7]==0)
      {
        // if were still lost, return what we have
        return startOptions;
      }
    }
    
    startOptions(pathStep,0) = x;
    startOptions(pathStep,1) = y;
  }
  return startOptions;
}