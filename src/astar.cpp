#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>


//////////////////////////////PATH NODE/////////////////////////////////
class node
{
  private:
    int xPos;
    int yPos;
    int S;// Steps thus far
    int R;// Remaining distance
    int T;// Total path length estimate (S+R)
    int fwic;// from whence it came(direction 0-7)

  public:
    node(const int x, 
         const int y, 
         const int xEnd, 
         const int yEnd, 
         const int parent_S, 
         const int k, 
         const float edgeVal)
    {
      fwic = k+10;//the +10 makes it easy to identify checked positions from the condition map
      xPos = x;
      yPos = y;
      
      //10 if up down left right, 14 if diagonal movement, crossing through the empty pixels costs extra
      //cost for traversing a pixel determined by brightness
      S = parent_S + ( (k%2==0?10:14)*ceil(std::max((-10.0*std::pow(edgeVal,2))+10, 1.0)));
      
      int xd = xEnd - xPos; 
      int yd = yEnd - yPos;
      R = sqrt(xd*xd+yd*yd)*10;
      T = S+R;
    }
    //~node();
    
    int get_xPos() const{return xPos;}//where are we?
    int get_yPos() const{return yPos;}
    int get_S() const{return S;}//how far have we come?
    int get_R() const{return R;}//how far have we yet to go?
    int get_T() const{return T;}//how much would this cost us?
    int get_fwic() const{return fwic;}//in which direction did we step to get here?
};

//comparison operator for nodes in heap
bool operator < (const node & a, const node & b)
{
  return a.get_T() > b.get_T();
}
//vector of nodes to be made into a heap
std::vector<node> node_heap;

//' findPath
//' 
//' This function returns a NumeriMatrix, identifying the optimal edge
//' Using the A* algorithm on a canny edge matrix weighted by a neural network
//' The traversal balances clarity/smoothness of the edge against the network evaluation 
//'
//' @param edge Weighted canny edge matrix
//' @param startx Starting pixel x component
//' @param starty Starting pixel y component
//' @param endx Ending pixel x component
//' @param endy Ending pixel y component
//' @param minX Left boundary
//' @param maxX Right boundary
//' @param minY Upper boundary
//' @param maxY Lower boundary
//' @param proximity Distance to ending coordinate at which trace can terminate
// [[Rcpp::export]]
std::vector<int> findPath(const Rcpp::NumericMatrix edge, 
                           const int startx, 
                           const int starty, 
                           const int endx, 
                           const int endy,
                           const int minX,
                           const int maxX,
                           const int minY,
                           const int maxY,
                           const int proximity)
{
  //directions available to a given node
  const int dx[8]={ 0, 1, 1, 1, 0, -1, -1, -1};
  const int dy[8]={-1,-1, 0, 1, 1,  1,  0, -1};
  
  int x = startx;
  int y = starty;
  int s = 0;
  int FWIC = 0;
  
  int minR_x = startx;
  int minR_y = starty;
  
  Rcpp::IntegerMatrix condition(edge.nrow(),edge.ncol());//what has been done thus far at a given position
  
  
  //initiate a heap for the nodes
  make_heap(node_heap.begin(),node_heap.end());
  
  //create a start node
  //add it to vector and then tell the heap
  node startNode(x, y, endx, endy,-10, 0, 1);//-10 so that the starting step length is 0
  condition(x,y) = 1;condition(x+1,y) = 1;condition(x-1,y) = 1;condition(x,y+1) = 1;condition(x,y-1) = 1;
  node_heap.push_back(startNode); push_heap(node_heap.begin(),node_heap.end());
  
  //for tracking what is the best we have gotten thus far
  int r = node_heap.front().get_R();
  
  //initiate vector for hodling path
  std::vector<int> backtrack;
  
  //investigate node on top of heap and generate its neighbors
  Rcpp::Rcout <<"begining path search"<<std::endl;
  int loopCount = 0;
  while(!node_heap.empty())
  {
    loopCount++;
    //currentNode is the node from top of the heap
    //make sure it is an unchecked position/in bounds and if so
    //evaluate the node
    if(condition(node_heap.front().get_xPos(),node_heap.front().get_yPos()) != 1 ||
       node_heap.front().get_yPos() < minY || 
       node_heap.front().get_yPos() > maxY || 
       node_heap.front().get_xPos() < minX || 
       node_heap.front().get_xPos() > maxX )
      // || std::sqrt(std::pow(std::abs(startx-node_heap.front().get_xPos()),2)+
      //           std::pow(std::abs(starty-node_heap.front().get_yPos()),2)) > maxRadius)
    {
       Rcpp::Rcout <<"loop: "<<loopCount
                 <<" ~ pop: "<<node_heap.size()
                 <<" ~ position == 1?: "<<condition(node_heap.front().get_xPos(),node_heap.front().get_yPos())
                 <<" ~ x > max?: "<<(node_heap.front().get_xPos()>maxX)
                 <<" ~ x < min?: "<<(node_heap.front().get_xPos()<minX)
                 <<" ~ y > max?: "<<(node_heap.front().get_yPos()>maxY)
                 <<" ~ y < min?: "<<(node_heap.front().get_yPos()<minY)<<std::endl;
    }else{
      x = node_heap.front().get_xPos();
      y = node_heap.front().get_yPos();
      
      s = node_heap.front().get_S();// for child nodes to know how many steps their parent took
      FWIC = node_heap.front().get_fwic();// to know what direction the parent node was made if we need to jump a gap
      condition(x,y) = FWIC;// mark this position with the direction used to get here so we know not to check again
      
      // if we dont find the end, use this to keep track of the best path found thus far
      if (node_heap.front().get_R() < r){r=node_heap.front().get_R(); minR_x = x; minR_y = y;}
      
      //finish if we are close to the end target
      if(node_heap.front().get_R() < proximity)
      {
        Rcpp::Rcout <<"finished size: "<<node_heap.size()<<"  "<<x<<","<<y<<std::endl;
        //backtrack from current node using the spawn directions(fwic) as a trail
        while (!(x == startx && y == starty))
        {
          //the -10 is to go back to (0-7) from (10-17)
          backtrack.push_back(condition(x,y)-10);
          x = x-dx[backtrack.back()];
          y = y-dy[backtrack.back()];
        }
        node_heap.clear();
        return backtrack;
      }
  
      //remove node from heap now that it has been evaluated
      pop_heap(node_heap.begin(),node_heap.end()); node_heap.pop_back();
      if(node_heap.size()>100000)
      {
        Rcpp::Rcout <<"node overflow"<<std::endl;
        x=minR_x; y=minR_y;
        while (!(x == startx && y == starty))
        {
          //the -10 is to go back to (0-7) from (10-17)
          backtrack.push_back(condition(x,y)-10);
          x = x-dx[backtrack.back()];
          y = y-dy[backtrack.back()];
        }
        node_heap.clear();
        return backtrack;
      }
      
      
      //prepare to create child node in each viable direction
      for(int k=0; k<8; k++)
      {
        int newx = x+dx[k];
        int newy = y+dy[k];
        
        //make sure the new node would be within the image boundary
        //make sure the node isnt already tested

        if(newy > minY && newy < maxY && newx > minX && newx < maxX && condition(newx,newy) < 1
           )
        {
          //Rcpp::Rcout <<"new"<< newx <<" "<< newy <<std::endl;
          //NEW NODE
          //spawn nodes from latest step
          //mark as open for investigation
          //add node to heap
          node childNode(newx,newy,endx,endy,s,k,edge(newx,newy));
          condition(newx,newy) = 1;
          node_heap.push_back(childNode); push_heap(node_heap.begin(),node_heap.end());
        }
      }
    }
  }
  Rcpp::Rcout <<"failue to find complete path"<<std::endl;
  x=minR_x; y=minR_y;
  //backtrack from minR x and y using the spawn directions(fwic) as a trail
  while (!(x == startx && y == starty))
  {
    //the -10 is to go back to (0-7) from (10-17)
    backtrack.push_back(condition(x,y)-10);
    x = x-dx[backtrack.back()];
    y = y-dy[backtrack.back()];
  }
  return backtrack;
}
