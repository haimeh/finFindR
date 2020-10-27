.onLoad <- function(libname,pkgname)
{
  require("mxnet")
  imager:::cimg.use.openmp("never")
  .finFindREnv <- new.env()
  .finFindREnv$finIter <- setRefClass("finIter",
                                      
                                      fields=c("data",
                                               "iter",
                                               "data.shape"),
                                      contains = "Rcpp_MXArrayDataIter",
                                      
                                      methods=list(
                                        initialize=function(iter=NULL,
                                                            data,
                                                            data.shape){
                                          data_len <- prod(data.shape)
                                          print(paste0("shp:",data.shape))
                                          array_iter <- mx.io.arrayiter(data,
                                                                        label=rep(0,ncol(data)),
                                                                        batch.size=min(ncol(data),128))
                                          
                                          .self$iter <- array_iter
                                          
                                          .self$data.shape <- data.shape
                                          
                                          .self
                                        },
                                        
                                        value=function(){
                                          val.x <- as.array(.self$iter$value()$data)
                                          val.x[is.na(val.x) | is.nan(val.x) | is.infinite(val.x)]<-(.5)
                                          dim(val.x) <- c(data.shape,16,3,ncol(val.x))
                                          
                                          list(data=mx.nd.array(val.x))
                                        },
                                        
                                        iter.next=function(){
                                          .self$iter$iter.next()
                                        },
                                        reset=function(){
                                          .self$iter$reset()
                                        },
                                        num.pad=function(){
                                          .self$iter$num.pad()
                                        },
                                        finalize=function(){
                                          .self$iter$finalize()
                                        }
                                      )
  )
  attach(what=.finFindREnv,name = ".finFindREnv")
}
