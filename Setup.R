checkPackages <- try({library("imager");library("shiny");library("DT");library("Rcpp")})

if(class(checkPackages) == "try-error")
{
  install.packages(c("Rcpp","imager","shiny","DT"))
  library("imager")
  library("shiny")
  library("DT")
  library("Rcpp")
}
  
# mxCheck <- try(require("mxnet"))
# if(!mxCheck)
# {
#   cran <- getOption("repos")
#   cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
#   options(repos = cran)
#   install.packages("mxnet")
#   require(mxnet)
# }
load("noiseData.Rdata")
load("finDist.Rdata")
mxnetModel = mx.model.load('residualHashNet', 1)


print("compiling/loading c++ code...")

source('IsolateCurve/extractFeatures.R')
Rcpp::sourceCpp('IsolateCurve/cExif.cpp')
Rcpp::sourceCpp('IsolateCurve/findStart.cpp')
Rcpp::sourceCpp('IsolateCurve/astar.cpp')
Rcpp::sourceCpp('IsolateCurve/imageToFeatureProcessing.cpp')
print("ready")

