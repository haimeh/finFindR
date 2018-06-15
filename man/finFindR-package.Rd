\name{finFindR-package}
\alias{finFindR-package}
\alias{finFindR}
\docType{package}
\title{
  Dolphin Recognition via Automated Cropping Tracing and Matching
}
\description{
finFindR offers the capacity to automatically crop and trace images for recognition work.
Strictly speaking, the package exports 3 key functions that can be called from R directly:\cr
\code{cropFins} \cr
\code{constrainSizeFinImage} \cr
\code{traceFromImage} \cr
These isolate a dorsal fin, resize the resulting cropped image for optimal feature extraction, and generate the input for the recognition neural network (mxnet framework).
}
\details{
Inside the package is a shiny app which uses these functions to provide the user with matches.\cr
The Full app precompiled for windows, is available at https://github.com/haimeh/finFindR/releases \cr
Raw images taken in the field are cropped to isolate dolphins in the images as recognized via neural network. Cropped images can be further processed using the app to generate the edge tracings for each image in a catalogue. A trained network then takes these edge traces and generates an embedding representing the distinct features for all individuals in each catalogue.\cr
Using the finFindR app, catalogues can be matched using a sorting algorithm. The distances between every pairing of individuals between the two catalogues are calculated. This is used to sort individuals by proximity of features. The app returns for the user; a table with each row representing an image in a query catalogue and its top 50 matches from the reference catalogue.
}
\author{
Jaime Thompson
<https://github.com/haimeh/finFindR/>

Maintainer: Jaime Thompson <jwthompson@west-inc.com>
}

