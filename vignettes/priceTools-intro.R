## ----setup, message=FALSE, echo=FALSE------------------------------------
library(knitr)
# This is necessary to direct knitr to find the 
# 'data', and other directories that contain
# files needed to execute this document
# thanks to http://stackoverflow.com/a/24585750/1036500
opts_knit$set(root.dir=normalizePath('../'))

## ------------------------------------------------------------------------
library(priceTools)

## ------------------------------------------------------------------------
price.data <- data(biomass)

