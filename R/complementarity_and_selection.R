
##########################################

#  Code for calculating complementarity and selection metrics of Hector and Loreau

##########################################


#' Calculate covariance
#'
#' @param x A vector
#' @param y Another vector
#' @return Covariance between x and y
#' @examples
#' x<-runif(10)
#' y<-runif(10)
#' cov2(x,y)
#' 
cov2<-function(x,y) mean((x-mean(x))*(y-mean(y)))


#' Calculate estimates of Complementarity and Selection
#'
#' Calculate the complementarity and selection effects (Loreau & Hector 2001) given data on
#' the abundance of species grown in mono- and poly-culture.
#'
#' @param data This is a data frame containing columns 'mono' (species' monoculture abundance), 'mixed' (species polyculture abundance), and 'init.mixed' (initial relative abundance of species in polyculture)
#' @param even.seeding If no 'init.mixed' column is provided, and even.seeding is set to TRUE, equal relative abundances are inferred within the function
#' 
#' @return CE the complementarity effect
#' @return SE the selection effect
#' 
#' @examples
#' 
#' Format 1:
#' set.seed(1)
#' d1<-data.frame(sps=c('A','B','C','D','E'),mono=1+runif(5),init.mixed=1/5,mixed=1+runif(5,min=0.2))
#' 
#' comp.sel(d1)
#' 
#' # Format 2:
#' set.seed(1)
#' d2<-data.frame(sps=c('A','B','C','D','E'),mono=1+runif(5),mixed=1+runif(5,min=0.2))
#' 
#' comp.sel(d2,even.seeding = T)
#' 
#' @export
comp.sel<-function(data,even.seeding=FALSE){

  # add check for species w/out monoculture data:
  if(!("init.mixed" %in% names(data)) & even.seeding){
    data$init.mixed <- 1/nrow(data)
  }

  # error checking
  if(sum(data$init.mixed)!=1){
    print("Error! Initial relative abundances in mixed community do not sum to 1")
  }
  
  N<-nrow(data)
  M<-data$mono
  YO.vec<-data$mixed        # observed species function in mixed community
  YO.tot<-sum(YO.vec)       # observed total function in mixed community
  YE.vec<-data$init.mixed*M # expected species yield in mixed community
  YE.tot<-sum(YE.vec)       # total expected function in mixed community
  RYO.vec<-YO.vec/M         # observed yield relative to monoculture yield
  RYE.vec<-YE.vec/M         # expected yield relative to monoculture 
  delta.Y<-YO.tot-YE.tot
  delta.RY<-RYO.vec-RYE.vec
  
  # calculate terms
  CE<-N*mean(delta.RY)*mean(M)
  SE<-N*cov2(delta.RY,M)
  
  return(data.frame(CE,SE))
}

