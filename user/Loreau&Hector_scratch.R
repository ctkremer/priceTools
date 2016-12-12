
##########################################

#  Code for calculating complementarity and selection metrics of Hector and Loreau

##########################################


# data set:

# Monoculture abundance:
mono<-data.frame(sps=c('A','B','C','D','E'),func=1+runif(5))

# Mixed community species:
mixed<-data.frame(sps=c('A','B','C','D','E'),init.func=1/5,func=1+runif(5,min=0.2))


# add check for species w/out monoculture data:

N<-nrow(mixed)
M<-mono$func
YO.vec<-mixed$func        # observed species function in mixed community
YO.tot<-sum(YO.vec)       # observed total function in mixed community
YE.vec<-mixed$init.func*M # expected species yield in mixed community
YE.tot<-sum(YE.vec)       # total expected function in mixed community
RYO.vec<-YO.vec/M         # observed yield relative to monoculture yield
RYE.vec<-YE.vec/M         # expected yield relative to monoculture (apparently initial relative abundance?)
delta.Y<-YO.tot-YE.tot
delta.RY<-RYO.vec-RYE.vec

cov2<-function(x,y) mean((x-mean(x))*(y-mean(y)))

CE<-N*mean(delta.RY)*mean(M)
SE<-N*cov2(delta.RY,M)

CE+SE
delta.Y

comp.sel<-function(data,even.seeding=FALSE){

  # add check for species w/out monoculture data:
  if(!("init.mixed" %in% names(data)) & even.seeding){
    data$init.mixed <- 1/nrow(data)
  }

  # Error checking
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
  
  CE<-N*mean(delta.RY)*mean(M)
  SE<-N*cov2(delta.RY,M)
  
  return(data.frame(CE,SE))
}



# Format 1:
set.seed(1)
d1<-data.frame(sps=c('A','B','C','D','E'),mono=1+runif(5),init.mixed=1/5,mixed=1+runif(5,min=0.2))

comp.sel(d1)

# Format 2:
set.seed(1)
d2<-data.frame(sps=c('A','B','C','D','E'),mono=1+runif(5),mixed=1+runif(5,min=0.2))

comp.sel(d2,even.seeding = T)

