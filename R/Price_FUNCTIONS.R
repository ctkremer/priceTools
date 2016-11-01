### Load required packages:
#require(reshape2)
#require(MASS)
#require(ggplot2)
#require(dplyr)
#require(vegan)
#require(gridExtra)

# Handle the import of packages - this should force dplyr to be added to NAMESPACE
# and be loaded when priceTools loads


######################## Basic Tools ########################


#' Logistic link function.
#'
#' @param x A number
#' @return Transformed values on the probability scale
#' @examples
#' expit(0)
#' expit(1)
expit<-function(x){
	exp(x)/(1+exp(x))
}


#' Asymptotic 95 percent confidence interval.
#'
#' This function computes the upper or lower 95 percent CI based on an asymptotic 
#' approximation and assumption of normality.
#'
#' @param x  A list of numeric values
#' 
#' @param type Upper ("upr") or lower ("lwr") CI boundary
#' 
#' @return Confidence interval value
#' 
#' @examples
#' 
#' x<-runif(20)
#' 
#' meanCI(x,type="lwr")
#' meanCI(x,type="upr")
#' 
meanCI<-function(x,type){
	
  if(type=="lwr"){
	  res <- mean(x) - 1.96*(sqrt(var(x))/length(x))
  }
  
  if(type=="upr"){
    res <- mean(x) + 1.96*(sqrt(var(x))/length(x))    
  }
  
  return(res)
}


#' Standard error.
#'
#' Short-cut for calculating the standard error of a vector of values
#'
#' @param x A list of numeric values
#' @return Standard error
#' @examples
#' x<-runif(29)
#' se(x)
se<-function(x) sd(x)/length(x)



######################## Data Cleaning & Setup Functions ########################


### Goal, specify 1 or more ordered variable
#   - reduce it to pre/post 0/1 categories, given one or more cut-points
clean.time.vars<-function(x,col,cut.point){
  
  tmp<-x[,grepl(col,names(x))] > cut.point
  tmp[,1]<-as.numeric(tmp[,1])
  tmp[,2]<-as.numeric(tmp[,2])
  tmp<-as.data.frame(tmp)
  
  x[,grepl(col,names(x))]<-tmp
  return(x)
}


#' Data setup for Price Equation calculations.
#'
#' Take two separate community lists and create a single data set, which is ready
#' for calculating the Price Equation partitions.
#'
#' @param input  Data on the composition of a pair of communities. This can take on two different forms: 
#'    (i) a single data frame, where the first column is 'species', a list of taxa names, and the 
#'        following two columns list the function of each species in communities X and Y. Species absent 
#'        from one community are given an entry of 0 function.
#'    (ii) a list of two separate data frames, one for each community X and Y, each containing two 
#'        columns, and the corresponding function of each species.
#' @param aggregate A rule for aggregating multiple entries for the same species. 
#' The default option is "sum", a secondary option is "mean"
#' 
#' @return A data frame of species identities and functions, appropriately formatted for running
#'  price partition calculations.
#'    \item{species}{A list of species names}
#'    \item{func.X}{The function of each species in communityX}
#'    \item{func.Y}{The function of each species in community Y}
#'    \item{wvec}{An indicator variable equal to 1 when a species occurs in both X and Y}
#'    \item{xvec}{An indicator variable equal to 1 when a species occurs in X}
#'    \item{yvec}{An indicator variable equal to 1 when a species occurs in Y}
#'    
#' @examples 
#' 
#' # Method 1:
#' data.setup(list(biomass))
#' 
#' # Method 2:
#' comX <- biomass[biomass$biomassX!=0,c('species','biomassX')]
#' comY <- biomass[biomass$biomassY!=0,c('species','biomassY')]
#' 
#' data.setup(list(comX,comY))
#' 
#' @import dplyr
data.setup<-function(input,aggregate="sum"){

  if(length(input)==1){
    comm<-input[[1]]
    names(comm)<-c('species','func.x','func.y')

    # add together multiple entries for the same species by community X and Y
    switch(aggregate,
           sum = {comm <- comm %>% group_by(species) %>%
                              summarize(func.x=sum(func.x),func.y=sum(func.y))},
           mean = {comm <- comm %>% group_by(species) %>%
                              summarize(func.x=mean(func.x),func.y=mean(func.y))}
           )

    # Flag species that occur in common between communities x and y
    comm$wvec<-ifelse(comm[,2]>0&comm[,3]>0,1,0)  # species occurs in x and y
    comm$xvec<-ifelse(comm[,2]>0,1,0)  		# species occurs in x
    comm$yvec<-ifelse(comm[,3]>0,1,0)			# species occurs in y
    comm<-comm[order(comm$xvec,comm$yvec,decreasing=T),]
	for(i in 1:ncol(comm)){
      comm[,i]<-ifelse(is.na(comm[,i]),0,comm[,i])
    }
  }

  if(length(input)==2){
    xcomm<-input[[1]]
    ycomm<-input[[2]]

    names(xcomm)<-c('species','func')
    names(ycomm)<-c('species','func')

    # Data formatting
    xcomm$site<-'func.x'
    ycomm$site<-'func.y'
    tmp0<-rbind(xcomm,ycomm)			# stick community data together

    # take this long-form data into wide-form, adding together repeated species entries.

    switch(aggregate,
           sum = {
             comm <- reshape2::dcast(tmp0,species~site,value.var="func",
                                     fun.aggregate = sum)
             },
           mean = {
             comm <- reshape2::dcast(tmp0,species~site,value.var="func",
                                     fun.aggregate = mean)
             }
    )

    # Flag species that occur in common between communities x and y
    comm$wvec<-ifelse(comm$func.x>0&comm$func.y>0,1,0)	# species occurs in x and y
    comm$xvec<-ifelse(comm$func.x>0,1,0)			# species occurs in x
    comm$yvec<-ifelse(comm$func.y>0,1,0)			# species occurs in y
    comm<-comm[order(comm$xvec,comm$yvec,decreasing=T),]

    for(i in 2:ncol(comm)){
      comm[,i]<-ifelse(is.na(comm[,i]),0,comm[,i])
    }
  }

  return(comm)
}


#' Rough estimate of memory requirements of distance matrix
#'
#' Calculate the amount of memory needed to store a distance matrix, before calculating the
#' distance matrix. This is a prudent way to prevent running out of RAM when calculating a
#' distance matrix. (Code implemented from response on R help list).
#'
#' @param n This is the number of rows in the distance matrix
#' 
#' @return Estimated memory requirement in GB
#' 
#' @examples
#' dist.mat.size(200)
dist.mat.size<-function(n){
  ((n*(n-1)/2)*8)/1024^3
}




#' Calculate Price component based distance matrix between community pairs.
#' 
#' The Price equation generates a vector of values reflecting a comparison between two 
#' communities. This vector can be used to establish a distance between different pairs 
#' of communities, compared with the Price equation. Across all pairwise combinations of
#' community pairs, this produces a distance matrix, which can be used to perform multivariate
#' tests on Price analyses.
#' 
#' @param x  A data frame, resulting from FUNCTION
#' 
#' @return This function returns a list of two distance matrices:
#'  \item{dst5}{A distance matrix calculated based on the full 5-part Price partition}
#'  \item{dst3}{A distance matrix calculated based on the 3-part sCAFE version of the Price partition}
#' 
#' @examples 
#' 
#' # write example
#' 
get.dist.mats<-function(x){
  
  # Check the estimated size of the requested distance matrix.
  # If >1 GB, consult user before continuing.
  size<-dist.mat.size(nrow(x))
  yn<-"y"
  if(size>1){
    cat("\n","Caution! Distance matrix size exceeds 1 GB.","\n","Continue? y/n")
    yn<-scan(n=1,what=character())
  }
  
  if(yn=="n"){
    print("get.dist.mats aborted")
    return(NA)
  }else{
    # 5-part partition
    dst5<-as.matrix(dist(x[,c('SRE.L','SRE.G','SCE.L','SCE.G','CDE')]))
    
    # for 3-part partition
    dst3<-as.matrix(dist(x[,c('SL','SG','CDE')]))
    
    comp.data<-list(covars=x,dist5=dst5,dist3=dst3)
    return(comp.data)
  }
}


#' Merge pairs of grouping columns
#' 
#' Take a pair of columns, distinguished with suffix .x and .y, and merge them into a
#' single column without suffixes. This is helpful for post-processing the output of
#' pairwise price calculations.
#' 
#' @param x  A data frame
#' @param gps A list of grouping column(s)
#' @param drop Drop original, unpaired columns after grouping? TRUE/FALSE
#' 
#' @return This function returns a data frame
#' 
#' @examples 
#' 
#' # write example
#' 
group.columns<-function(x,gps,drop=F){

  for(i in 1:length(gps)){
    tmp<-as.data.frame(x[,grepl(gps[i],names(x))])

    # this is the column to stash in place of the .x and .y columns
    new.var<-paste(as.character(tmp[,1]),as.character(tmp[,2]))

    # drop old columns, if requested:
    if(drop){
      x<-x[,!grepl(gps[i],names(x))]
    }

    # add new column
    x<-data.frame(new.var,x)

    # update name of new column
    names(x)[1]<-gps[i]
  }

  return(x)
}



######################## Price Partition Functions ########################

#' Calculate the Price equation partition for two communities
#'
#' Take a (formatted) list of species and functions for two communities, and calculate
#' the Price equation partition comparing the communities.
#'
#' Extra thoughts on how the interpret the Price equation partitions, which may get relocated into a vignette. 
#' 
#' Comments on SCE.L. If a species x' from x is lost in y, SCE.L will increase if x' 
#' is less productive on average, and SCE.L will decrease if x' is more productive 
#' than average. If a species x' from x is NOT lost in y, SCE.L will increase if x' 
#' is more productive on average, and SCE.L will decrease if x' is less productive 
#' than average. Overall, high/positive values of SCE.L mean that weak species were 
#' lost and good species were retained. Noteably, SCE.L will not be affected by new
#' species that y gains relative to what is shared or lost. Average species have 
#' little effect on the value of SCE.L. If either barely any or almost all species 
#' occur in common between communities x and y, then the few species that are kept 
#' (or lost) will have a particularly large influence on the value of SCE.L. Overall, 
#' SCE.L will probably be smaller in this case, and greatly affected by whether the
#' species lost/gained are more or less productive.
#' 
#' Comments on SCE.G. A less productive than average species in y (-1) makes a negative
#' contribution to SCE.G if it is NOT in community x, and a positive contribution to 
#' SCE.G if it is in community x. A more productive than average species in y (+1) 
#' makes a negative contribution to SCE.G if it is in x, and a positive contribution 
#' to SCE.G if it does NOT occur in community x. A positive SCE.G occurs when less
#' productive than average members of y also occured in x, and more productive than
#' average species in y do not occur in x.
#'
#' @param comm  A data frame formatted according to the \code{\link{data.setup()}}
#'  function, or if formatting independently, a data frame with the following columns.
#' 
#' @param quiet Silence error messages? TRUE/FALSE
#' 
#' @param sps.level Provide species-level contributions to 5 Price components? TRUE/FALSE
#' 
#' @return If \code{sps.level=FALSE}, a data frame of Price equation components.
#'    \item{SRE.L}{species richness effect (loss of species)}
#'    \item{SRE.G}{species richness effect (gain of species)}
#'    \item{SCE.L}{species composition effect (loss of species)}
#'    \item{SCE.G}{species composition effect (gain of species)}
#'    \item{CDE}{context dependent effect}
#'    \item{SL}{sum of SRE.L and SCE.L}
#'    \item{SG}{sum of SRE.G and SCE.G}
#'    \item{SR}{sum of SRE.L and SRE.G}
#'    \item{CE}{sum of SCE.G, SCE.L, and CDE}
#'    \item{x.func}{Total function in community X}
#'    \item{y.func}{Total function in community Y}
#'    \item{x.rich}{Number of species in community X}
#'    \item{y.rich}{Number of species in community Y}
#'    \item{c.rich}{Number of shared species between X and Y}
#'
#' @return If \code{sps.level=TRUE}, a list containing the above information in the first slot, and a data frame of individual species' contributions to each Price component in the second slot.
#'        
#' @examples 
#' 
#' formatted.data<-data.setup(list(biomass))
#' price.part(formatted.data) 
#' 
price.part<-function(comm,quiet=F,sps.level=F){

  # Combined species list
  sps.list <- comm$species
  sx <- sum(comm$xvec)		# number of species in x
  sy <- sum(comm$yvec)		# number of species in y
  sc <- sum(comm$wvec)		# number of species in both
  if(sc < 1 & quiet==F){
    print('Caution! Communities share no species in common.')
    #		break
  }

  # Measures of ecosystem function
  totx<-sum(comm$func.x)
  toty<-sum(comm$func.y)

  # Partition change in function
  zbarx <- totx/sx						# average function per species in x
  zbary <- toty/sy						# average function per species in y
  wbarx <- mean(comm$wvec[comm$xvec==1])		# probability that species in x is also in y
  wbary <- mean(comm$wvec[comm$yvec==1])		# probability that species in y is also in x

  ### Solve for components:

  # Difference between shared diversity & x diversity, times average function of x
  SRE.L.list <- ((sc-sx)/sx)*comm$func.x
  SRE.L.list<-data.frame(species=comm$species,SRE.L.list)
  SRE.L<-sum(SRE.L.list[,2])

  # Difference between diversity of y & shared diversity times average function of y
  SRE.G.list <- ((sy-sc)/sy)*comm$func.y
  SRE.G.list <- data.frame(species=comm$species,SRE.G.list)
  SRE.G <- sum(SRE.G.list[,2])

  # SCE.L computation
  SCE.L.list <- (comm$func.x[comm$xvec==1]-zbarx)*(comm$wvec[comm$xvec==1]-wbarx)
  SCE.L.list <- data.frame(species=comm$species[comm$xvec==1],SCE.L.list)
  SCE.L <- sum(SCE.L.list[,2])

  # SCE.G computation
  SCE.G.list <- -1*(comm$func.y[comm$yvec==1]-zbary)*(comm$wvec[comm$yvec==1]-wbary)
  SCE.G.list<- data.frame(species=comm$species[comm$yvec==1],SCE.G.list)
  SCE.G<- sum(SCE.G.list[,2])

  # Total change in function due to changes in function of species shared by communities.
  CDE.list <- comm$func.y[comm$wvec==1]-comm$func.x[comm$wvec==1]
  CDE.list <- data.frame(species=comm$species[comm$wvec==1],CDE.list)
  CDE <- sum(CDE.list[,2])

  # combine pieces:
  pp.list <- merge(SRE.G.list,SRE.L.list,all=T)
  pp.list <- merge(pp.list,SCE.L.list,all.x = T)
  pp.list <- merge(pp.list,SCE.G.list,all.x = T)
  pp.list <- merge(pp.list,CDE.list,all.x = T)
  
  # additional diagnostic output:
  SL <- SRE.L+SCE.L
  SG <- SRE.G+SCE.G
  SR <- SRE.L+SRE.G
  CE <- SCE.L+SCE.G+CDE
  x.func <- totx
  y.func <- toty
  x.rich <- sx
  y.rich <- sy
  c.rich <- sc
  
  # structure output:
  pp <- c(SRE.L,SRE.G,SCE.L,SCE.G,CDE,SL,SG,SR,CE,x.func,y.func,x.rich,y.rich,c.rich)
  names(pp) <- c("SRE.L","SRE.G","SCE.L","SCE.G","CDE",
                  "SL","SG","SR","CE","x.func","y.func","x.rich","y.rich","c.rich")
  if(sps.level){
    res <- list(pp,pp.list)
  }else{
    res <- pp
  }
  
  return(res)
}


#########

### The next 3 functions support automated calculations of price components for
# all pairwise community comparisons



#' Low-level wrapper function for applying Price partition to a pair of communities
#' 
#' Given a list of species names and their functions, and a reference community,
#' calculate the full set of Price partition components and return them. This is a 
#' low-level function used inside of higher-level functions (ie, \code{price.part.all()})
#' that automate the pairwise comparison of many communities.
#' 
#' @param sps  A vector of species' names
#' @param func A numerical vector of species' functions
#' @param commX A reference or 'baseline' community
#' 
#' @return This function returns a matrix with a single row, and columns consisting of 
#' Price equation components.
#' 
#' @examples 
#' 
#' # write example
#' 
price.part.single<-function(sps,func,commX){
  commY<-data.frame(sps,func)         # set up comparison community
  ds<-data.setup(list(commX,commY))   # set up community lists for price calculations
  data.frame(t(price.part(ds,quiet=T)))   # run price calculation
}


#' Wrapper function for applying Price partition to list of communities
#' 
#' Given a list of species names and their ecosystem functions, this function generates
#' a reference community, and then compares the reference community against a set of
#' other communities (including species and their ecosystem function) supplied in a
#' separate, grouped data frame. This is a low-level function that invokes 
#' \code{price.part.single()} and is called by higher-level functions such as 
#' \code{pairwise.price()}, which automates the pairwise comparison of many communities.
#' 
#' @param sps  A vector of species' names for the reference community
#' @param func A numerical vector of species' ecosystem functions in the reference
#'  community
#' @param dat A grouped data frame of species' names and ecosystem functions, which 
#'  must contain at least one grouping variable, as created by dplyr.
#' 
#' @return This function returns a data set of Price equation components, one full set
#'  for each community (defined by the grouping variables) compared against the 
#'  reference community
#' 
#' @examples 
#' 
#' # write example
#'
price.part.column<-function(sps,func,dat){
  gps<-groups(dat)      # snag the grouping variable(s)
  ngroups<-length(gps)  # how many are there?
  
  tmpX<-data.frame(sps,func) # define reference community
  
  # turn off progress bar for low-level do() command
  options(dplyr.show_progress=F)  
  
  # calculate price components, given reference community
  res <- dat %>% group_by_(.dots=gps) %>% do(price.part.single(.$species,.$func,tmpX))
  
  # turn progress bar back on (so it's visible for high-level do command)
  options(dplyr.show_progress=T)  
  
  # rename grouping variable columns to distinguish comparison communities.
  names(res)[1:ngroups] <- paste(names(res[1:ngroups]),"y",sep=".")
  
  return(res)
}


#' Calculate the Price equation partition for all possible community pairs
#' 
#' Given a grouped data set containing a species ID column and a column of species'
#' ecosystem functions, this function returns a dataset of Price components that results
#' from comparing all pairwise combinations of unique communities as defined by the 
#' grouping variable(s).
#' 
#' @param x  A grouped data set, with grouping variables defined as in dplyr operations
#' @param species The name of the column in \code{x} containing species ID's
#' @param func The name of the column in \code{x} containing species' ecosystem function
#' 
#' @return This function returns a data set of the Price equation components 
#'  corresponding to pairs of communities, identified by one or more grouping variables,
#'  which are provided in pairs of columns with the format: groupvar1.x groupvar1.y, etc.
#'  These can be conveniently re-combined using the \code{group.columns()} command.
#' 
#' @examples 
#' 
#' # write example
#' 
pairwise.price<-function(x,species='Species',func='Function'){
  gps <- groups(x)  # extract grouping variables

  # standardize user-specified species and function columns
  names(x)[which(names(x)==species)] <- "species"
  names(x)[which(names(x)==func)] <- "func"

  if(!(length(gps) >= 1)){
    print("ERROR! data provided to pairwise.price must have at least one identified
          grouping variable")
    break;
  }else{
    # apply the price.part.column function across sets of ref. comms in x
    res <- x %>% do(price.part.column(.$species,.$func,dat=x))  
    
    # distinguish grouping column names of refs. from comparison comms.
    names(res)[1:length(gps)] <- paste(names(res[1:length(gps)]),"x",sep=".") 

    # drops self-comparisons:
    res <- ungroup(res)
    res <- res %>% filter((SRE.L!=0 | SRE.G!=0 | SCE.L!=0 | SCE.G!=0 | CDE!=0))

    return(res)
  }
}







###############  Jaccard Functions ##################


### The next 3 functions support automated calculations of Jaccard similarity for
# all pairwise community comparisons

# NOTE: there is almost certainly a faster way to do this using the vegdist() function inside the vegan package - this approach produces results that are correctly formatted for comparison with the price partition components, however, which counts for something...

# Given a grouped data set with a species id column (specified as species), and
# a function column (specified as func).
# this function returns the jaccard index for all pairwise combinations
# of the lowest levels of the grouping variables.
pairwise.jaccard<-function(x,species='Species',func='Function'){
  gps<-groups(x)  # extract grouping variables

  # standardize user-specified species and function columns
  names(x)[which(names(x)==species)]<-"species"
  names(x)[which(names(x)==func)]<-"func"

  if(!(length(gps)>=1)){
    print("ERROR! data provided to pairwise.price must have at least one identified grouping variable")
    break;
  }else{
    res<-x %>% do(jaccard.column(.$species,.$func,dat=x))  # apply the price.part.column function across sets of ref. comms in x
    names(res)[1:length(gps)]<-paste(names(res[1:length(gps)]),"x",sep=".") # distinguish grouping column names of refs. from comparison comms.

    # drops self-comparisons:
    res<-ungroup(res)

    return(res)
  }
}

jaccard.column<-function(sps,func,dat){
  gps<-groups(dat)      # snag the grouping variables
  ngroups<-length(gps)  # how many are there?

  tmpX<-data.frame(sps,func) # define reference community

  # calculate all price comparisons against reference community.
  options(dplyr.show_progress=F)  # turn off progress bar for low-level do() command
  bob<-dat %>% group_by_(.dots=gps) %>% do(jaccard.single(.$species,.$func,tmpX))  # calculate price components
  options(dplyr.show_progress=T)  # turn progress bar back on (so it's visible for high-level do command)

  # rename grouping variable columns to distinguish comparison communities.
  names(bob)[1:ngroups]<-paste(names(bob[1:ngroups]),"y",sep=".")
  bob
}


# Calculates the price components produced by comparing fixed community commX
# with the community commY corresponding to the list of species and functions given by sps and func,
# which can be flexibly/iteratively supplied by the dplyr do() command inside price.part.all()
jaccard.single<-function(sps,func,commX){
  commY<-data.frame(sps,func)         # set up comparison community
  commX$ID<-'x'
  commY$ID<-'y'
  comm<-rbind(commX,commY)

  comm<-reshape2::dcast(comm,ID~sps,value.var='func',fill = 0)
  vd<-vegdist(comm[,-1],"jaccard")

  return(data.frame(vd[[1]]))
}




