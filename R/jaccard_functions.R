

###############  Jaccard Functions ##################

### These functions support automated calculations of Jaccard similarity for all pairs of communities 

# NOTE: there is almost certainly a faster way to do this using the vegdist() function inside the vegan package - this approach produces results that are correctly formatted for comparison with the price partition components, however, which counts for something...

#####################################################


#' Calculate the Jaccard index for all possible community pairs
#' 
#' Given a grouped data set containing a species ID column and a column of species'
#' ecosystem functions, this function returns a dataset of Jaccard indices that results
#' from all pairwise combinations of unique communities as defined by the 
#' grouping variable(s).
#' 
#' @param x  A grouped data set, with grouping variables defined as in dplyr operations
#' @param species The name of the column in \code{x} containing species ID's
#' @param func The name of the column in \code{x} containing species' ecosystem function
#' 
#' @return This function returns a data set of the Jaccard indices
#'  corresponding to pairs of communities, identified by one or more grouping variables,
#'  which are provided in pairs of columns with the format: groupvar1.x groupvar1.y, etc.
#'  These can be conveniently re-combined using the \code{group.columns()} command.
#' 
#' @examples 
#' 
#' # write example
#' 
#' @export
#' @import tidyr
pairwise.jaccard<-function(x,species='Species',func='Function'){
  gps<-groups(x)  # extract grouping variables
  
  # standardize user-specified species and function columns
  names(x)[which(names(x)==species)]<-"species"
  names(x)[which(names(x)==func)]<-"func"
  
  if(!(length(gps)>=1)){
    print("ERROR! data provided to pairwise.price must have at least one identified 
          grouping variable")
    break;
  }else{
    
    # apply the jaccard.column function across sets of ref. comms in x
    res <- x %>% do(tmp=jaccard.column(.$species,.$func,dat=x))  

    # distinguish grouping column names of refs. from comparison comms.
    names(res)[1:length(gps)] <- paste(names(res[1:length(gps)]),"x",sep=".") 

    # expand the tibble returned by do()
    res<-tidyr::unnest(ungroup(res))
    
    # fix labels of comparison community's grouping variables
    locs<-which(names(res) %in% gps)
    names(res)[locs]<-paste(names(res)[locs],'y',sep='.')
    
    res<-rename(res,jaccard=vd..1..)
    
    return(res)
  }
}




#' Wrapper function for calculating Jaccard index for a list of communities
#' 
#' Given a list of species names and their ecosystem functions, this function generates
#' a reference community, and then compares the reference community against a set of
#' other communities (including species and their ecosystem function) supplied in a
#' separate, grouped data frame. This is a low-level function that invokes 
#' \code{jaccard.single()} and is called by higher-level functions such as 
#' \code{pairwise.jaccard()}, which automates the pairwise comparison of many communities.
#' 
#' @param sps  A vector of species' names for the reference community
#' @param func A numerical vector of species' ecosystem functions in the reference
#'  community
#' @param dat A grouped data frame of species' names and ecosystem functions, which 
#'  must contain at least one grouping variable, as created by dplyr.
#' 
#' @return This function returns a data set of Jaccard indices for each community 
#' (defined by the grouping variables) compared against the reference community
#' 
#' @examples 
#' 
#' # write example
#'
jaccard.column<-function(sps,func,dat){
  gps<-groups(dat)      # snag the grouping variables
  ngroups<-length(gps)  # how many are there?
  
  tmpX<-data.frame(sps,func) # define reference community
  
  # calculate all price comparisons against reference community.
  options(dplyr.show_progress=F)  # turn off progress bar for low-level do() command
  
  # calculate price components
  res<-dat %>% group_by_(.dots=gps) %>% do(jaccard.single(.$species,.$func,tmpX))  
  
  options(dplyr.show_progress=T)  # turn progress bar back on (so it's visible for high-level do command)
  
  res
}


#' Low-level wrapper function for calculating Jaccard index similarity between a pair of communities
#' 
#' Given a list of species names and their functions, and a reference community,
#' calculate the Jaccard index and return it. This is a  low-level function used inside of 
#' higher-level functions (ie, \code{pairwise.jaccard()}) that automate the pairwise comparison 
#' of many communities. Note that if a comparison fails, NA is returned instead. So far this only 
#' seems to happen when comparisons are made between two communities each containing the same, 
#' single species.
#' 
#' @param sps  A vector of species' names
#' @param func A numerical vector of species' functions
#' @param commX A reference community
#' 
#' @return This function returns a matrix with a single row, and a column with the Jaccard index.
#' 
#' @examples 
#' 
#' # write example
#' 
#' @import reshape2
jaccard.single<-function(sps,func,commX){
  commY<-data.frame(sps,func)         # set up comparison community
  commX$ID<-'x'
  commY$ID<-'y'
  comm<-rbind(commX,commY)
  
  comm<-reshape2::dcast(comm,ID~sps,value.var='func',fill = 0)
  
  if(ncol(comm)==2 & comm[1,2]==comm[2,2]){
    vd<-list(0) # if communities have 1 species, and are identical, J = 0
  }else{
    vd<-try(vegdist(comm[,-1],"jaccard"))
  }
  
  if(class(vd)=='try-error'){
    vd<-list(NA)
  }
  
  return(data.frame(vd[[1]]))
}