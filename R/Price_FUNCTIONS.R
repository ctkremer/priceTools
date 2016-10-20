### Load required packages:
require(reshape2)
require(MASS)
require(ggplot2)
require(dplyr)
require(vegan)
require(gridExtra)



######################## Basic Tools ########################


#' Logistic link function
#'
#' @param x A number
#' @return Transformed values on the probability scale
#' @examples
#' expit(0)
#' expit(1)
expit<-function(x){
	exp(x)/(1+exp(x))
}


#' Asymptotic 95% confidence interval
#'
#' @param x  A list of numeric values
#' @param type Upper ("upr") or lower ("lwr") CI boundary?
#' @return Confidence interval value
#' @examples
#' x<-runif(20)
#' mean.CI(x,type="lwr")
#' mean.CI(x,type="upr")
#' 
mean.CI<-function(x,type){
	
  if(type=="lwr"){
	  res <- mean(x) - 1.96*(sqrt(var(x))/length(x))
  }
  
  if(type=="upr"){
    res <- mean(x) + 1.96*(sqrt(var(x))/length(x))    
  }
  
  return(res)
}


#' Standard error
#'
#' @param x A list of numeric values
#' @return Standard error
#' @examples
#' x<-runif(29)
#' se(x)
se<-function(x) sd(x)/length(x)



######################## Data Cleaning & Setup Functions ########################

###### Take 2 separate community lists and create single data set ready for calculating the price partition.

#' Data setup for Price Equation calculations
#'
#' Take two separate community lists and create a single data set, which is ready
#' for calculating the Price Equation partitions.
#'
#' @param input  A list of species names
#' @param aggregate A rule for aggregating multiple entries for the same species. 
#' The default option is "sum"
#' @return comm  A data frame containing columns:
#' @examples 
#' #data.setup()
#' 
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
           sum = {comm<-dcast(tmp0,species~site,value.var="func",fun.aggregate = sum)},
           mean = {comm<-dcast(tmp0,species~site,value.var="func",fun.aggregate = mean)}
    )

    # Flag species that occur in common between communities x and y
    comm$wvec<-ifelse(comm$func.x>0&comm$func.y>0,1,0)	# species occurs in x and y
    comm$xvec<-ifelse(comm$func.x>0,1,0)			# species occurs in x
    comm$yvec<-ifelse(comm$func.y>0,1,0)			# species occurs in y
    comm<-comm[order(comm$xvec,comm$yvec,decreasing=T),]

    for(i in 1:ncol(comm)){
      comm[,i]<-ifelse(is.na(comm[,i]),0,comm[,i])
    }
  }

  return(comm)
}


# write a function that merges pairs of grouping variable columns...
# to run after price partitioning
# NOTE: x must be a data object containing the grouping column(s) listed in gps
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



# Crude estimate of distance matrix size: (stolen from R help list)
#  Or simpler speaking, you need to calculate 365000 * (365000-1) / 2 =
#  66612317500 distances and with 8 bytes each, hence you need 66612317500
#* 8 = 532898540000 Bytes = 532898540000 / (1024)^3 GB ~= 496.3 Gb to
#store it in memory.
dist.mat.size<-function(n){
  ((n*(n-1)/2)*8)/1024^3
}

### ADD: 2-part partition (BEF style)

# Function for calculating 5 and 3-part Price partitions
# given a properly formatted data frame x.
get.dist.mats<-function(x){

  # check estimated size of distance matrix, if >1 gb, consult user before continuing.
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


######################## Price Partition Function ########################


### Input

# Either the output of data.setup() OR
# Community data set having the following columns:

# species (being a numeric or text ID for a unique species)
# func.x (a column of data on the function of each species in community X; 0 indicates no data due to a species not occuring in X)
# func.y (same as above, but for species in community Y)
# xvec (1 if species occurs in community X, otherwise 0)
# yvec (1 if species occurs in community Y, otherwise 0)
# wvec = xvec*yvec (or, 1 if species occurs in X and Y, otherwise 0)

# there should be n rows in this data set, one for each unique species in the combined species list for communities X and Y.

### Output
#
#	These are the basic components of the price partition
#	- SRE.L,		species richness effect (loss of species)
#	- SRE.G,		species richness effect (gain of species)
#	- SCE.L,		species composition effect (loss of species)
#	- SCE.G,		species composition effect (gain of species)
#	- CDE,		context dependent effect
#



price.part<-function(comm,quiet=F){

	# Combined species list
	sps.list<-comm$species
	sx<-sum(comm$xvec)		# number of species in x
	sy<-sum(comm$yvec)		# number of species in y
	sc<-sum(comm$wvec)		# number of species in both
	if(sc<1 & quiet==F){
		print('Caution! Communities share no species in common.')
#		break
	}

	# Measures of ecosystem function
	totx<-sum(comm$func.x)
	toty<-sum(comm$func.y)

	# Partition change in function
	zbarx<-totx/sx						# average function per species in x
	zbary<-toty/sy						# average function per species in y
	wbarx<-mean(comm$wvec[comm$xvec==1])		# probability that species in x is also in y
	wbary<-mean(comm$wvec[comm$yvec==1])		# probability that species in y is also in x

	### Solve for components:

	# Difference btwn shared diversity & x diversity, times average function of x
	SRE.L<- (sc-sx)*zbarx

	# Difference btwn diversity of y & shared diversity times average function of y
	SRE.G<- (sy-sc)*zbary

	# SCE.L reflects combined effects of both the non-random loss of species from x and the non-random retention of species in y.
	SCE.L<- sum((comm$func.x[comm$xvec==1]-zbarx)*(comm$wvec[comm$xvec==1]-wbarx))

	# if a species x' from x is lost in y, SCE.L will increase if x' is less productive on average, and SCE.L will decrease if x' is more productive than average.
	# if a species x' from x is NOT lost in y, SCE.L will increase if x' is more productive on average, and SCE.L will decrease if x' is less productive than average.
	# Overall, high/positive values of SCE.L mean that weak species were lost and good species were retained.
	# Noteably, SCE.L will not be affected by new species that y gains relative to what is shared or lost.
	# average species have little effect on the value of SCE.L.

	# If either barely any or almost all species occur in common between communities x and y, then the few species that are kept (or lost) will have a particularly large influence on the value of SCE.L. Overall, SCE.L will probably be smaller in this case, and greatly affected by whether the species lost/gained are more or less productive.


	SCE.G<- -sum((comm$func.y[comm$yvec==1]-zbary)*(comm$wvec[comm$yvec==1]-wbary))

# SCE.G reflects combined effects of both the non-random gain of species by community y and the non-random retention of species from x.

# a less productive than average species in y (-1) makes a negative contribution to SCE.G if it is NOT in community x, and a positive contribution to SCE.G if it is in community x.

# a more productive than average species in y (+1) makes a negative contribution to SCE.G if it is in x, and a positive contribution to SCE.G if it does NOT occur in community x.

# A positive SCE.G occurs when less productive than average members of y also occured in x, and more productive than average species in y do not occur in x


	# Total change in function due to changes in function of sps shared by communities.
	CDE<-sum(comm$func.y[comm$wvec==1]-comm$func.x[comm$wvec==1])

	# additional diagnostic output:
	SL<-SRE.L+SCE.L
	SG<-SRE.G+SCE.G
	SR<-SRE.L+SRE.G
	CE<-SCE.L+SCE.G+CDE
	x.func<-totx
	y.func<-toty
	x.rich<-sx
	y.rich<-sy
	c.rich<-sc

	# structure output:
	pp<-c(SRE.L,SRE.G,SCE.L,SCE.G,CDE,SL,SG,SR,CE,x.func,y.func,x.rich,y.rich,c.rich)
	names(pp)<-c("SRE.L","SRE.G","SCE.L","SCE.G","CDE",
	             "SL","SG","SR","CE","x.func","y.func","x.rich","y.rich","c.rich")

	return(pp)
}


### Try to intuit and explain the SCE.L and SCE.G terms:

# SCE.L:

#	Mathematically, consider the sum of:
#	(biomass of species in x MINUS mean biomass of all x species)
# 	* (whether or not particular species in x also occurs in y MINUS probability a generic species x occurs in y)

# Basically, determine how important particular species x' is in community x and weight it by how unexpected the presence of x' in y is. Then add up all of these comparisons across all x' in x.
# Or, take a measure of how good a particular species is, and how likely it is to be shared between communities.

# All possible contributions of a species:
# a less productive than average species in x (-1) makes a positive contribution to SCE.L if it is NOT in community y, and a negative contribution to SCE.L if it does occur in community y.
# a more productive than average species in x (+1) makes a positive contribution to SCE.L if it occurs in y, and a negative contribution to SCE.L if it does not occur.

# If SCE.L is positive as a whole, then community y must have 'lost' species in x that underperform and 'gained' species from x that overperformed.


# A positive SCE.G occurs when less productive than average members of y also occured in x, and more productive than average species in y do not occur in x


# SCE.G:

# a less productive than average species in x (-1) makes a positive contribution to SCE.L if it is NOT in community y, and a negative contribution to SCE.L if it does occur in community y.
# a more productive than average species in x (+1) makes a positive contribution to SCE.L if it occurs in y, and a negative contribution to SCE.L if it does not occur.
# If SCE.L is positive as a whole, than community y must have 'lost' species in x that underperform and 'gained' species from x that overperformed.



##################

## Extended version of price partition function, that outputs contributions of individual species to each component.

price.part2<-function(comm,quiet=F){

  # Combined species list
  sps.list<-comm$species
  sx<-sum(comm$xvec)		# number of species in x
  sy<-sum(comm$yvec)		# number of species in y
  sc<-sum(comm$wvec)		# number of species in both
  if(sc<1 & quiet==F){
    print('Caution! Communities share no species in common.')
    #		break
  }

  # Measures of ecosystem function
  totx<-sum(comm$func.x)
  toty<-sum(comm$func.y)

  # Partition change in function
  zbarx<-totx/sx						# average function per species in x
  zbary<-toty/sy						# average function per species in y
  wbarx<-mean(comm$wvec[comm$xvec==1])		# probability that species in x is also in y
  wbary<-mean(comm$wvec[comm$yvec==1])		# probability that species in y is also in x

  ### Solve for components:

  # Difference btwn shared diversity & x diversity, times average function of x
  SRE.L.list <- ((sc-sx)/sx)*comm$func.x
  SRE.L.list<-data.frame(species=comm$species,SRE.L.list)
  SRE.L<-sum(SRE.L.list[,2])

  # Difference btwn diversity of y & shared diversity times average function of y
  SRE.G.list <- ((sy-sc)/sy)*comm$func.y
  SRE.G.list<-data.frame(species=comm$species,SRE.G.list)
  SRE.G<-sum(SRE.G.list[,2])

  # SCE.L reflects combined effects of both the non-random loss of species from x and the non-random retention of species in y.
  SCE.L.list <- (comm$func.x[comm$xvec==1]-zbarx)*(comm$wvec[comm$xvec==1]-wbarx)
  SCE.L.list<- data.frame(species=comm$species[comm$xvec==1],SCE.L.list)
  SCE.L <- sum(SCE.L.list[,2])

  # if a species x' from x is lost in y, SCE.L will increase if x' is less productive on average, and SCE.L will decrease if x' is more productive than average.
  # if a species x' from x is NOT lost in y, SCE.L will increase if x' is more productive on average, and SCE.L will decrease if x' is less productive than average.
  # Overall, high/positive values of SCE.L mean that weak species were lost and good species were retained.
  # Noteably, SCE.L will not be affected by new species that y gains relative to what is shared or lost.
  # average species have little effect on the value of SCE.L.

  # If either barely any or almost all species occur in common between communities x and y, then the few species that are kept (or lost) will have a particularly large influence on the value of SCE.L. Overall, SCE.L will probably be smaller in this case, and greatly affected by whether the species lost/gained are more or less productive.
  SCE.G.list <- -1*(comm$func.y[comm$yvec==1]-zbary)*(comm$wvec[comm$yvec==1]-wbary)
  SCE.G.list<- data.frame(species=comm$species[comm$yvec==1],SCE.G.list)
  SCE.G<- sum(SCE.G.list[,2])

  # SCE.G reflects combined effects of both the non-random gain of species by community y and the non-random retention of species from x.
  # a less productive than average species in y (-1) makes a negative contribution to SCE.G if it is NOT in community x, and a positive contribution to SCE.G if it is in community x.
  # a more productive than average species in y (+1) makes a negative contribution to SCE.G if it is in x, and a positive contribution to SCE.G if it does NOT occur in community x.
  # A positive SCE.G occurs when less productive than average members of y also occured in x, and more productive than average species in y do not occur in x

  # Total change in function due to changes in function of sps shared by communities.
  CDE.list <- comm$func.y[comm$wvec==1]-comm$func.x[comm$wvec==1]
  CDE.list <- data.frame(species=comm$species[comm$wvec==1],CDE.list)
  CDE<-sum(CDE.list[,2])

  # combine pieces:
  pp.list<-merge(SRE.G.list,SRE.L.list,all=T)
  pp.list<-merge(pp.list,SCE.L.list,all.x = T)
  pp.list<-merge(pp.list,SCE.G.list,all.x = T)
  pp.list<-merge(pp.list,CDE.list,all.x = T)
  pp<-c(SRE.L,SRE.G,SCE.L,SCE.G,CDE)
  names(pp)<-c("SRE.L","SRE.G","SCE.L","SCE.G","CDE")

  return(list(pp,pp.list))
}


#########

### The next 3 functions support automated calculations of price components for
# all pairwise community comparisons

# Given a grouped data set with a species id column (specified as species), and
# a function column (specified as func).
# this function returns a set of price components for all pairwise combinations
# of the lowest levels of the grouping variables.
pairwise.price<-function(x,species='Species',func='Function'){
  gps<-groups(x)  # extract grouping variables

  # standardize user-specified species and function columns
  names(x)[which(names(x)==species)]<-"species"
  names(x)[which(names(x)==func)]<-"func"

  if(!(length(gps)>=1)){
    print("ERROR! data provided to pairwise.price must have at least one identified grouping variable")
    break;
  }else{
    res<-x %>% do(price.part.column(.$species,.$func,dat=x))  # apply the price.part.column function across sets of ref. comms in x
    names(res)[1:length(gps)]<-paste(names(res[1:length(gps)]),"x",sep=".") # distinguish grouping column names of refs. from comparison comms.

    # drops self-comparisons:
    res<-ungroup(res)
    res<- res %>% filter((SRE.L!=0 | SRE.G!=0 | SCE.L!=0 | SCE.G!=0 | CDE!=0))

    return(res)
  }
}

# This function produces a data frame of all price component values generated by comparing a reference community
# (with species list sps and function values func)
# against all possible alternative communities in the grouped data set dat,
price.part.column<-function(sps,func,dat){
  gps<-groups(dat)      # snag the grouping variables
  ngroups<-length(gps)  # how many are there?

  tmpX<-data.frame(sps,func) # define reference community

  # calculate all price comparisons against reference community.
  options(dplyr.show_progress=F)  # turn off progress bar for low-level do() command
  bob<-dat %>% group_by_(.dots=gps) %>% do(price.part.single(.$species,.$func,tmpX))  # calculate price components
  options(dplyr.show_progress=T)  # turn progress bar back on (so it's visible for high-level do command)

  # rename grouping variable columns to distinguish comparison communities.
  names(bob)[1:ngroups]<-paste(names(bob[1:ngroups]),"y",sep=".")
  bob
}


# Calculates the price components produced by comparing fixed community commX
# with the community commY corresponding to the list of species and functions given by sps and func,
# which can be flexibly/iteratively supplied by the dplyr do() command inside price.part.all()
price.part.single<-function(sps,func,commX){
  commY<-data.frame(sps,func)         # set up comparison community
  ds<-data.setup(list(commX,commY))   # set up community lists for price calculations
  data.frame(t(price.part(ds,quiet=T)))   # run price calculation
}


###############


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

  comm<-dcast(comm,ID~sps,value.var='func',fill = 0)
  vd<-vegdist(comm[,-1],"jaccard")

  return(data.frame(vd[[1]]))
}




