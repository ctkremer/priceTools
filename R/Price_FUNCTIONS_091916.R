### Load required packages:
require(reshape2)
require(MASS)
require(ggplot2)
require(dplyr)
require(vegan)
require(gridExtra)

# graphing functions edited by KBM to:
# move legend to bottom of plots for better stacking (leap.zig)
# change standardization=FALSE method (process.data)
# plot absolute EF on x-axis instead of raw change (process.data and leap.zig)
# remove gridlines from plots (leap.zig)
# last updated 08-04-2016

# Wishlist:
# make aspect ratio of plots 1; i.e. square

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
#' @return Confidence interval value
#' @examples
#' x<-runif(20)
#' lwCImean(x)
#'
lwCImean<-function(x){
	mean(x)-1.96*(sqrt(var(x))/length(x))
}
upCImean<-function(x){
	mean(x)+1.96*(sqrt(var(x))/length(x))
}

#' Standard error
#'
#' @param x  A list of numeric values
#' @return Standard error
#' @examples
#' x<-runif(29)
#' se(x)
se<-function(x) sd(x)/length(x)

#ci.se<-function(x) 1.96*(sd(x)/length(x))


######################## Data Cleaning & Setup Functions ########################

###### Take 2 separate community lists and create single data set ready for calculating the price partition.

# 2/4/16 - adding ability to check for duplicate species entries and aggregate them.
#     --> currently this leads to repeated species entries being summed
#     --> could cause issues if sampling design is unbalanced.

#' Data setup for Price Equation calculations
#'
#' Take two separate community lists and create a single data set, which is ready
#' for calculating the Price Equation partitions.
#'
#' @param input  A list of species names
#' @return comm  A data frame containing columns:
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

######################## Price Simulation Code ########################

### Function for simulating community data to analyze with price partition

draw.sps<-function(nsp,fnc,ids,beta){
  #fitness.vals<-expit((beta)*(fnc-mean(fnc)))
  fitness.vals<-expit((beta/sd(fnc))*(fnc-mean(fnc)))
  weights<- fitness.vals/sum(fitness.vals)

  #sample(fnc,nsp,prob=weights)
  sample(ids,nsp,prob=weights)
}

price.sim<-function(pars){

  # set pars:
  x.mn.sp<-pars[1]
  x.sd.sp<-pars[2]
  n.x<-pars[3]
  y.mn.sp<-pars[4]
  y.sd.sp<-pars[5]
  lambda.L<-pars[6]
  lambda.G<-pars[7]
  beta.L<-pars[8]
  beta.G<-pars[9]
  chng.mn<-pars[10]
  chng.sd<-pars[11]

  # (1) Draw x community
  x.func<-rnorm(n.x,mean=x.mn.sp,sd=x.sd.sp)
  x.id<-seq(1,n.x)

  # (2) Determine number of sps lost and gained
  x.loss<-rpois(1,lambda=lambda.L)
  x.loss<-ifelse(x.loss > n.x, n.x, x.loss)	# protect against killing more species than we have. (maybe use a binomial distribution instead? avoids this risk...)
  x.gain<-rpois(1,lambda=lambda.G)

  # (3) Figure out which sps are lost
  x.sps.lost<-draw.sps(nsp=x.loss,fnc=x.func,
                       ids=x.id,beta=beta.L)	# draw ids
  x.func.new<-x.func[-x.sps.lost]			# save old functions for retained sps
  x.id.new<-x.id[-x.sps.lost]				# save ids for retained sps

  # (4) Figure out change in function
  x.func.new<-x.func.new + rnorm(length(x.id.new),mean=chng.mn,sd=chng.sd)

  # (5) Figure out which sps are gained
  y.func.pool<-rnorm(n=1000,mean=y.mn.sp,sd=y.sd.sp)	# simulate species pool
  x.sps.gained<-draw.sps(nsp=x.gain,fnc=y.func.pool,
                         ids=seq(1,length(y.func.pool)),beta=beta.G)	# draw sps id

  x.func.new<-c(x.func.new,y.func.pool[x.sps.gained])
  if(x.gain>0){
    x.id.new<-c(x.id.new,n.x+seq(1,x.gain))
  }

  # (6) Assemble the data frame
  comm<-merge(data.frame(id=x.id,x.func),
              data.frame(id=x.id.new,x.func.new),all=T)
  names(comm)<-c('id','x.func','y.func')


  ## Run Price partition:
  comm2<-data.setup(list(comm))
  res<-price.part(comm2,quiet=T)

  return(res)
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

### CHANGE LOG

#	- changing missing species convention. They should be represented by 0 abundance entries, rather than NA codes.

#	1/19/15 - The above change may be problematic if function values of 0 are meaningful.... although it keeps the math tidy.

#	11/17/15 - turned off 'break' command for occurrence of non-overlapping communities. And added an error message/verbosity flag.

# 3/4/16 - added additional output columns tracking aggregations of price components, total function, and richness values

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


######################## Vector Plotting ########################


### Process single treatment's worth of data for plotting CAFE components:
process.data.cafe<-function(data,group.vars=NULL,standardize=TRUE){

  ### standardize
  if(standardize==TRUE){
    comps<-c("SRE.L","SRE.G","SCE.L","SCE.G","CDE","SL","SG","SR","CE")
    data[,comps]<-100*data[,comps]/data$x.func                  # X function scaled
    data$y.func<-100*(data$y.func - data$x.func)/data$x.func    # Y function scaled
    data$x.func<-0 # X function set as baseline.
    # net SG change:
    data$SG <- data$SL+data$SG
  } else{
    data$x.func <- data$x.func
    data$y.func <- data$y.func
    data$SL <- data$x.func + data$SL
    data$SG <- data$SL + data$SG
  }

  # CAFE component    richness    function
  # base = c(x.rich,x.func)
  # SL = c(c.rich,SL)
  # SG = c(y.rich,SL+SG)
  # CDE = c(y.rich,y.func)

  p2 <- melt(data[,c(group.vars,'x.func','SL','SG','y.func','x.rich','c.rich','y.rich')],
             id.vars=c(group.vars,'x.rich','c.rich','y.rich'))

  # add richness column:
  p2$rich<-ifelse(p2$variable=="x.func",p2$x.rich,
                  ifelse(p2$variable=="SL",p2$c.rich,
                         ifelse(p2$variable=="SG",p2$y.rich,
                                ifelse(p2$variable=="y.func",p2$y.rich,NA))))

  # summarize raw points:
  p3b<- p2 %>% group_by(variable) %>% summarise(mean.y=mean(value),
                                                y.qt.lw=quantile(value,probs=0.025),
                                                y.qt.up=quantile(value,probs=0.975),
                                                mean.x=mean(rich),
                                                x.qt.lw=quantile(rich,probs=0.025),
                                                x.qt.up=quantile(rich,probs=0.975))

  # stagger rows:
  p3b2<-p3b
  nms<-p3b2$variable[2:4]
  p3b2<-p3b2[1:3,]
  p3b2$variable<-nms
  p4<-rbind(p3b,p3b2)
  p4<-p4[p4$variable!="x.func",]

  # Organize factor levels for plotting:
  p2$variable<-factor(p2$variable,levels=c("x.func","SL","SG","y.func"),labels=c("baseline","SL","SG","comparison"))
  p2$variable<-factor(p2$variable,levels=c("baseline","SL","SG","comparison","SL vector","SG vector","CDE vector"))
  p3b$variable<-factor(p3b$variable,levels=c("x.func","SL","SG","y.func"),labels=c("baseline","SL","SG","comparison"))
  p3b$variable<-factor(p3b$variable,levels=c("baseline","SL","SG","comparison","SL vector","SG vector","CDE vector"))
  p4$variable<-factor(p4$variable,levels=c("SL","SG","y.func"),labels=c("SL vector","SG vector","CDE vector"))
  p4$variable<-factor(p4$variable,levels=c("baseline","SL","SG","comparison","SL vector","SG vector","CDE vector"))

  p3b<-p3b[p3b$variable!="baseline",]

  return(list(p2,p3b,p4))
}


### Process single treatment's worth of data for plotting BEF components:
process.data.bef<-function(data,group.vars=NULL,standardize=TRUE){

  ### standardize
  if(standardize==TRUE){
    comps<-c("SRE.L","SRE.G","SCE.L","SCE.G","CDE","SL","SG","SR","CE")
    data[,comps]<-100*data[,comps]/data$x.func                  # X function scaled
    data$y.func<-100*(data$y.func - data$x.func)/data$x.func    # Y function scaled
    data$x.func<-0     # X function set as baseline.
  } else{
    data$x.func <- data$x.func
    data$y.func <- data$y.func
    data$SR <- data$x.func + data$SR
  }

  # BEF component
  # base = c(x.rich,x.func)
  # SR = c(x.rich,x.func + SRE.L + SRE.G)
  # CE = c(y.rich,y.func) = c(y.rich, x.func + SRE.L + SRE.G + SCEs + CDE)

  p2 <- melt(data[,c(group.vars,'x.func','SR','y.func','x.rich','c.rich','y.rich')],
             id.vars=c(group.vars,'x.rich','c.rich','y.rich'))

  # add composite richness column:
  p2$rich<-ifelse(p2$variable=="x.func",p2$x.rich,
                  ifelse(p2$variable=="SR",p2$y.rich,
                         ifelse(p2$variable=="y.func",p2$y.rich,NA)))#)

  # summarize raw points:
  p3b<- p2 %>% group_by(variable) %>% summarise(mean.y=mean(value),
                                                y.qt.lw=quantile(value,probs=0.025),
                                                y.qt.up=quantile(value,probs=0.975),
                                                mean.x=mean(rich),
                                                x.qt.lw=quantile(rich,probs=0.025),
                                                x.qt.up=quantile(rich,probs=0.975))

  # stagger rows:
  p3b2<-p3b
  nms<-p3b2$variable[2:3]
  p3b2<-p3b2[1:2,]
  p3b2$variable<-nms
  p4<-rbind(p3b,p3b2)
  p4<-p4[p4$variable!="x.func",]

  # Organize factor levels for plotting:
  p2$variable<-factor(p2$variable,levels=c("x.func","SR","y.func"),labels=c("baseline","SR","comparison"))
  p2$variable<-factor(p2$variable,levels=c("baseline","SR","comparison","SR vector","CE vector"))
  p3b$variable<-factor(p3b$variable,levels=c("x.func","SR","y.func"),labels=c("baseline","SR","comparison"))
  p3b$variable<-factor(p3b$variable,levels=c("baseline","SR","comparison","SR vector","CE vector"))
  #p4$variable<-factor(p4$variable,levels=c("SR","CE","y.func"),labels=c("SR vector","CE vector"))
  p4$variable<-factor(p4$variable,levels=c("SR","y.func"),labels=c("SR vector","CE vector"))
  p4$variable<-factor(p4$variable,levels=c("baseline","SR","comparison","SR vector","CE vector"))

  p3b<-p3b[p3b$variable!="baseline",]

  return(list(p2,p3b,p4))
}


### Process single treatment's worth of data for plotting Price components:
process.data.price<-function(data,group.vars=NULL,standardize=TRUE){

  ### standardize
  if(standardize==TRUE){
    comps<-c("SRE.L","SRE.G","SCE.L","SCE.G","CDE")
    data[,comps]<-100*data[,comps]/data$x.func                  # X function scaled
    data$y.func<-100*(data$y.func - data$x.func)/data$x.func    # Y function scaled
    data$x.func<-0     # X function set as baseline.
    # create net change vector
    data$SCE.L <- data$SRE.L + data$SCE.L
    data$SRE.G <- data$SCE.L + data$SRE.G
    data$SCE.G <- data$SRE.G + data$SCE.G
  } else{
    data$x.func <- data$x.func
    data$y.func <- data$y.func
    data$SRE.L <- data$x.func + data$SRE.L
    data$SCE.L <- data$SRE.L + data$SCE.L
    data$SRE.G <- data$SCE.L + data$SRE.G
    data$SCE.G <- data$SRE.G + data$SCE.G
  }

  # base = c(x.rich,x.func)
  # SRE.L = c(c.rich,0 + SRE.L)
  # SCE.L = c(c.rich,0 + SRE.L + SCE.L = SL)
  # SRE.G = c(y.rich,0 + SRE.L + SCE.L + SRE.G = SL + SRE.G)
  # SCE.G = c(y.rich,0 + SRE.L + SCE.L + SRE.G + SCE.G = SL + SG)
  # CDE = c(y.rich,y.func)

  p2 <- melt(data[,c(group.vars,'x.func','SRE.L','SCE.L','SRE.G','SCE.G','y.func','x.rich','c.rich','y.rich')],
             id.vars=c(group.vars,'x.rich','c.rich','y.rich'))

  # add richness column:
  p2$rich<-ifelse(p2$variable=="x.func",p2$x.rich,
                  ifelse(p2$variable=="SRE.L",p2$c.rich,
                         ifelse(p2$variable=="SCE.L",p2$c.rich,
                                ifelse(p2$variable=="SRE.G",p2$y.rich,
                                       ifelse(p2$variable=="SCE.G",p2$y.rich,
                                            ifelse(p2$variable=="y.func",p2$y.rich,NA))))))

  # summarize raw points:
  p3b<- p2 %>% group_by(variable) %>% summarise(mean.y=mean(value),
                                                y.qt.lw=quantile(value,probs=0.025),
                                                y.qt.up=quantile(value,probs=0.975),
                                                mean.x=mean(rich),
                                                x.qt.lw=quantile(rich,probs=0.025),
                                                x.qt.up=quantile(rich,probs=0.975))

  # stagger rows:
  p3b2<-p3b
  nms<-p3b2$variable[2:6]
  p3b2<-p3b2[1:5,]
  p3b2$variable<-nms
  p4<-rbind(p3b,p3b2)
  p4<-p4[p4$variable!="x.func",]

  # Organize factor levels for plotting:
  p2$variable<-factor(p2$variable,levels=c("x.func","SRE.L","SCE.L","SRE.G","SCE.G","y.func"),
                                  labels=c("baseline","SRE.L","SCE.L","SRE.G","SCE.G","comparison"))
  p2$variable<-factor(p2$variable,levels=c("baseline","SRE.L","SCE.L","SRE.G","SCE.G","comparison",
                                              "SRE.L vector","SCE.L vector","SRE.G vector","SCE.G vector","CDE vector"))
  p3b$variable<-factor(p3b$variable,levels=c("x.func","SRE.L","SCE.L","SRE.G","SCE.G","y.func"),
                                    labels=c("baseline","SRE.L","SCE.L","SRE.G","SCE.G","comparison"))
  p3b$variable<-factor(p3b$variable,levels=c("baseline","SRE.L","SCE.L","SRE.G","SCE.G","comparison",
                                              "SRE.L vector","SCE.L vector","SRE.G vector","SCE.G vector","CDE vector"))
  p4$variable<-factor(p4$variable,levels=c("SRE.L","SCE.L","SRE.G","SCE.G","y.func"),
                                  labels=c("SRE.L vector","SCE.L vector","SRE.G vector","SCE.G vector","CDE vector"))
  p4$variable<-factor(p4$variable,levels=c("baseline","SRE.L","SCE.L","SRE.G","SCE.G","comparison",
                                           "SRE.L vector","SCE.L vector","SRE.G vector","SCE.G vector","CDE vector"))

  p3b<-p3b[p3b$variable!="baseline",]

  return(list(p2,p3b,p4))
}


# Wrapper plotting function for different kinds of vector plots (BEF, CAFE, BOTH):
leap.zig<-function(data,type="cafe",group.vars=NULL,standardize=TRUE,...){

  ## Process data
  switch(type,
         cafe={
           tmp<-process.data.cafe(data,group.vars,standardize)
           leap.zig.cafe(tmp, loc.standardize=standardize, ...)
         },
         bef={
           tmp<-process.data.bef(data,group.vars,standardize)
           leap.zig.bef(tmp, loc.standardize=standardize, ...)
         },
         both={
           tmp<-process.data.cafe(data,group.vars,standardize)
           tmp.bef<-process.data.bef(data,group.vars,standardize)

           tmp[[1]]<-unique(rbind(tmp[[1]],tmp.bef[[1]]))
           tmp[[2]]<-unique(rbind(tmp[[2]],tmp.bef[[2]]))
           tmp[[3]]<-unique(rbind(tmp[[3]],tmp.bef[[3]]))

           tmp[[1]]$variable<-factor(tmp[[1]]$variable,levels=c("baseline","SL","SG","SR","comparison","SL vector","SG vector","CDE vector","SR vector","CE vector"))

           leap.zig.both(tmp, loc.standardize=standardize, ...)
         },
         price={
           tmp<-process.data.price(data,group.vars,standardize)

           leap.zig.price(tmp, loc.standardize=standardize, ...)
         },
         "Error! Invalid plot method in leap.zig()"
  )
}



leap.zig.both<-function(tmp,xlim=NA,ylim=NA,loc.standardize=TRUE,
                        error.bars=FALSE,raw.points=TRUE,
                        vectors=TRUE,all.vectors=FALSE,
                        legend=TRUE,old.plot=NA,main="",linetype=1,add=FALSE){

  # With this kind of set up, could run process.data.cafe repeatedly,
  # for different treatment combinations.

  # trim out un-needed factor levels
  if(raw.points==FALSE & vectors==TRUE){
    tmp[[3]]$variable<-factor(as.character(tmp[[3]]$variable),levels=c("SL vector","SG vector","CDE vector","SR vector","CE vector"))
  }

  ## Plot it:
  if(add==TRUE){
    b8<-old.plot
  }else{
    b8<-ggplot()+geom_hline(yintercept=0,linetype=2)
  }

  # Add points
  if(raw.points){
    b8<-b8+geom_point(data=tmp[[1]],aes(colour=variable,x=rich,y=value))
  }

  # Add error bars.
  # - in future, add jittering to third set of error bars?
  if(error.bars){
    b8<-b8+geom_errorbarh(data=tmp[[2]],aes(xmin=x.qt.lw,xmax=x.qt.up,x=mean.x,y=mean.y),
                          colour='gray',linetype=linetype)+
      geom_errorbar(data=tmp[[2]],aes(ymin=y.qt.lw,ymax=y.qt.up,x=mean.x,y=mean.y),
                    width=1,colour='gray',linetype=linetype)
  }

  # Add vectors
  if(vectors){
    b8<-b8+geom_path(data=tmp[[3]],aes(colour=variable,x=mean.x,y=mean.y),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"),linetype=linetype)
  }

  if(all.vectors){
    b8<-b8+geom_path(data=tmp[[1]][tmp[[1]]$variable!='baseline',],
                     aes(colour=variable,x=rich,y=value),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"),linetype=linetype)
  }

  cols<-c(alpha('#d95f02'),alpha('#1b9e77'),alpha('#7570b3'),alpha('#7fcdbb'),alpha('#e34a33'))
  trcols<-c(alpha('#d95f02',0.1),alpha('#1b9e77',0.1),alpha('#7fcdbb',0.1),alpha('#7570b3',0.1))

  if(raw.points==FALSE & vectors==TRUE){
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,values=cols)+
      guides(colour=guide_legend(nrow=3,override.aes=list(shape=c(NA,NA,NA,NA,NA),linetype=c(1,1,1,1,1))))
  }else{
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,
                              values=c('black',trcols,cols))+
      guides(colour=guide_legend(nrow=3,override.aes=list(shape=c(19,1,1,1,1,NA,NA,NA,NA,NA),
                                                   linetype=c(0,0,0,0,0,1,1,1,1,1),
                                                   colour=c('black',cols[1:4],cols))))
  }


  # figure out plot ranges, based on data ranges:
  xlim.D<-range(tmp[[1]]$rich)*c(0.98,1.02)
  ylim.D<-range(tmp[[1]]$value)*c(0.98,1.02)

  # if user supplied xlim's
  if(length(xlim)==2){
    xlim.outer<-c(min(xlim.D[1],xlim[1]),max(xlim.D[2],xlim[2]))
    xlim.inner<-xlim
  }else{
    xlim.outer<-xlim.inner<-xlim.D
  }

  # if user supplied ylim's
  if(length(ylim)==2){
    ylim.outer<-c(min(ylim.D[1],ylim[1]),max(ylim.D[2],ylim[2]))
    ylim.inner<-ylim
  }else{
    ylim.outer<-ylim.inner<-ylim.D
  }

  # adjust inner plotting range
  b8<-b8+coord_cartesian(xlim=xlim.inner,ylim=ylim.inner)

  # Select color and label options:


  b8<-b8+theme_bw()+
    scale_x_continuous("Species richness",limits=xlim.outer)

  if(loc.standardize==TRUE){
    b8<-b8+scale_y_continuous("% change in EF vs. baseline",limits=ylim.outer)
  }else{
    b8<-b8+scale_y_continuous("Ecosystem function",limits=ylim.outer)
  }

  if(legend==TRUE){
    b8<-b8+theme(legend.position="bottom",legend.title=element_blank())
  }

  if(legend==FALSE){
    b8<-b8+theme(legend.position="none")
  }

  b8<-b8+ggtitle(main)

  #  print(b8)
  return(b8)
}



leap.zig.bef<-function(tmp,xlim=NA,ylim=NA,loc.standardize=TRUE,
                       error.bars=FALSE,raw.points=TRUE,
                       vectors=TRUE,all.vectors=FALSE,
                       legend=TRUE,old.plot=NA,main="",linetype=1,add=FALSE){

  # With this kind of set up, could run process.data.bef repeatedly,
  # for different pairs of treatment combinations.

  # trim out un-needed factor levels
  if(raw.points==FALSE & vectors==TRUE){
    tmp[[3]]$variable<-factor(as.character(tmp[[3]]$variable),levels=c("SR vector","CE vector"))
  }

  ## Plot it:
  if(add==TRUE){
    b8<-old.plot
  }else{
    b8<-ggplot()
  }

  # Add points
  if(raw.points){
    b8<-b8+geom_point(data=tmp[[1]],aes(colour=variable,x=rich,y=value))
  }

  # Add error bars.
  # - in future, add jittering to third set of error bars?
  if(error.bars){
    b8<-b8+geom_errorbarh(data=tmp[[2]],aes(xmin=x.qt.lw,xmax=x.qt.up,x=mean.x,y=mean.y),
                          colour='gray',linetype=linetype)+
      geom_errorbar(data=tmp[[2]],aes(ymin=y.qt.lw,ymax=y.qt.up,x=mean.x,y=mean.y),
                    width=1,colour='gray',linetype=linetype)
  }

  # Add vectors
  if(vectors){
    b8<-b8+geom_path(data=tmp[[3]],aes(colour=variable,x=mean.x,y=mean.y),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"),linetype=linetype)
  }

  if(all.vectors){
    b8<-b8+geom_path(data=tmp[[1]][tmp[[1]]$variable!='baseline',],
                     aes(colour=variable,x=rich,y=value),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"),linetype=linetype)
  }

  cols<-c(alpha('#7fcdbb'),alpha('#e34a33'))
  trcols<-c(alpha('#7fcdbb',0.1),alpha('#e34a33',0.1))

  if(raw.points==FALSE & vectors==TRUE){
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,values=cols)+
      guides(colour=guide_legend(nrow=3,override.aes=list(shape=c(NA,NA),linetype=c(1,1))))
  }else{
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,values=c('black',trcols,cols))+
      guides(colour=guide_legend(nrow=3,
                              override.aes=list(shape=c(19,1,1,NA,NA),
                                                linetype=c(0,0,0,1,1),
                                                colour=c('black',cols,cols))))
  }

  # figure out plot ranges, based on data ranges:
  xlim.D<-range(tmp[[1]]$rich)*c(0.98,1.02)
  ylim.D<-range(tmp[[1]]$value)*c(0.98,1.02)

  # if user supplied xlim's
  if(length(xlim)==2){
    xlim.outer<-c(min(xlim.D[1],xlim[1]),max(xlim.D[2],xlim[2]))
    xlim.inner<-xlim
  }else{
    xlim.outer<-xlim.inner<-xlim.D
  }

  # if user supplied ylim's
  if(length(ylim)==2){
    ylim.outer<-c(min(ylim.D[1],ylim[1]),max(ylim.D[2],ylim[2]))
    ylim.inner<-ylim
  }else{
    ylim.outer<-ylim.inner<-ylim.D
  }

  # adjust inner plotting range
  b8<-b8+coord_cartesian(xlim=xlim.inner,ylim=ylim.inner)

  # Select color and label options:
  b8<-b8+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    scale_x_continuous("Species richness",limits=xlim.outer)

  if(loc.standardize==TRUE){
    b8<-b8+scale_y_continuous("% change in EF vs. baseline",limits=ylim.outer)+
            geom_hline(yintercept=0,linetype=2)
  }else{
    b8<-b8+scale_y_continuous("Ecosystem function",limits=ylim.outer)
  }

  if(legend==TRUE){
    b8<-b8+theme(legend.position="bottom",legend.title=element_blank())
  }

  if(legend==FALSE){
    b8<-b8+theme(legend.position="none")
  }

  b8<-b8+ggtitle(main)

  #  print(b8)
  return(b8)
}


leap.zig.cafe<-function(tmp,xlim=NA,ylim=NA,loc.standardize=TRUE,
                        error.bars=FALSE,raw.points=TRUE,
                        vectors=TRUE,all.vectors=FALSE,
                        legend=TRUE,old.plot=NA,main="",linetype=1,add=FALSE){

  # With this kind of set up, could run process.data.cafe repeatedly,
  # for different treatment combinations.

  # trim out un-needed factor levels
  if(raw.points==FALSE & vectors==TRUE){
    tmp[[3]]$variable<-factor(as.character(tmp[[3]]$variable),levels=c("SL vector","SG vector","CDE vector"))
  }

  ## Plot it:
  if(add==TRUE){
    b8<-old.plot
  }else{
    b8<-ggplot()
  }

  # Add points
  if(raw.points){
    b8<-b8+geom_point(data=tmp[[1]],aes(colour=variable,x=rich,y=value))
  }

  # Add error bars.
  # - in future, add jittering to third set of error bars?
  if(error.bars){
    b8<-b8+geom_errorbarh(data=tmp[[2]],aes(xmin=x.qt.lw,xmax=x.qt.up,x=mean.x,y=mean.y),
                          colour='gray',linetype=linetype)+
      geom_errorbar(data=tmp[[2]],aes(ymin=y.qt.lw,ymax=y.qt.up,x=mean.x,y=mean.y),
                    width=1,colour='gray',linetype=linetype)
  }

  # Add vectors
  if(vectors){
    b8<-b8+geom_path(data=tmp[[3]],aes(colour=variable,x=mean.x,y=mean.y),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"),linetype=linetype)
  }

  #print(head(tmp[[1]]))

  if(all.vectors){
    b8<-b8+geom_path(data=tmp[[1]],
                     aes(colour=variable,x=rich,y=value,linetype=Plot),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"))
  }

  cols<-c(alpha('#d95f02'),alpha('#1b9e77'),alpha('#7570b3'))
  trcols<-c(alpha('#d95f02',0.1),alpha('#1b9e77',0.1),alpha('#7570b3',0.1))

  if(raw.points==FALSE & vectors==TRUE){
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,values=cols)+
      guides(colour=guide_legend(nrow=3,override.aes=list(shape=c(NA,NA,NA),linetype=c(1,1,1))))
  }else{
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,values=c('black',trcols,cols))+
      guides(colour=guide_legend(nrow=3,
                            override.aes=list(shape=c(19,1,1,1,NA,NA,NA),
                                              linetype=c(0,0,0,0,1,1,1),
                                              colour=c('black',cols,cols))))
  }

  # figure out plot ranges, based on data ranges:
  xlim.D<-range(tmp[[1]]$rich)*c(0.98,1.02)
  ylim.D<-range(tmp[[1]]$value)*c(0.98,1.02)

  # if user supplied xlim's
  if(length(xlim)==2){
    xlim.outer<-c(min(xlim.D[1],xlim[1]),max(xlim.D[2],xlim[2]))
    xlim.inner<-xlim
  }else{
    xlim.outer<-xlim.inner<-xlim.D
  }

  # if user supplied ylim's
  if(length(ylim)==2){
    ylim.outer<-c(min(ylim.D[1],ylim[1]),max(ylim.D[2],ylim[2]))
    ylim.inner<-ylim
  }else{
    ylim.outer<-ylim.inner<-ylim.D
  }

  # adjust inner plotting range
  b8<-b8+coord_cartesian(xlim=xlim.inner,ylim=ylim.inner)

  # Select color and label options:
  b8<-b8+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    scale_x_continuous("Species richness",limits=xlim.outer)

  if(loc.standardize==TRUE){
    b8<-b8+scale_y_continuous("% change in EF vs. baseline",limits=ylim.outer)+
      geom_hline(yintercept=0,linetype=2)
  }else{
    b8<-b8+scale_y_continuous("Ecosystem function",limits=ylim.outer)
  }

  if(legend==TRUE){
    b8<-b8+theme(legend.position="bottom",legend.title=element_blank())
  }

  if(legend==FALSE){
    b8<-b8+theme(legend.position="none")
  }

  b8<-b8+ggtitle(main)

  #  print(b8)
  return(b8)
}



leap.zig.price<-function(tmp,xlim=NA,ylim=NA,loc.standardize=TRUE,
                          error.bars=FALSE,raw.points=TRUE,
                          vectors=TRUE,all.vectors=FALSE,
                          legend=TRUE,old.plot=NA,main="",linetype=1,add=FALSE){

  # With this kind of set up, could run process.data.cafe repeatedly,
  # for different treatment combinations.

  # trim out un-needed factor levels
  if(raw.points==FALSE & vectors==TRUE){
    tmp[[3]]$variable<-factor(as.character(tmp[[3]]$variable),levels=c("SRE.L vector","SCE.L vector","SRE.G vector","SCE.G vector","CDE vector"))
  }

  ## Plot it:
  if(add==TRUE){
    b8<-old.plot
  }else{
    b8<-ggplot()
  }

  # Add points
  if(raw.points){
    b8<-b8+geom_point(data=tmp[[1]],aes(colour=variable,x=rich,y=value))
  }

  # Add error bars.
  # - in future, add jittering to third set of error bars?
  if(error.bars){
    b8<-b8+geom_errorbarh(data=tmp[[2]],aes(xmin=x.qt.lw,xmax=x.qt.up,x=mean.x,y=mean.y),
                          colour='gray',linetype=linetype)+
      geom_errorbar(data=tmp[[2]],aes(ymin=y.qt.lw,ymax=y.qt.up,x=mean.x,y=mean.y),
                    width=1,colour='gray',linetype=linetype)
  }

  # Add vectors
  if(vectors){
    b8<-b8+geom_path(data=tmp[[3]],aes(colour=variable,x=mean.x,y=mean.y),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"),linetype=linetype)
  }

  if(all.vectors){
    b8<-b8+geom_path(data=tmp[[1]][tmp[[1]]$variable!='baseline',],
                     aes(colour=variable,x=rich,y=value),
                     arrow=arrow(length=unit(0.2,"cm"),ends="first"),linetype=linetype)
  }

  cols<-c(alpha('#e41a1c'),alpha('#377eb8'),alpha('#4daf4a'),alpha('#984ea3'),alpha('#ff7f00'))
  trcols<-c(alpha('#e41a1c',0.1),alpha('#377eb8',0.1),alpha('#4daf4a',0.1),alpha('#984ea3',0.1),alpha('#ff7f00',0.1))

  if(raw.points==FALSE & vectors==TRUE){
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,values=cols)+
      guides(colour=guide_legend(nrow=3,override.aes=list(shape=c(NA,NA,NA,NA,NA),linetype=c(1,1,1,1,1))))
  }else{
    b8<-b8+scale_color_manual("Component\n",drop=FALSE,values=c('black',trcols,cols))+
      guides(colour=guide_legend(nrow=3,
                      override.aes=list(shape=c(19,1,1,1,1,1,NA,NA,NA,NA,NA),
                                        linetype=c(0,0,0,0,0,0,1,1,1,1,1),
                                        colour=c('black',cols,cols))))
  }

  # figure out plot ranges, based on data ranges:
  xlim.D<-range(tmp[[1]]$rich)*c(0.98,1.02)
  ylim.D<-range(tmp[[1]]$value)*c(0.98,1.02)

  # if user supplied xlim's
  if(length(xlim)==2){
    xlim.outer<-c(min(xlim.D[1],xlim[1]),max(xlim.D[2],xlim[2]))
    xlim.inner<-xlim
  }else{
    xlim.outer<-xlim.inner<-xlim.D
  }

  # if user supplied ylim's
  if(length(ylim)==2){
    ylim.outer<-c(min(ylim.D[1],ylim[1]),max(ylim.D[2],ylim[2]))
    ylim.inner<-ylim
  }else{
    ylim.outer<-ylim.inner<-ylim.D
  }

  # adjust inner plotting range
  b8<-b8+coord_cartesian(xlim=xlim.inner,ylim=ylim.inner)

  # Select color and label options:
  b8<-b8+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    scale_x_continuous("Species richness",limits=xlim.outer)

  if(loc.standardize==TRUE){
    b8<-b8+scale_y_continuous("% change in EF vs. baseline",limits=ylim.outer)+
            geom_hline(yintercept=0,linetype=2)
  }else{
    b8<-b8+scale_y_continuous("Ecosystem function",limits=ylim.outer)
  }

  if(legend==TRUE){
    b8<-b8+theme(legend.position="bottom",legend.title=element_blank())
  }

  if(legend==FALSE){
    b8<-b8+theme(legend.position="none")
  }

  b8<-b8+ggtitle(main)

  #  print(b8)
  return(b8)
}


######################## Vecotor Summary Stats ########################


# do distributions have different means?
dist.test<-function(x,y){
  data<-data.frame(x=x,y=y)
  md1<-lm(y~x,data=data)
  val<-round(as.data.frame(anova(md1))[1,5],3)
  ifelse(val<0.001,"<0.001",as.character(val))
}

# what is the difference in mean?
delta.mean<-function(x,y){
  data<-data.frame(x=x,y=y)
  lvl<-levels(data$x)
  v1<-data$y[data$x==lvl[1]]
  v2<-data$y[data$x==lvl[2]]
  mean(v2)-mean(v1)
}

# do distributions have different variances?
var.test.2<-function(x,y){
  data<-data.frame(x=x,y=y)
  lvl<-levels(data$x)
  v1<-data$y[data$x==lvl[1]]
  v2<-data$y[data$x==lvl[2]]
  vd1<-var.test(v1,v2)
  val<-round(vd1$p.value,3)
  ifelse(val<0.001,"<0.001",as.character(val))
}

# what is the difference in variance?
delta.vars<-function(x,y){
  data<-data.frame(x=x,y=y)
  lvl<-levels(data$x)
  v1<-data$y[data$x==lvl[1]]
  v2<-data$y[data$x==lvl[2]]
  var(v2)-var(v1)
}

# what is the treatment mean?
trt.mean<-function(x,y,ind){
  data<-data.frame(x=x,y=y)
  lvl<-levels(data$x)
  v2<-data$y[data$x==lvl[2]]
  v1<-data$y[data$x==lvl[1]]
  c(mean(v1),mean(v2))[[ind]]
}


# Given a data set, run series of tests on vector plot components:
# - name of treatment pairs column
# - control-control case
# - price components for pairs of communities

# TO DO - significance tests of slope variables under 'cafe' tests is busted; for now the slope variables are simply excluded.
test.partitions<-function(data,type='both',treat.var,control,standardize=T,print=F,plot=F){

  if(length(control)>2){
    print("Error! test.cafe() only supports comparisons between one control and one treatment case")}
  if(!(treat.var %in% names(data))){ print("Error! Specified data column does not appear.")}

  data<-as.data.frame(data)

  if(standardize){
    comps<-c("SRE.L","SRE.G","SCE.L","SCE.G","CDE","SL","SG","SR","CE")
    data[,comps]<-100*data[,comps]/data$x.func                  # X function scaled
    data$y.func<-100*(data$y.func - data$x.func)/data$x.func    # Y function scaled
    data$x.func<-0     # X function set as baseline.
  }

  # duplicate treatment column, providing standardized name... for later use in dplyr
  data$calcTrt<-data[,treat.var]

  data$s.loss<- -1*(data$x.rich-data$c.rich)
  data$s.gain<-data$y.rich-data$c.rich
  data$s.change<-data$y.rich-data$x.rich

  data$SL.slope <- -1*data$SL/(data$y.rich-data$c.rich)
  data$SG.slope <- data$SG/(data$x.rich-data$c.rich)
#  data$SR.slope <- data$SR/data$s.change

  data$SL.mag <- sqrt(data$SL^2 + data$s.loss^2)
  data$SG.mag <- sqrt(data$SG^2 + data$s.gain^2)
#  data$SR.mag <- sqrt(data$SR^2 + data$s.change^2)

#  data$slope.log.ratio<-log(data$SL.slope/data$SG.slope)

  data$Total<-data$SL+data$SG+data$CDE

  ## Process data
  switch(type,
         cafe={
           m2<-melt(data[,c('calcTrt','s.loss','s.gain',
                            'SL',#'SL.slope','SL.mag',
                            'SG',#'SG.slope','SG.mag',
                            'CDE','Total')],id.vars = 'calcTrt')
         },
         bef={
           m2<-melt(data[,c('calcTrt','s.change',
                            'SR',#'SR.slope',#'SR.mag',
                            'CE','Total')],id.vars = 'calcTrt')
         },
         both={
           m2<-melt(data[,c('calcTrt','s.loss','s.gain','s.change',
                            'SL',#'SL.slope','SL.mag',
                            'SG',#'SG.slope','SG.mag',
                            'CDE',
                            'SR',#'SR.slope','SR.mag',
                            'CE','Total')],id.vars = 'calcTrt')
         },
         price={
           m2<-melt(data[,c('calcTrt','s.loss','s.gain',
                            'SRE.L',#'SRE.L.slope','SRE.L.mag',
                            'SCE.L',#'SCE.L.slope','SCE.L.mag',
                            'SRE.G',#'SRE.G.slope','SRE.G.mag',
                            'SCE.G',#'SCE.G.slope','SCE.G.mag',
                            'CDE','Total')],id.vars = 'calcTrt')
         },
         "Error! Invalid test type in test.partitions()"
  )

  # ensure that baseline pair is the desired control pair
  lvls<-levels(m2$calcTrt)
  m2$calcTrt<-factor(m2$calcTrt,levels=c(control,lvls[lvls!=control]))

  # Calculate table of p-vals
  ptable2<-m2 %>% group_by(variable) %>% summarise(trt.mean=trt.mean(calcTrt,value,2),
                                                   ctrl.mean=trt.mean(calcTrt,value,1),
                                                   delta.mean=delta.mean(calcTrt,value),
                                                   mn.pvals=dist.test(calcTrt,value),
                                                   delta.var=round(delta.vars(calcTrt,value),3),
                                                   var.pvals=var.test.2(calcTrt,value))

  if(print) print(ptable2)

  if(plot){
    g1<-ggplot(m2,aes(x=calcTrt,y=value,variable))+
          geom_boxplot(aes(fill=calcTrt))+
          theme_bw()+theme(axis.text.x=element_blank())+
          facet_wrap(~variable,scales='free_y')
    print(g1)
  }

  return(ptable2)
}

