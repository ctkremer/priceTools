

######################## Vecotor Summary Stats ########################


#' Test the difference in means between distributions.
#'
#' This function computes a simple t-test, determining a p-value for the difference in means
#' between two distributions, assuming homogeneity of variance.
#'
#' @param x A categorical value (factor) with two levels
#' @param y A list of response values
#' 
#' @return A p-value coming from an anova/t-test
#' 
#' @examples
#' 
#' y <- c(rnorm(20,mean=10,sd=2),rnorm(20,mean=25,sd=6))
#' x <- factor(c(rep('A',20),rep('B',20)))
#' 
#' dist.test(x,y)
#' 
dist.test <- function(x, y){
  data <- data.frame(x=x, y=y)
  md1 <- lm(y~x, data=data)
  val <- round(as.data.frame(anova(md1))[1,5],3)
  res <- ifelse(val < 0.001, "<0.001", as.character(val))
  return(res)
}


#' Calculate the difference in means between distributions.
#'
#'
#' @param x A categorical value (factor) with two levels.
#' @param y A list of response values.
#' 
#' @return The difference in means.
#' 
#' @examples
#' 
#' y <- c(rnorm(20,mean=10,sd=2),rnorm(20,mean=25,sd=6))
#' x <- factor(c(rep('A',20),rep('B',20)))
#' 
#' delta.mean(x,y)
#' 
delta.mean <- function(x, y){
  data <- data.frame(x=x, y=y)
  lvl <- levels(data$x)
  v1 <- data$y[data$x == lvl[1]]
  v2 <- data$y[data$x == lvl[2]]
  res <- mean(v2) - mean(v1)
  return(res)
}


#' Test the difference in variances between distributions.
#'
#'
#' @param x A categorical value (factor) with two levels
#' @param y A list of response values
#' 
#' @return A p-value coming from \code{\link{var.test()}}
#' 
#' @examples
#' 
#' y <- c(rnorm(20,mean=10,sd=2),rnorm(20,mean=25,sd=6))
#' x <- factor(c(rep('A',20),rep('B',20)))
#' 
#' var.test.2(x,y)
#' 
var.test.2 <- function(x, y){
  data <- data.frame(x=x, y=y)
  lvl <- levels(data$x)
  v1 <- data$y[data$x == lvl[1]]
  v2 <- data$y[data$x == lvl[2]]
  vd1 <- var.test(v1, v2)
  val <- round(vd1$p.value, 3)
  res <- ifelse(val < 0.001, "<0.001", as.character(val))
  return(res)
}


#' Calculate the difference in variances between distributions.
#'
#'
#' @param x A categorical value (factor) with two levels.
#' @param y A list of response values.
#' 
#' @return The difference in variances.
#' 
#' @examples
#' 
#' y <- c(rnorm(20,mean=10,sd=2),rnorm(20,mean=25,sd=6))
#' x <- factor(c(rep('A',20),rep('B',20)))
#' 
#' delta.vars(x,y)
#' 
delta.vars <- function(x, y){
  data <- data.frame(x=x, y=y)
  lvl <- levels(data$x)
  v1 <- data$y[data$x == lvl[1]]
  v2 <- data$y[data$x == lvl[2]]
  res <- var(v2) - var(v1)
  return(res)
}


#' Calculate the mean of a specific treatment's distribution.
#'
#'
#' @param x A categorical value (factor) with two levels.
#' @param y A list of response values.
#' @param ind The index of the specific treatment to return a mean for.
#' 
#' @return Treatment mean.
#' 
#' @examples
#' 
#' y <- c(rnorm(20,mean=10,sd=2),rnorm(20,mean=25,sd=6))
#' x <- factor(c(rep('A',20),rep('B',20)))
#' 
#' trt.mean(x,y,1)
#' trt.mean(x,y,2)
#' 
trt.mean <- function(x, y, ind){
  data <- data.frame(x=x, y=y)
  lvl <- levels(data$x)
  v2 <- data$y[data$x==lvl[2]]
  v1 <- data$y[data$x==lvl[1]]
  res <- c(mean(v1), mean(v2))[[ind]]
  return(res)
}


# Given a data set, run series of tests on vector plot components:
# - name of treatment pairs column
# - control-control case
# - price components for pairs of communities


#' Run a set of significance tests on vector plot components.
#'
#' This functions tests the significance of the changes in richness in function associated with the
#' vectors of a vector plot. It compares a single baseline (or control) community against a comparison
#' (treatment) community. This can be calculated for the BEF, CAFE, and 5-part Price components.
#'
#' @param data 
#' @param type Specify which kind of components ("bef", "cafe", "both", "price")
#' @param treat.var Specify which column in data contains the treatment variable.
#' @param control Control variable
#' @param standardize Use standardized variables
#' @param print Print table
#' @param plot Display plot
#' 
#' @return Table of significance tests for the means and variances of vector components.
#' 
#' @examples
#' 
#' # write one.
#'
#' @export 
#' @import dplyr 
#' @import ggplot2
test.partitions <- function(data, type='both', treat.var, control, standardize=T, print=F, plot=F){
  
  if(length(control) > 2){
    print("Error! test.cafe() only supports comparisons between one control and one treatment case")}
  if(!(treat.var %in% names(data))){ print("Error! Specified data column does not appear.")}
  
  data <- as.data.frame(data)
  
  if(standardize){
    comps <- c("SRE.L","SRE.G","SCE.L","SCE.G","CDE","SL","SG","SR","CE")
    data[,comps] <- 100*data[,comps]/data$x.func                  # X function scaled
    data$y.func <- 100*(data$y.func - data$x.func)/data$x.func    # Y function scaled
    data$x.func <- 0                                    # X function set as baseline
  }
  
  # Duplicate treatment column, providing standardized name... for later use in dplyr
  data$calcTrt <- data[,treat.var]
  
  data$s.loss <- -1*(data$x.rich - data$c.rich)
  data$s.gain <- data$y.rich - data$c.rich
  data$s.change <- data$y.rich - data$x.rich
  
  data$SL.slope <- -1*data$SL/(data$y.rich - data$c.rich)
  data$SG.slope <- data$SG/(data$x.rich - data$c.rich)

  data$SL.mag <- sqrt(data$SL^2 + data$s.loss^2)
  data$SG.mag <- sqrt(data$SG^2 + data$s.gain^2)

  data$Total <- data$SL + data$SG + data$CDE
  
  # Process data
  switch(type,
         cafe={
           m2 <- reshape2::melt(data[,c('calcTrt','s.loss','s.gain','SL','SG','CDE','Total')],
                                id.vars = 'calcTrt')
         },
         bef={
           m2 <- reshape2::melt(data[,c('calcTrt','s.change','SR','CE','Total')],
                                id.vars = 'calcTrt')
         },
         both={
           m2 <- reshape2::melt(data[,c('calcTrt','s.loss','s.gain','s.change','SL','SG','CDE',
                                        'SR','CE','Total')], id.vars = 'calcTrt')
         },
         price={
           m2 <- reshape2::melt(data[,c('calcTrt','s.loss','s.gain','SRE.L','SCE.L','SRE.G',
                                        'SCE.G','CDE','Total')], id.vars = 'calcTrt')
         },
         "Error! Invalid test type in test.partitions()"
  )
  
  # Ensure that baseline pair is the desired control pair
  lvls <- levels(m2$calcTrt)
  m2$calcTrt <- factor(m2$calcTrt, levels=c(control, lvls[lvls!=control]))
  
  # Calculate table of p-vals
  ptable2<-m2 %>% group_by(variable) %>% summarise(trt.mean=trt.mean(calcTrt,value,2),
                                                   ctrl.mean=trt.mean(calcTrt,value,1),
                                                   delta.mean=delta.mean(calcTrt,value),
                                                   mn.pvals=dist.test(calcTrt,value),
                                                   delta.var=round(delta.vars(calcTrt,value),3),
                                                   var.pvals=var.test.2(calcTrt,value))
  
  if(print) print(ptable2)
  
  if(plot){
    g1 <- ggplot(m2, aes(x=calcTrt, y=value, variable)) +
              geom_boxplot(aes(fill=calcTrt)) +
              theme_bw() + theme(axis.text.x=element_blank()) +
              facet_wrap(~variable, scales='free_y')
    print(g1)
  }
  
  return(ptable2)
}

