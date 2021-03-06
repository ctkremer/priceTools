% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_summary_stats.R
\name{test.partitions}
\alias{test.partitions}
\title{Run a set of significance tests on vector plot components.}
\usage{
test.partitions(data, type = "both", treat.var, control, standardize = T,
  print = F, plot = F)
}
\arguments{
\item{data}{Input data, generated by \code{\link{pairwise.price}}}

\item{type}{Specify which set of vector components to test ("bef", "cafe", "both", "price")}

\item{treat.var}{Specify which column in the input data contains the treatment variable}

\item{control}{Identify which level of the treatment variable shoudl serve as the control distribution}

\item{standardize}{Use standardized variables? (T/F)}

\item{print}{Print output table?}

\item{plot}{Display plot?}
}
\value{
Table of significance tests for the means and variances of vector components. \code{variable} is the richness or function component being tested, \code{trt.mean} and \code{ctrl.mean} are the means of each variable for the treatment and control distributions, \code{delta.mean} is the difference in means, \code{mn.pvals} is the p-value associated with a t-test of the difference between means, \code{delta.var} is the difference in variance between distributions, and \code{var.pvals} is the significance of this difference.
}
\description{
This functions tests the significance of the changes in richness and in function associated with the
vectors of a vector plot. It compares a single baseline (or control) community against a comparison
(treatment) community. This can be calculated for the BEF, CAFE, and 5-part Price components.
}
\details{
Currently, this simple suite of tests focus on detecting differences in the mean and variance
of distributions using parametric tests. In future versions, this can (and should) be expanded to 
include a variety of other tests, including non-parametric approaches. This will be important, as these
distributions are often skewed and complex.
}
\examples{

# Data setup
data(cedarcreek)
head(cedarcreek)

#Identify one grouping columns
cc2<-group_by(cedarcreek,NTrt,NAdd,Plot)

# Perform pairwise comparisons of all communities in cms identified by comm.id
# (takes ~30 sec)
pp<-pairwise.price(cc2,species='Species',func='Biomass')

# Organize/format the results, and pull out a subset using NAdd.x=0 as the control/baseline site
pp<-group.columns(pp,gps=c('NTrt','NAdd'))

pp1<-pp[pp$NAdd \%in\% c('0 0','0 27.2'),]
pp1$NAdd<-factor(as.character(pp1$NAdd),levels=c('0 0','0 27.2'))

# Test differences between the BEF vectors of control-control comparisons & control-treatment comparisons
test.partitions(pp1,type='bef',treat.var = 'NAdd',control = '0 0',print=F,plot=F)

# Combine these tests with a visual summary of the different distributions being tested:
test.partitions(pp1,type='bef',treat.var = 'NAdd',control = '0 0',print=F,plot=T)

# These tests consider the x-axis (richness) and y-axis (function) values of all of the vectors underlying the vector plots produced by \\code{leap.zig}. For example, these two are compatible:
test.partitions(pp1,type='cafe',treat.var = 'NAdd',control = '0 0',print=F,plot=T,standardize=F)
leap.zig(pp1,type='cafe',group.vars='NAdd',raw.points=F,ylim=c(0,1500),standardize=F)
# NOTE - for these comparisions to be valid, the decision to standardize (or not) must be consisted between test.partitions() and leap.zig()

# This approach can also be applied to the CAFE, Price, or CAFE & BEF vector arrangements:
test.partitions(pp1,type='cafe',treat.var = 'NAdd',control = '0 0',print=F,plot=F)
test.partitions(pp1,type='price',treat.var = 'NAdd',control = '0 0',print=F,plot=F)
test.partitions(pp1,type='both',treat.var = 'NAdd',control = '0 0',print=F,plot=F)

}

