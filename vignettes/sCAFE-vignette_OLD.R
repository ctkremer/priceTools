---
title: "sCAFE Vignette"
author: "Colin T. Kremer"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{"sCAFE Vignette"}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

This document provides a short introduction to the Price equation partition, useful for disentangling links between changes in diversity and ecosystem function between pairs of communities. Additionally, it provides examples of useful R functions developed for this workshop to make applying the Price equation easy (hopefully). Finally, the end of the document walks through an example analysis of a larger data set.

Eventually, these tools and documentation will be wrapped into an R package.

***

# Getting Started

## Load Price tools

Many of the R functions designed for this workshop can be found in the `Price_FUNCTIONS_050916.R` file, or a more recent version. You can open and run this file directly, or use the `source()` command.

```{r,results='hide',message=FALSE,echo=FALSE}
setwd('/Users/colin/Dropbox//sCAFE_SharedFolder/')
source('./sCAFE_R_Code/Price_FUNCTIONS_050916.R')
```


## Setting up data

To calculate the Price Partition, we need data on the identity and function of species from two communities, X and Y. We can then use the function `price.part()` to obtain the components explaining change in function between communities. First, however, we need to make sure that our community data is in the right format.

Load example data for demoing tools:
```{r,echo=FALSE}
price.data<-read.csv("./sCAFE_Documentation/example_data.csv")
```

We can either load a data file that is already formatted for `price.part()`, or we can load data in a format more typical of empirical data sets, and run that data through the function `data.setup()` to format it correctly. The desired final format has a row for each unique species that occurs in one or both communities. Columns include the species ID, the function of each species in X and Y, and three book-keeping columns that track whether each species appears in both X and Y, or X, or Y. Species that do not appear in a community are listed as having 0 function in that community.

*Method A:*

One data set with 3 columns:
  - species
  - function of X
  - function of Y

```{r,warning=FALSE}
head(price.data)
comm<-data.setup(list(price.data))

head(comm)
```

*Method B:*

```{r,echo=FALSE}
dataX<-price.data[price.data$biomassX!=0,c(1,2)]
dataY<-price.data[price.data$biomassY!=0,c(1,3)]
```

Two data set with 2 columns:
  - species
  - function

```{r}
head(dataX)
head(dataY)

comm<-data.setup(list(dataX,dataY))

head(comm)
```


## Calculating Price equation partition

After we have taken data from two communities, X and Y, and created a properly formatted data object (either by hand, or by using the `data.setup()` function), we can use the `price.part()` function to compute the price equation partition for these communities.

```{r}
price.part(comm)
```

Following Fox & Kerr 2012, the output gives us values for the Price equation partition:
  - SRE.L = species richness effect, loss
  - SRE.G = species richness effect, gain
  - SCE.L = species composition effect, loss
  - SCE.G = species composition effect, gain
  - CDE = context dependent effect

It also provides terms that quantify ecosystem change from the CAFE and BEF perspectives:
  - SL = SRE.L + SCE.L = 'Species loss effect', CAFE
  - SG = SRE.G + SCE.G = 'Species gain effect', CAFE
  - SR = SRE.L + SRE.G = 'Species richness effect', BEF
  - CE = SCE.L + SCE.G + CDE = 'Species composition effect', BEF

And additional values:
  - x.func = Total ecosystem function in baseline X community
  - y.func = Total ecosystem function in comparison Y community
  - x.rich = Species richness in X community
  - y.rich = Species richness in Y community
  - c.rich = Number of species shared by X and Y communities

***

# Exploring some real data

## Data

This example uses data provided by H. Auge for sCAFE on plant community cover and biomass, including disturbance, rodent exclosure, and seeding treatments.

```{r}
#data<-read.csv("./sCAFE_Documentation/sCafe_Data_Auge_biomass.csv",stringsAsFactors=F)
data<-read.csv("./sCAFE_Data/Edited/Auge/sCafe_Data_Auge_sppbiom_2010to2012_2.csv",stringsAsFactors=F)

### Clean up data

# Remove some oddball entries.
data<-data[!(data$Species %in% c('','herb spec. 1','#REF!','woody seedling')),]

# Focus on 2012 data.
data<-data[data$Year==2012,]

# take a peak at the data:
head(data)
```

## Single Price equation comparison

This data set provides many many different communities that we hope to compare. For now, we will just pull out two of them, corresponding to two replicates within the same site, with the same combination of treatments.

```{r}
# Subset the data to extract 2 specific community samples:
comX<-data[data$Site==1 & data$Rodent=="control" &
data$Seed_addition=="no_seeds" & data$Disturbance=="disturbed" &
data$Replicate==1,]

comY<-data[data$Site==1 & data$Rodent=="control" &
data$Seed_addition=="no_seeds" & data$Disturbance=="undisturbed" &
data$Replicate==1,]

# Just need the species ID and function columns:
comX<-comX[,c('Species','Biomass_CalcSp')]
comY<-comY[,c('Species','Biomass_CalcSp')]

# Set up the data:
comm<-data.setup(list(comX,comY))
head(comm)
```

Great! Now we can run a Price equation partition on these two communities:

```{r}
price.part(comm)
```

Again, we obtain the 5 components outlined in Fox & Kerr 2012. To provide some interpretation, we are trying to understand how changes in the presence and function of species influences differences in total function between community X and Y.

- SRE.L is negative; losing species randomly from X decreases function (biomass).
- SRE.G is positive; gaining species randomly in Y increases function (biomass).
- SCE.L is positive; the particular species lost from X had lower than average function. One way to understand this is that the sum of SRE.L and SCE.L provide the total effect of species loss (SL). A positive value for SCE.L moderates the negative effect of SRE.L, making the total effect of species loss smaller. This means the species lost were not as high functioning as the 'average' species in X.
- SCE.G is negative; the particular species gained in Y had below average function.
- CDE is positive; species that occur in X and Y show higher function in Y than they had in X.


## Multiple, pairwise Price comparisons

This is great, but if we have to repeat this process over and over again for the thousands and thousands of possible pairwise comparisons between communities and treatments in this data set, it will get very tedious and confusing. In the next section, I want to introduce you to some tools that will automate this process.

The first step is to take our entire data set and provide information on the set of columns that are used to group species observations into a single community data set. In this case, this includes our treatment variables (Rodent, Seed_addition, Disturbance) as well as columns indicating sampling structure (Site, Replicate). In a different data set, this might also include a time variable, like sampling date.

First we will look at the disturbance treatment. This requires subsetting our data, which you can do in a bunch of ways (here I am making use of tools from the dplyr package).

### Effects of Disturbance on plant community composition and function.

For this example, we will return to data from H. Auge on the composition and function of plant communities. Before making calculations, we need to organize and process our data. A number of different treatments are available; initially we will focus on the effects of disturbance, and avoid examining the consequences of other treatments (Rodent exclosure, and Seed addition).

```{r}
# Exclude the Rodent and Seed addition treatments (by focusing only on 'control' and 'no_seeds' cases):
data1<- data %>% filter(Rodent=="control",Seed_addition=="no_seeds")
```

Another step we need to take is to identify the grouping variable(s) that organize our data, usually based on the treatment and replication structures in our data set. This is necessary for allowing our code to identify the unique communities that we want to compare.

```{r}
# Define a set of grouping and treatment variables and associate them with the data object:
group.vars<-c('Site','Replicate')
treat.vars<-c('Disturbance')
grouped.data1 <- data1 %>% group_by_(.dots=c(group.vars,treat.vars))
```

Having grouped our data, we can use a function called `pairwise.price()` which will take our data frame and compute the Price equation paritions for all pairwise combinations of communities identified by our grouping variables. When we call the `pairwise.price()` function, we have to provide it with our grouped data, and also indicate which columns in the grouped data set contain the species IDs (`species="Species"`) and the ecosystem function we are examining (`func="Biomass_CalcSp"`).

*CAUTION* - This function can take a while to run, as the number of pairwise comparisons can be quite large. It is worth pausing to think a moment before running this function so you are aware of the size of the computational task you are setting for your computer (and maybe whether you have time to go have a coffee).

```{r}
# Calculate pairwise comparisons of sampled communities using the price equation.
#   -  NOTE: Self-comparisons of each community to itself are automatically excluded
res1<- pairwise.price(grouped.data1,species="Species",func="Biomass_CalcSp")
head(res1)
```

This is pretty awesome. For each of our treatment, site, and replicate combinations, we now have the 5-part Price equation partition, as well as combinations of these terms (SL, SG, SR, CE). There are also additional columns keeping track of the function and richness of the baseline and comparison communities, and the number of shared species between communities.

Take a look at the second line of the `res1` data frame. It should look pretty familiar, because it is the set of results we obtained from our single Price equation comparison in the previous section. But now we have all of the possible pairwise comparisons, which will allow us to disentangle treatment effects from background noise caused by sampling error or process error. The next section will explore ways of visualizing and analyzing this data set of pairwise Price comparisons.


### Visually & statistically comparing Price partition results

After manipulating these results a little bit, we can use a set of new graphing functions to explore visually and statistically how different decompositions of changes in ecosystem function (BEF, CAFE, Price) respond to the imposed Disturbance treatment.

#### Data set up

```{r}
# Create a single column keeping track of the paired set of Disturbance treatments & other grouping variables:
pp1<-res1
pp1<-group.columns(pp1,gps=c(group.vars,treat.vars),drop=T)
head(pp1)
```

Depending on the analyses that we are interested in, and what we want to test, we do not need to examine all pairs of comparisons. We can subset the results of `pairwise.price()` to remove unneeded comparisons. For example, in this analysis, we are interested in the control-control comparisons (here, the undisturbed vs. undisturbed cases). We also want to retain the control-treatment comparisons (undisturbed disturbed), but not the treatment-control comparisons, because we want to be able to identify directional effects of imposing a disturbance treatment.

```{r}
# Subset pairwise results:
pp1<-pp1[pp1$Disturbance %in% c('undisturbed undisturbed','undisturbed disturbed'),]

# Update factor labeling for Disturbance treatment (helps later with plotting)
pp1$Disturbance<-factor(as.character(pp1$Disturbance),levels=c('undisturbed undisturbed','undisturbed disturbed'))
head(as.data.frame(pp1))

# Stash data on distinct sets of comparisons separately (to aid plotting)
dat1<-pp1[pp1$Disturbance %in% c('undisturbed disturbed'),]
dat1.ctrl<-pp1[pp1$Disturbance %in% c('undisturbed undisturbed'),]
```


#### CAFE-style vector plots

We can plot the result of the pairwise Price comparisons using the `leap.zip()` function, which is essentially a wrapper for a complex ggplot construction. It requires inputting a data set of comparisons, resulting from the `pairwise.price()` computation we ran earlier. This `leap.zip()` function can accept a large number of options, which give the user control over the appearance and content of the final plot. Several examples follow.

First, we can look at the CAFE-style decomposition of changes in ecosystem function (by specifying `type='cafe'`). Additional options allow us to provide plot titles, change the size of the plotting window, and display the mean vectors associated with each component as well as associated error bars.

```{r}
leap.zig(dat1,type='cafe',main="Disturbance \n(ctrl vs. distrb)")
leap.zig(dat1,type='cafe',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-100,500))
leap.zig(dat1,type='cafe',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-100,500),error.bars=T)
leap.zig(dat1,type='cafe',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-100,500),error.bars=T,vectors=T)
```

Note that by default, the `leap.zig()` function standardizes all changes in function by the total function of the baseline communities. As a result, all y-axis values can be viewed as %changes in ecosystem function relative to the baseline community. Alternatively, we can avoid this choice of standardization by:

```{r}
leap.zig(dat1,type='cafe',main="Disturbance \n(ctrl vs. distrb)",standardize = FALSE)
```

These plots can get quite busy. Sometimes it will be helpful to make similar plots, but display only the mean vectors across pairwise comparisons:

```{r}
# vectors only
leap.zig(dat1,type='cafe',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-100,500),raw.points=F,error.bars=T,vectors=T)
```

We can also get a sense of how this plot of control-treatment pairs looks compared with control-control pairs. This requires saving the graphical vector plots that result from multiple treatments. Then we can draw plots side-by-side:

```{r}
s1<-leap.zig(dat1.ctrl,type='cafe',main="Disturbance \n(ctrl vs. ctrl)",
             xlim=c(0,35),ylim=c(-100,100),error.bars=T,
             vectors=T,raw.points = F,legend=FALSE)
s2<-leap.zig(dat1,type='cafe',main="Disturbance \n(ctrl vs. distrb)",
             xlim=c(0,35),ylim=c(-100,100),error.bars=T,
             vectors=T,raw.points = F,legend=FALSE)
grid.arrange(s1,s2,nrow=1)
```

In this example, the differences are quite minor.



#### BEF-style vector plots

Analogous sets of plots can be produced for the BEF decomposition of changes in ecosystem function.

```{r}
leap.zig(dat1,type='bef',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-100,300),error.bars=T,vectors=T)
```

With vectors only:

```{r}
# vectors only
leap.zig(dat1,type='bef',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-10,20),raw.points=F,error.bars=T,vectors=T)
```


#### 5-part Price vector plots

Analogous sets of plots can be produced for the full 5-part Price decomposition of changes in ecosystem function.

```{r}
leap.zig(dat1,type='price',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-100,50),error.bars=T,vectors=T)
```

Or just the vectors:

```{r}
leap.zig(dat1,type='price',main="Disturbance \n(ctrl vs. distrb)",xlim=c(0,35),ylim=c(-100,50),raw.points=F,error.bars=F,vectors=T)
```


#### Comparing treatments

We can also compare the results of multiple treatments, in a variety of ways.

CAFE vs. BEF comparisons can be made easily using `type='both'` in function `leap.zig()`, as follows:

```{r}
leap.zig(dat1,type='both',standardize=T,
         xlim=c(0,30),ylim=c(-75,10),error.bars=F,
         main="Disturbance (ctrl vs. distrb)",vectors=T,raw.points = F,legend=T)
```

Less standard comparisons can also be made, by saving the output of one or more vector plots, which then can be drawn side-by-side, or on top of each other. To demonstrate this, we will process data for a second treatment in this data set, which examines the effects of imposing a Rodent exclosure on the composition and function of plant communities.

Data processing (mirrors previous section, succinctly):

```{r}
## Rodent treatments:

# Exclude Disturbance and Seed addition treatments
data2<- data %>% filter(Disturbance=="undisturbed",Seed_addition=="no_seeds")

# Define grouping variables required for this comparison:
group.vars.2<-c('Site','Replicate')
treat.vars.2<-c('Rodent')
grouped.data2 <- data2 %>% group_by_(.dots=c(group.vars.2,treat.vars.2))
res2<- pairwise.price(grouped.data2,species="Species",func="Biomass_CalcSp")

# Create a single column keeping track of the paired set of Rodent treatments:
pp2<-res2
pp2<-group.columns(pp2,gps=c(group.vars.2,treat.vars.2),drop=T)

# Subset pairwise results, keeping control-control and control-treatment comparisons
pp2<-pp2[pp2$Rodent %in% c('control control','control exclosure'),]

# Update factor labels
pp2$Rodent<-factor(pp2$Rodent,levels=c('control control','control exclosure'))

# Stash data on distinct sets of comparisons, to aid plotting:
dat2<-pp2[pp2$Rodent %in% c('control exclosure'),]
dat2.ctrl<-pp2[pp2$Rodent %in% c('control control'),]
```

Now we can compare vector plots, first side-by-side:

```{r}
s1<-leap.zig(dat1,type='cafe',main="Disturbance (ctrl vs. distrb)",
             xlim=c(0,35),ylim=c(-100,100),error.bars=T,
             vectors=T,raw.points = F,legend=FALSE)
s2<-leap.zig(dat2,type='cafe',main="Rodent (ctrl vs. excl)",
             xlim=c(0,35),ylim=c(-100,100),error.bars=T,
             vectors=T,raw.points = F,legend=FALSE)
grid.arrange(s1,s2,nrow=1)
```

Then on top of each other:

```{r}
leap.zig(dat2,type='cafe',main="Disturbance vs. Rodent ",
         xlim=c(0,35),ylim=c(-100,100),
         error.bars=T,vectors=T,raw.points = F,legend=FALSE,
         add=TRUE,old.plot=s1)
```

#### Statistical comparisons

We have also designed a suite of statistical tests that can be run on each of the components behind the vectors in the visualizations we just explored. At the simplest, these depend on comparing two distributions for each component, such as the CDE term. The first distribution comes from the set of all pairwise comparisons of control-control communities, while the second comes from the control-treatment pairs. Currently, we use parametric tests to determine whether these distributions differ in terms of their means and variances.

To provide an example, let us first examine the BEF decomposition for the Disturbance data. In terms of vector plots, we saw:

```{r}
s1<-leap.zig(dat1.ctrl,type='bef',main="Disturbance \n(ctrl vs. ctrl)",
             xlim=c(5,25),ylim=c(-10,45),error.bars=F,
             vectors=T,raw.points = F,legend=FALSE)
s2<-leap.zig(dat1,type='bef',main="Disturbance \n(ctrl vs. distrb)",
             xlim=c(5,25),ylim=c(-10,45),error.bars=F,
             vectors=T,raw.points = F,legend=FALSE)
grid.arrange(s1,s2,nrow=1)
```

When we run the statistical tests, we see:

```{r}
test.partitions(pp1,type='bef',treat.var = 'Disturbance',control = 'undisturbed undisturbed',print=F,plot=F)
```

This function returns a table of statistical results. Within the table, the first column specifies the variable (vector component) being tested. Reading across the columns left to right, we find:
  - `trt.mean` the mean value for each vector component within the control-treatment pairs,
  - `ctrl.mean` and then for the control-control pairs,
  - `delta.mean` the difference of these values,
  - `mn.pvals` a p-value associated with the difference in means of the distributions,
  - `delta.var` the difference in variance between the distributions, and
  - `var.pvals` a p-value associated with the difference in variance.

In this case, we see that the net change in species richness (s.change) was non-significant (less than one species on average). However,

head(res1)

as well as a plot of each of the distributions being tested (the latter behavior can be turned off using `plot=FALSE`)

resX<-res1
resX$dfunc<-resX$y.func-resX$x.func

resX %>% group_by(Disturbance.x,Disturbance.y) %>% summarise(mean(dfunc))

Similar results can be obtained invoking different decompositions of change in ecosystem function (CAFE, 5-part Price), as follows:


```{r}
# first example, using cafe partition:
test.partitions(pp1,type='cafe',treat.var = 'Disturbance',control = 'undisturbed undisturbed',print=F,plot=F)

test.partitions(pp1,type='price',treat.var = 'Disturbance',control = 'undisturbed undisturbed',print=F,plot=T)
```

NOTE: I have included estimates for slopes/magnitudes for the CAFE components only. We are still trying to understand if these alternate parameterizations of vectors offer any additional useful interpretations over the raw X and Y components of each vector. If they prove useful, the BEF and Price code can be extended to match.

CAUTION: for compatability with the vector plots, these statistical tests should be standardized (or not standardized), depending on earlier choices, using the flag: `standardize=T`.

NOTE: standardizing values has some interesting effects on control-control components. In several cases, values that we expect to be centered on zero due to symmetry actually shift away from zero. This is mathematically correct, if initially unintuitive, and arises from taking the mean of often highly skewed distributions. Whether this is desirable or avoidable is an open question.

We can perform the same calculations for other data sets or comparisons:

```{r}
# Here for the Rodent data set
test.partitions(pp2,type='bef',treat.var = 'Rodent',control = 'control control',print=F)
```


***

# Wishlist:

There are a whole bunch of avenues and tasks that could be extended (and hopefully will be over the coming months). Here is a partial list:






