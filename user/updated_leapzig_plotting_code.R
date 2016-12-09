
##########################################################

### Upgraded tech of leap.zig family of plotting functions to accept a group aesthetic

# The following script provides a primer for the updated plotting and data processing functions.

# CTK, 12/8/16


##########################################################

library(priceTools)
library(ggplot2)
library(dplyr)
library(gridExtra)

####

find.dropbox<-function(){
  sinf<-Sys.info()
  switch(sinf[['sysname']],
         Windows = {dir=paste("C:/Users/",sinf[['user']],"/Dropbox",sep="")},
         Darwin = {dir=paste("/Users/",sinf[['user']],"/Dropbox",sep="")},
         Linux = {dir=paste("/home/", sinf[['user']],"/Dropbox",sep="")}
  )
  dir
}

# Detect user/platform & find dropbox:
to.dropbox<-find.dropbox()

# load data
resA<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldA_timeXnitrate_pairwise_price_120916.csv",sep=""))
resB<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldB_timeXnitrate_pairwise_price_120916.csv",sep=""))
resC<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldC_timeXnitrate_pairwise_price_120916.csv",sep=""))
resD<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldD_timeXnitrate_pairwise_price_120916.csv",sep=""))

resA$Field<-"A"
resB$Field<-"B"
resC$Field<-"C"
resD$Field<-"D"

# join fields:
rs<-rbind(resA,resB,resC,resD[,-which(names(resD)=="jaccard")])

# pull out a subset of data
rs<-rs[rs$Year.x==1982 & rs$Year.y==1986,]
rs<-rs[rs$NAdd.x==0,]
rs$NAdd<-paste(rs$NAdd.x,rs$NAdd.y)


########################################################


#### Simple vector plots:

# Test that old approaches still work.

# single N comparison
dat1<-rs[rs$NAdd %in% c('0 27.2'),]

leap.zig(dat1,type='bef')
leap.zig(dat1,type='cafe')
leap.zig(dat1,type='cafe',xlim=c(0,18),ylim=c(-100,800),raw.points=F,error.bars=T,vectors=T)


#### Vector plots with grouping:

# Comments. The default behavior for these functions is to calculate average vectors across all pair-wise Price components supplied to process.data() and leap.zig() families of functions. However, we can get more involved in this process and specify additional grouping vectors, so that averages are only computed within sub-groups of the data.

# Average over all N, all fields
# - key here is that we're not specifying any grouping variable
tmpA<-process.data.cafe(data=rs, group.vars=NULL, standardize=F)
leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL)

# Draw separate vector sets by field, average over all N
# - now we have specified an explicit grouping variable in both process.data() and later in leap.zig()
tmpA<-process.data.cafe(data=rs, group.vars=c('Field'), standardize=F)
leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field')

# Draw separate vector sets by N level, with panels by field
# - this requires providing both Field and NAdd to the list of group.vars in process.data()
# - then leap.zig() gets 'NAdd' as a grouping variable (it cannot accept more than one grouping variable)
# - but a separate addition to leap.zig() - a facet_wrap() - uses Field to create separate panels
tmpA<-process.data.cafe(data=rs, group.vars=c('Field','NAdd'), standardize=F)
leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,
              legend=F,error.bars=F,group.vars = 'NAdd') + facet_wrap(~Field)


#########################

### Labeling the origin points for vectors:

tmpA<-process.data.cafe(data=rs, group.vars=c('Field'), standardize=F)

# We can take the labels from the output of process.data()
labs<-tmpA[[3]]


### SL origins:

# we can extract the SL positions directly
labs.SL<-labs[labs$variable=='SL vector',]

leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,
              legend=F,error.bars=F,group.vars = c('Field'),xlim=c(0,20),
              ylim=c(0,630))+
  geom_text(data=labs.SL,aes(x=I(mean.x),y=I(mean.y+30),label=Field),colour='blue',
            size=4)+
  labs(title="1982 vs. 1986")+
  theme(plot.title=element_text(hjust=0.5))


### CDE ends:

# the CDE positions take two steps to extract, because we have to take the second of a pair of points.
labs.CDE<-labs[labs$variable=='CDE vector',]
labs.CDE<-labs.CDE %>% group_by(Field) %>% summarise(mean.y=last(mean.y),mean.x=last(mean.x))

leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,
                         legend=F,error.bars=F,group.vars = c('Field'),xlim=c(0,20),
                         ylim=c(0,630))+
  geom_text(data=labs.CDE,aes(x=I(mean.x),y=I(mean.y+30),label=Field),colour='blue',
            size=4)+
  labs(title="1982 vs. 1986")+
  theme(plot.title=element_text(hjust=0.5))




############################################################

### Large test bed


#### CAFE ####
tmpC<-process.data.cafe(data=rs, group.vars=NULL, standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL,xlim=c(0,20))

# Multiple N treatments with error bars
tmpC<-process.data.cafe(data=rs, group.vars=c('NAdd'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=T,group.vars = 'NAdd',xlim=c(0,20))

# Multiple fields
tmpC<-process.data.cafe(data=rs, group.vars=c('Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))

# Panels by N treatment, vectors by field
tmpC<-process.data.cafe(data=rs, group.vars=c('NAdd','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))+facet_wrap(~NAdd)


#### BEF ####
tmpC<-process.data.bef(data=rs, group.vars=NULL, standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL,xlim=c(0,20))

# Multiple N treatments with error bars
tmpC<-process.data.bef(data=rs, group.vars=c('NAdd'), standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=T,group.vars = 'NAdd',xlim=c(0,20))

# Multiple fields
tmpC<-process.data.bef(data=rs, group.vars=c('Field'), standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))

# Panels by N treatment, vectors by field
tmpC<-process.data.bef(data=rs, group.vars=c('NAdd','Field'), standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))+facet_wrap(~NAdd)



#### Price ####
tmpC<-process.data.price(data=rs, group.vars=NULL, standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL,xlim=c(0,20))

# Multiple N treatments with error bars
tmpC<-process.data.price(data=rs, group.vars=c('NAdd'), standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'NAdd',xlim=c(0,20))

# Multiple fields
tmpC<-process.data.price(data=rs, group.vars=c('Field'), standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))

# Panels by N treatment, vectors by field
tmpC<-process.data.price(data=rs, group.vars=c('NAdd','Field'), standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20),ylim=c(-50,550))+facet_wrap(~NAdd)
