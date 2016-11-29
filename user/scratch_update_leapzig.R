

### Upgrade tech of leap.zig family of plotting functions to accept a group aesthetic?






leap.zig(dat1,type='cafe',main="Enrichment \n(0 vs. 27.2)")

leap.zig(dat1,type='cafe',main="Enrichment \n(0 vs. 27.2)",standardize = FALSE,raw.points=F,vectors=T)


library(ggplot2)
library(dplyr)
library(gridExtra)

s1 <- leap.zig(dat1.ctrl,type='cafe',main="Enrichment \n(0 vs. 0)",
               error.bars=F,standardize = F,
               vectors=T,raw.points = F,legend=FALSE)
s2 <- leap.zig(dat1,type='cafe',main="Enrichment \n(0 vs. 27.2)",
               error.bars=F,standardize = F,add=T,old.plot=s1,
               vectors=T,raw.points = F,legend=FALSE)
#grid.arrange(s1,s2,nrow=1)
s2

head(dat1)
head(dat1.ctrl)

dd<-rbind(dat1.ctrl,dat1)
head(dd)



### Now in pieces:

tmp <- process.data.cafe(data=dd, group.vars=c('NAdd'), standardize=F)
head(tmp[[1]])
head(tmp[[2]])
tmp[[3]]

leap.zig.cafe(tmp, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars='NAdd')

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
resA<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldA_timeXnitrate_pairwise_price_092116.csv",sep=""))
resB<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldB_timeXnitrate_pairwise_price_092116.csv",sep=""))
resC<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldC_timeXnitrate_pairwise_price_092116.csv",sep=""))
resD<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/data/fieldD_timeXnitrate_pairwise_price_092116.csv",sep=""))

resA$Field<-"A"
resB$Field<-"B"
resC$Field<-"C"
resD$Field<-"D"

# join fields:
rs<-rbind(resA,resB,resC,resD[,-which(names(resD)=="jaccard")])

rs<-rs[rs$Year.x==1982 & rs$Year.y==1986,]
rs<-rs[rs$NAdd.x==0,]

group.vars <- c('Plot','Year')
treat.vars <- c('NTrt')
ppA<-group.columns(rs,gps=c(group.vars,treat.vars),drop=T)
ppA$NAdd<-paste(ppA$NAdd.x,ppA$NAdd.y)

tmpA<-process.data.cafe(data=ppA, group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
tmpA[[3]]

# Field A map of vectors
leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = NULL)

leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = 'NAdd')+facet_wrap(~Field)


labs<-tmpA[[3]]

helper<-function(x){
  sign(x[2]-x[1])
}
labs2<-labs %>% filter(variable=='CDE vector') %>% group_by(NAdd,Field) %>% summarise(sgn=helper(mean.y))
labs <- merge(labs,labs2,all.x = T,by=c('NAdd','Field'))
labs<-labs[labs$variable=='CDE vector',]
labs<-labs[seq(2,64,2),]
head(labs)

leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,
              legend=F,error.bars=F,gp.vars = 'NAdd',ylim=c(0,600),
              xlim=c(0,18))+
    geom_text(data=labs,aes(x=mean.x,y=I(mean.y+sgn*30),label=NAdd.y),size=3)+
    facet_wrap(~Field)



# experimental
tmpC<-process.data.cafe(data=ppA[ppA$Field=='C' & ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL,xlim=c(0,20),all.vectors = T)
# why did this break?

# Multiple N treatments with error bars
tmpC<-process.data.cafe(data=ppA[ppA$Field=='C',], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=T,gp.vars = 'NAdd',xlim=c(0,20))

# Multiple fields
tmpC<-process.data.cafe(data=ppA[ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = 'Field',xlim=c(0,20))

# Panels by N treatment, vectors by field
tmpC<-process.data.cafe(data=ppA, group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = 'Field',xlim=c(0,20))+facet_wrap(~NAdd.y)



leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=T,gp.vars = 'NAdd')+facet_wrap(~Field)

leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = T,legend=F,error.bars=F,gp.vars = 'NAdd')+facet_wrap(~Field)



### Over drought:

rs<-rbind(resA,resB,resC,resD[,-which(names(resD)=="jaccard")])

rs<-rs[rs$Year.x==1986 & rs$Year.y==1988,]
rs<-rs[rs$NAdd.x==rs$NAdd.y,]

group.vars <- c('Plot','Year')
treat.vars <- c()
ppB<-group.columns(rs,gps=c(group.vars,treat.vars),drop=T)
ppB$NAdd<-paste(ppB$NAdd.x,ppB$NAdd.y)

head(rs)

rs$i.rich<-rs$x.rich
tmpB<-process.data.cafe(data=rs, group.vars=c('i.rich','Field'), standardize=F)
head(tmpB)

#[rs$Plot.x==rs$Plot.y,]
rs<-rbind(resA,resB,resC,resD[,-which(names(resD)=="jaccard")])
rs<-rs[rs$Year.x==1986 & rs$Year.y==1988,]
rs<-rs[rs$NAdd.x==rs$NAdd.y,]
rs$i.rich<-rs$x.rich
head(rs)
str(rs)

tmpB<-process.data.cafe(data=rs[rs$NTrt.x %in% c(1) & rs$Plot.x==rs$Plot.y,], group.vars=c('i.rich','NTrt.x'), standardize=F)

labs<-tmpB[[3]]
labs<-labs[labs$variable=='SL vector',]
head(labs)

leap.zig.cafe(tmpB, loc.standardize=F, vectors=T,raw.points = F,
              legend=F,error.bars=F,gp.vars = 'i.rich',xlim=c(0,20),
              ylim=c(50,600))+
  geom_text(data=labs,aes(x=I(mean.x+0.5),y=mean.y,label=i.rich),size=3)+
  facet_wrap(~NTrt.x)+
  geom_point(data=jay,aes(x=i.rich,y=y.func))

jay <- rs[rs$Plot.x==rs$Plot.y & rs$NTrt.x==1,] %>% group_by(i.rich) %>% summarise(y.func=mean(x.func))
head(jay)
head(tmpB[[3]])
data.frame(tmpB[[3]])




### Initial fig:

rs<-rbind(resA,resB,resC,resD[,-which(names(resD)=="jaccard")])
rs$Year<-paste(rs$Year.x,rs$Year.y)
rs2<-rs[rs$Year.x==1982 & rs$Year.y==1986,]
rs2<-rs2[rs2$NTrt.x==rs2$NTrt.y,]

# NTrt = 1, from 82-86 
rs5<-rs[rs$Year.y == '1986' & rs$Plot.x==rs$Plot.y & rs$NTrt.x==rs$NTrt.y,]
rs5<-rs5[rs5$NTrt.x==1,]

#library(bbmle)
#m1<-mle2(x.func~dnorm(mean=10*a*x.rich/(x.rich+k),sd=exp(s)),start=list(a=30,k = 1,s=10),data=rs5)
#summary(m1)
pd<-predict(m1,newdata=data.frame(x.rich=seq(0,18,1)))
pd2<-data.frame(x.rich=seq(0,18,1),x.func=pd)

tmpB<-process.data.cafe(data=rs2, group.vars=c('NTrt.y','Field'), standardize=F)

labs<-tmpB[[3]]
labs<-labs[labs$variable=='SL vector',]


leap.zig.cafe(tmpB, loc.standardize=F, vectors=T,raw.points = F,
              legend=F,error.bars=F,group.vars = c('NTrt.y'),xlim=c(0,20),
              ylim=c(50,600))+
  geom_text(data=labs,aes(x=I(mean.x+0.5),y=mean.y,label=NTrt.y),colour='blue',
            size=4)+
  #geom_point(data=rs5,aes(x=x.rich,y=x.func),alpha=0.3,size=3)+
  geom_line(data=pd2,aes(x=x.rich,y=x.func),col='blue')+
  facet_wrap(~Field)+ggtitle(label = "1982 vs 1986")


# 1982 variation in function by N treatment
sal<-rs2[rs2$Plot.x==rs2$Plot.y,]
sal$NTrt.x<-as.factor(sal$NTrt.x)
ggplot(data=sal,aes(y=x.func,Field,x=NTrt.x))+geom_boxplot(aes(fill=NTrt.x))+facet_wrap(~Field)


# Or in one go:

leap.zig(data=rs2,type='cafe',group.vars=c('NTrt.y'),standardize = F,
         vectors=T,raw.points = F,
         legend=F,error.bars=F,xlim=c(0,20),ylim=c(0,600))+
  geom_text(data=labs,aes(x=I(mean.x+0.5),y=mean.y,label=NTrt.y),colour='blue',
            size=4)+
  geom_point(data=rs5,aes(x=x.rich,y=x.func),alpha=0.3,size=3)+
  geom_line(data=pd2,aes(x=x.rich,y=x.func),col='blue')+
  geom_hline(yintercept = 0)





###############

# Large test bed

#### CAFE ####
tmpC<-process.data.cafe(data=ppA[ppA$Field=='C' & ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL,xlim=c(0,20))

# Multiple N treatments with error bars
tmpC<-process.data.cafe(data=ppA[ppA$Field=='C',], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=T,group.vars = 'NAdd',xlim=c(0,20))

# Multiple fields
tmpC<-process.data.cafe(data=ppA[ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))

# Panels by N treatment, vectors by field
tmpC<-process.data.cafe(data=ppA, group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))+facet_wrap(~NAdd.y)


#### BEF ####
tmpC<-process.data.bef(data=ppA[ppA$Field=='C' & ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL,xlim=c(0,20))

# Multiple N treatments with error bars
tmpC<-process.data.bef(data=ppA[ppA$Field=='C',], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=T,group.vars = 'NAdd',xlim=c(0,20))

# Multiple fields
tmpC<-process.data.bef(data=ppA[ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))

# Panels by N treatment, vectors by field
tmpC<-process.data.bef(data=ppA, group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.bef(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))+facet_wrap(~NAdd.y)



#### Price ####
tmpC<-process.data.price(data=ppA[ppA$Field=='C' & ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = NULL,xlim=c(0,20))

# Multiple N treatments with error bars
tmpC<-process.data.price(data=ppA[ppA$Field=='C',], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'NAdd',xlim=c(0,20))

# Multiple fields
tmpC<-process.data.price(data=ppA[ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))

# Panels by N treatment, vectors by field
tmpC<-process.data.price(data=ppA, group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.price(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,group.vars = 'Field',xlim=c(0,20))+facet_wrap(~NAdd.y)
