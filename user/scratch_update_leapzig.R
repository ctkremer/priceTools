

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
labs<-labs[labs$variable=='CDE vector',]
labs<-labs[seq(2,64,2),]
labs$mean.y<-labs$mean.y+30

leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = 'NAdd',ylim=c(0,600),xlim=c(0,18))+geom_text(data=labs,aes(x=mean.x,y=mean.y,label=NAdd.y),size=2)+facet_wrap(~Field)



# experimental
tmpC<-process.data.cafe(data=ppA[ppA$Field=='C' & ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = NULL,xlim=c(0,20))
# why did this break?

tmpC<-process.data.cafe(data=ppA[ppA$Field=='C',], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = 'NAdd',xlim=c(0,20))

tmpC<-process.data.cafe(data=ppA[ppA$NAdd.y==27.20,], group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = 'Field',xlim=c(0,20))

tmpC<-process.data.cafe(data=ppA, group.vars=c('NAdd','NAdd.y','Field'), standardize=F)
leap.zig.cafe(tmpC, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=F,gp.vars = 'Field',xlim=c(0,20))+facet_wrap(~NAdd.y)



leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = F,legend=F,error.bars=T,gp.vars = 'NAdd')+facet_wrap(~Field)

leap.zig.cafe(tmpA, loc.standardize=F, vectors=T,raw.points = T,legend=F,error.bars=F,gp.vars = 'NAdd')+facet_wrap(~Field)


# test function
leap.zig.cafe<-function(tmp, xlim=NA, ylim=NA, loc.standardize=TRUE, error.bars=FALSE,
                        raw.points=TRUE, vectors=TRUE, all.vectors=FALSE,gp.vars=NULL,
                        legend=TRUE, old.plot=NA, main="", linetype=1, add=FALSE){
  
  tmp3<-tmp[[3]]
  
  # Trim out un-needed factor levels
  tmp3$variable <- factor(as.character(tmp3$variable), levels=c("SL vector","SG vector","CDE vector"))
  
  # Set up group column?
  if(!is.null(gp.vars)){
    tmp3$gps <- data.frame(tmp3[,gp.vars])[,1]    
  }
  
  # Start plot
  lzp <- ggplot()

  # Add vectors
  if(vectors){
    if(!is.null(gp.vars)){
      lzp <- lzp + geom_path(data=tmp3, aes(colour=variable, x=mean.x, y=mean.y,group=gps),
                             arrow=arrow(length=unit(0.2,"cm"), ends="last"), linetype=linetype)
    }else{
      lzp <- lzp + geom_path(data=tmp3, aes(colour=variable, x=mean.x, y=mean.y),
                             arrow=arrow(length=unit(0.2,"cm"), ends="last"), linetype=linetype)
    }
  }
  
  cols <- c(alpha('#ff0000'), alpha('#00ee00'), alpha('#900090')) 
  trcols <- c(alpha('#ff0000',0.1), alpha('#00ee00',0.1), alpha('#900090',0.1))
  lzp <- lzp + scale_color_manual("Component\n", drop=FALSE, values=cols) +
               guides(colour=guide_legend( override.aes=list(shape=c(NA,NA,NA),
                                                    linetype=c(1,1,1))))

  # Select color and label options:
  lzp <- lzp + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
    scale_x_continuous("Species richness")
  lzp <- lzp + scale_y_continuous("Ecosystem function",limits=c(0,550))

  lzp <- lzp + theme(legend.position="none")
  
  lzp <- lzp + ggtitle(main)
  
  return(lzp)
}

sue<-tmp[[3]] 
sue<-sue[,c(2,3,6)]

hi<-data.frame(x=c(0,1,2,2),y=c(0,0,2,3))
bye<-data.frame(x=c(0,1,2,2,0,1,0.5,0.5),y=c(0,0,2,3,1,1,2,3),
                    gp=c('A','A','A','A','B','B','B','B'),col=as.factor(c(1,2,3,3,1,2,3,3)))

ggplot(hi,aes(x=x,y=y))+geom_path() 

ggplot(bye,aes(x=x,y=y))+geom_path(aes(group=gp,colour=col),arrow=arrow(length=unit(0.2,"cm"), ends="last"))  

plot(mean.y~mean.x,data=sue)






# Needed changes:
#   - process.data doesn't retain grouping variable in elements [[2]] and [[3]]

data<-dd
data<-dat1
group.vars<-c('NAdd')
standardize<-F

process.data.cafe<-function(data, group.vars=NULL, standardize=TRUE){
  
  if(standardize == TRUE){
    comps <- c("SRE.L","SRE.G","SCE.L","SCE.G","CDE","SL","SG","SR","CE")
    data[,comps] <- 100*data[,comps]/data$x.func                  # X function scaled
    data$y.func <- 100*(data$y.func - data$x.func)/data$x.func    # Y function scaled
    data$x.func <- 0 # X function set as baseline.
    data$SG <- data$SL + data$SG  # net SG change:
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
  
  cols <- c(group.vars,'x.func','SL','SG','y.func','x.rich','c.rich','y.rich')
  p2 <- reshape2::melt(data[,cols],id.vars=c(group.vars,'x.rich','c.rich','y.rich'))
  
  # add richness column:
  p2$rich <- ifelse(p2$variable == "x.func", p2$x.rich,
                    ifelse(p2$variable == "SL", p2$c.rich,
                           ifelse(p2$variable == "SG", p2$y.rich,
                                  ifelse(p2$variable == "y.func", p2$y.rich, NA))))
  
  # summarize raw points:
  if(!is.null(group.vars)){
    p3b <- p2 %>% group_by_(.dots=c(group.vars,'variable')) %>% summarise(mean.y=mean(value),
                                                 y.qt.lw=quantile(value, probs=0.025),
                                                 y.qt.up=quantile(value, probs=0.975),
                                                 mean.x=mean(rich),
                                                 x.qt.lw=quantile(rich, probs=0.025),
                                                 x.qt.up=quantile(rich, probs=0.975))
  }else{
    p3b <- p2 %>% group_by(variable) %>% summarise(mean.y=mean(value),
                                                y.qt.lw=quantile(value, probs=0.025),
                                                y.qt.up=quantile(value, probs=0.975),
                                                mean.x=mean(rich),
                                                x.qt.lw=quantile(rich, probs=0.025),
                                                x.qt.up=quantile(rich, probs=0.975))
  }

  
  # stagger rows:
  #p3b2 <- p3b
  #nms <- p3b2$variable[2:4]
  #p3b2 <- p3b2[1:3,]  # instead, drop row y.func?
  #p3b2 <- p3b2[p3b2$variable!='y.func',]
  #p3b2$variable <- nms
  #p4 <- rbind(p3b, p3b2)
  #p4 <- p4[p4$variable != "x.func",]
  p4 <- p3b
  
  # Organize factor levels for plotting:
  p2$variable <- factor(p2$variable,levels=c("x.func","SL","SG","y.func"),
                        labels=c("baseline","SL","SG","comparison"))
  p3b$variable <- factor(p3b$variable,levels=c("x.func","SL","SG","y.func"),
                         labels=c("baseline","SL","SG","comparison"))
#  p4$variable <- factor(p4$variable,levels=c("SL","SG","y.func"),
#                        labels=c("SL vector","SG vector","CDE vector"))
  p4$variable <- as.character(p4$variable)
  p4$variable <- ifelse(p4$variable=="y.func","SG",p4$variable)
  p4$variable <- factor(p4$variable,levels=c("x.func","SL","SG"),
                        labels=c("SL vector","SG vector","CDE vector"))
p4  
  
  p2$variable <- factor(p2$variable, levels=c("baseline","SL","SG","comparison",
                                              "SL vector","SG vector","CDE vector"))
  p3b$variable <- factor(p3b$variable, levels=c("baseline","SL","SG","comparison",
                                                "SL vector","SG vector","CDE vector"))
  p4$variable <- factor(p4$variable, levels=c("baseline","SL","SG","comparison",
                                              "SL vector","SG vector","CDE vector"))
  
  p3b <- p3b[p3b$variable != "baseline",]
  
  return(list(p2, p3b, p4))
}



