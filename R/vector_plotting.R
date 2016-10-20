

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
