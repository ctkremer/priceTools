

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

