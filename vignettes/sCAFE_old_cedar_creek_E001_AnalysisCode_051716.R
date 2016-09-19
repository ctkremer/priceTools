
##################

# sCAFE analysis of old Cedar Creek/Tilman data set
# - begun by S. Harpole
# - extended by CT Kremer

##################

library(splines)
library(MASS)

# Function to detect system type and user, to establish path to dropbox
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

# load remaining tools for project:
source(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_R_Code/development/Price_FUNCTIONS_050916.R",sep=""))

# function for accessing ggplot color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_color_hue(6)


##################

# Stan's input file; line numbers didn't work for CTK.
#cdr<-read.table(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Data/Original/CedarCreek/harpole/e001_Plant aboveground biomass data.txt",sep=""),
#                    sep="\t", skip=51, nrows=46808-52, header=T)

# Load input data:
cdr<-read.table(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Data/Original/CedarCreek/harpole/e001_Plant aboveground biomass data.txt",sep=""),
                sep="\t", skip=135, header=T,nrows=46756)

# clean up:

# drop litter
cdr<-subset(cdr, cdr$Species!="Miscellaneous litter")

# other oddballs:
cdr<-subset(cdr, !(cdr$Species %in% c("Miscellaneous grasses","Miscellaneous herbs","Mosses & lichens")))

# just year 1992
cdr92<-subset(cdr, cdr$Year==1992)

# just field D
cdr92D<-subset(cdr92, cdr92$Field=="D")

# Unresolved: What is the difference between NTrt 1 and 9?
#table(cdr92D[,c('NTrt','NAdd')])
#table(cdr92D[,c('NTrt','NitrAdd')])


########### First look at the highest level of Nitrate addition ################

## Set up data:

# focus on 0 and 28 g/m2/yr nitrogen additions
data1<- cdr92D %>% filter(NTrt %in% c(1,8))

# Define a set of grouping variables that organize the data,
# based on treatment and replicate structure, then associate them with the data object:
group.vars<-c('Plot')
treat.vars<-c('NTrt','NAdd')
grouped.data1 <- data1 %>% group_by_(.dots=c(group.vars,treat.vars))

# calculate pairwise comparisons of sampled communities using the price equation:
# - self-comparisons of the same community are automatically excluded
res1<- pairwise.price(grouped.data1,species="Species",func="Biomass")

# Save a copy of this output, and prepare it for analysis:
pp1<-res1

# create a single column keeping track of the paired set of treatments & other grouping variables:
pp1<-group.columns(pp1,gps=c(group.vars,treat.vars),drop=T)
head(pp1)
dim(pp1)

# For analysis, we want to retain control-control comparisons, control-treatment comparisons,
# but not treatment-control comparisons
pp1<-pp1[pp1$NTrt %in% c('1 1','1 8'),]

# update factor labeling for Nutrient treatment
pp1$NTrt<-factor(pp1$NTrt,levels=c('1 1','1 8'))
head(as.data.frame(pp1))

# stash data on distinct sets of comparisons, to aid plotting:
dat1<-pp1[pp1$NTrt %in% c('1 8'),]
dat1.ctrl<-pp1[pp1$NTrt %in% c('1 1'),]


### Plotting results:

# As a single figure
s1<-leap.zig(dat1,type='cafe',main="Nutrients \n(ctrl vs. addition)",
              xlim=c(0,20),ylim=c(-100,600),error.bars=T,vectors=T,
             legend=F)

s2<-leap.zig(dat1,type='bef',main="Nutrients \n(ctrl vs. addition)",
         xlim=c(0,20),ylim=c(-100,600),error.bars=T,vectors=T,
         legend=F)

s3<-leap.zig(dat1,type='price',main="Nutrients \n(ctrl vs. addition)",
             xlim=c(0,20),ylim=c(-100,600),error.bars=T,vectors=T,
             legend=F)

grid.arrange(s1,s2,s3,nrow=1)



## Results by conceptual approach:

# CAFE:
test.partitions(pp1,type='cafe',treat.var = "NTrt",control = '1 1',print=F,plot=F)

leap.zig(dat1.ctrl,type='cafe',main="NTrt ",
         xlim=c(3,20),ylim=c(-100,600),error.bars=T,vectors=T,
         legend=F,add=TRUE,linetype=2,old.plot=s1)

# BEF
test.partitions(pp1,type='bef',treat.var = "NTrt",control = '1 1',print=F,plot=F)

leap.zig(dat1.ctrl,type='bef',main="NTrt ",
         xlim=c(3,20),ylim=c(-100,600),error.bars=T,vectors=T,
         legend=F,add=TRUE,linetype=2,old.plot=s2)

# 5-part Price
test.partitions(pp1,type='price',treat.var = "NTrt",control = '1 1',print=F,plot=F)

leap.zig(dat1.ctrl,type='price',main="NTrt ",
         xlim=c(3,20),ylim=c(-100,600),error.bars=T,vectors=T,
         legend=F,add=TRUE,linetype=2,old.plot=s3)



############ Expanded analysis across Nitrate levels ##################


# focus on full range of nitrogen additions, excluding treatment 9 (which I don't understand)
data2<- cdr92D %>% filter(NTrt != 9)

# Define a set of grouping variables that organize the data,
group.vars<-c('Plot')
treat.vars<-c('NTrt','NAdd')
grouped.data2 <- data2 %>% group_by_(.dots=c(group.vars,treat.vars))

# calculate pairwise comparisons of sampled communities using the price equation:
# - self-comparisons of the same community are automatically excluded
res2<- pairwise.price(grouped.data2,species="Species",func="Biomass")

# Save a copy of this output, and prepare it for analysis:
pp2<-res2

# retain only comparisons against NTrt.x = 1
pp2<-pp2[pp2$NTrt.x==1,]

# create a single column keeping track of the paired set of treatments & other grouping variables:
pp2<-group.columns(pp2,gps=c(group.vars,treat.vars),drop=T)
head(pp2)
dim(pp2)


########### More Plotting ###############

# via CAFE:
comps<-levels(pp2$NTrt)
comps<-comps[-1]

plot.list<-list()
stats.list<-c()
for(i in 1:length(comps)){

  tmp<-pp2[pp2$NTrt %in% c("1 1",comps[i]),]
  tmp$NTrt<-factor(as.character(tmp$NTrt),levels=c("1 1",comps[i]))

  plot.list[[i]]<-leap.zig(tmp,type='cafe',
                           main=paste("(",comps[i],")",sep=""),
           xlim=c(0,25),ylim=c(-80,400),error.bars=T,vectors=T,
           legend=F)

  tst<-test.partitions(tmp,type='cafe',treat.var = "NTrt",control = '1 1',print=F,plot=F)

  tst<-cbind(comps=rep(comps[i],nrow(tst)),tst)

  stats.list<-rbind(stats.list,tst)
}

# Look at the entire sequence of vector plots:
grid.arrange(plot.list[[1]],plot.list[[2]],plot.list[[3]],
             plot.list[[4]],plot.list[[5]],plot.list[[6]],
             plot.list[[7]],nrow=1)

# Stash this object for exporting:
g1<-arrangeGrob(plot.list[[1]],plot.list[[2]],plot.list[[3]],
             plot.list[[4]],plot.list[[5]],plot.list[[6]],
             plot.list[[7]],nrow=1)

# Save graphic to Dropbox:
pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_cafe_plots.pdf",sep="")
scl<-2
ggsave(file=pth,g1,width = scl*9, height = scl*2)


# Combine model outputs, make some plots
t1<-as.numeric(ifelse(stats.list$mn.pvals=="<0.001",0.001,stats.list$mn.pvals))
stats.list$mn.cols<-ifelse(t1<=0.01,'p<=0.01','n.s.')

lk<-unique(data2[,c('NAdd','NTrt')])
lk<-lk[order(lk$NTrt),]
lk$comps<-paste("1",lk$NTrt)
stats.list<-merge(stats.list,lk)
stats.list$variable<-factor(stats.list$variable,
                            levels=c('SL','s.loss','SL.slope','SL.mag',
                                     'SG','s.gain','SG.slope','SG.mag',
                                     'CDE','Total'))

# Plot each individual component of changes in function and richness, corresponding to the CAFE paradigm.
h3<-ggplot(stats.list,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,colour='gray')+
  geom_point(aes(colour=mn.cols))+
  geom_hline(yintercept = 0)+
  facet_wrap(~variable,scales="free_y",ncol=4)+
  scale_color_manual(values=c('red','black'))+
  theme_bw()
h3


h3<-ggplot(stats.list[!(stats.list$variable %in% c('SL.slope','SL.mag','SG.slope','SG.mag')),],aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,colour='gray')+
  geom_point(aes(colour=mn.cols))+
  geom_hline(yintercept = 0)+
  facet_wrap(~variable,scales="free_y",ncol=2)+
  scale_color_manual(values=c('red','black'))+
  theme_bw()
h3

head(stats.list)


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_cafe_components_vs_NAdd.pdf",sep="")
scl<-2.5
ggsave(file=pth,h3,width = scl*4, height = scl*2.5)




bob<-stats.list[!(stats.list$variable %in% c('SL.slope','SL.mag','SG.slope','SG.mag','s.loss','s.gain')),]

h4<-ggplot(bob,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  geom_hline(yintercept = 0,colour='gray')+
  stat_smooth(method="lm", formula = y~ns(x,3),se = F,
              aes(colour=variable),lwd=0.5)+
  geom_point(aes(colour=variable,shape=mn.cols,fill=mn.cols),
             stroke=1,size=2)+
  scale_color_manual(values=gg_color_hue(6))+
  scale_shape_manual(values=c(22,23))+
  scale_fill_manual(values=c('white','black'))+
  scale_y_continuous('(ctrl-trt mean) - (ctrl-ctrl mean)')+
  scale_x_continuous('Nitrogen added (g/m2/yr)')+
  theme_bw()
h4

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_cafe_components_vs_NAdd_single_panel.pdf",sep="")
scl<-1.1
ggsave(file=pth,h4,width = scl*4, height = scl*3.5)



# re-jigger the names:
stats.list2<-stats.list
names(stats.list2)[names(stats.list2)=="variable"]<-"component"

sl2<-melt(stats.list2[,!(names(stats.list2) %in% c('delta.var','var.pvals'))],id.vars=c('comps','NAdd','NTrt','component','mn.pvals','mn.cols'))
sl2$mn.cols<-ifelse(sl2$variable=='ctrl.mean',NA,sl2$mn.cols)


bob2<-sl2[!(sl2$component %in% c('SL.slope','SL.mag','SG.slope','SG.mag')) & sl2$variable!='delta.mean',]
bob2$component<-factor(bob2$component,levels=c('SL','SG','CDE','Total','s.loss','s.gain'))

h5<-ggplot(bob2,
           aes(x=NAdd,y=value,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,aes(colour=component,linetype=variable),lwd=0.5)+
  facet_wrap(~component,scales="free_y",ncol=2)+
  geom_point(aes(colour=component,shape=mn.cols,fill=mn.cols),size=2,stroke=1)+
  scale_fill_manual(values=c('white','black'))+
  scale_shape_manual(values=c(22,23))+
  theme_bw()
h5

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_cafe_components_vs_NAdd_multi_panel.pdf",sep="")
scl<-1.8
ggsave(file=pth,h5,width = scl*3, height = scl*2.5)






##########################

# via BEF:

comps<-levels(pp2$NTrt)
comps<-comps[-1]

plot.list<-list()
stats.list<-c()
for(i in 1:length(comps)){

  tmp<-pp2[pp2$NTrt %in% c("1 1",comps[i]),]
  tmp$NTrt<-factor(as.character(tmp$NTrt),levels=c("1 1",comps[i]))

  plot.list[[i]]<-leap.zig(tmp,type='bef',
                           main=paste("(",comps[i],")",sep=""),
                           xlim=c(3,22),ylim=c(-80,400),error.bars=T,vectors=T,
                           legend=F)

  tst<-test.partitions(tmp,type='bef',treat.var = "NTrt",control = '1 1',print=F,plot=F)

  tst<-cbind(comps=rep(comps[i],nrow(tst)),tst)

  stats.list<-rbind(stats.list,tst)
}


grid.arrange(plot.list[[1]],plot.list[[2]],plot.list[[3]],
             plot.list[[4]],plot.list[[5]],plot.list[[6]],
             plot.list[[7]],nrow=1)

g1<-arrangeGrob(plot.list[[1]],plot.list[[2]],plot.list[[3]],
                plot.list[[4]],plot.list[[5]],plot.list[[6]],
                plot.list[[7]],nrow=1)

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_bef_plots.pdf",sep="")
scl<-2
ggsave(file=pth,g1,width = scl*9, height = scl*2)


# combine model outputs, make some plots

t1<-as.numeric(ifelse(stats.list$mn.pvals=="<0.001",0.001,stats.list$mn.pvals))
stats.list$mn.cols<-ifelse(t1<=0.01,'p<=0.01','n.s.')

lk<-unique(data2[,c('NAdd','NTrt')])
lk<-lk[order(lk$NTrt),]
lk$comps<-paste("1",lk$NTrt)
stats.list<-merge(stats.list,lk)
stats.list$variable<-factor(stats.list$variable,
                            levels=c('SR','s.change','CE','Total'))


h3<-ggplot(stats.list,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,colour='gray')+
  geom_point(aes(colour=mn.cols))+
  geom_hline(yintercept = 0)+
  facet_wrap(~variable,scales="free_y",ncol=2)+
  scale_color_manual(values=c('red','black'))+
  theme_bw()
h3


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_bef_components_vs_NAdd.pdf",sep="")
scl<-2.5
ggsave(file=pth,h3,width = scl*4, height = scl*4)


head(stats.list)
bob<-stats.list[!(stats.list$variable %in% c('s.change')),]
bob$variable<-factor(bob$variable,levels=c('CE','SR','Total'))
levels(bob$variable)

h4<-ggplot(bob,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  geom_hline(yintercept = 0,colour='gray')+
  stat_smooth(method="lm", formula = y~ns(x,3),se = F,
              aes(colour=variable),lwd=0.5)+
  geom_point(aes(colour=variable,shape=mn.cols,fill=mn.cols),
             stroke=1,size=2)+
  scale_color_manual(values=gg_color_hue(4))+
  scale_shape_manual(values=c(22,23))+
  scale_fill_manual(values=c('white','black'))+
  scale_y_continuous('(ctrl-trt mean) - (ctrl-ctrl mean)')+
  scale_x_continuous('Nitrogen added (g/m2/yr)')+
  theme_bw()
h4

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_bef_components_vs_NAdd_single_panel.pdf",sep="")
scl<-1.1
ggsave(file=pth,h4,width = scl*4, height = scl*3.5)



# re-jigger the names:
stats.list2<-stats.list
names(stats.list2)[names(stats.list2)=="variable"]<-"component"

sl2<-melt(stats.list2[,!(names(stats.list2) %in% c('delta.var','var.pvals'))],id.vars=c('comps','NAdd','NTrt','component','mn.pvals','mn.cols'))
sl2$mn.cols<-ifelse(sl2$variable=='ctrl.mean',NA,sl2$mn.cols)


bob2<-sl2[sl2$variable!='delta.mean',]
bob2$component<-factor(bob2$component,levels=c('CE','SR','Total','s.change'))

h5<-ggplot(bob2,
           aes(x=NAdd,y=value,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,aes(colour=component,linetype=variable),lwd=0.5)+
  facet_wrap(~component,scales="free_y",ncol=2)+
  geom_point(aes(colour=component,shape=mn.cols,fill=mn.cols),size=2,stroke=1)+
  scale_fill_manual(values=c('white','black'))+
  scale_shape_manual(values=c(22,23))+
  theme_bw()
h5



pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_bef_components_vs_NAdd_multi_panel.pdf",sep="")
scl<-1.8
ggsave(file=pth,h5,width = scl*3, height = scl*2.5)





##########################

# via Price:

comps<-levels(pp2$NTrt)
comps<-comps[-1]

plot.list<-list()
stats.list<-c()
for(i in 1:length(comps)){

  tmp<-pp2[pp2$NTrt %in% c("1 1",comps[i]),]
  tmp$NTrt<-factor(as.character(tmp$NTrt),levels=c("1 1",comps[i]))

  plot.list[[i]]<-leap.zig(tmp,type='price',
                           main=paste("(",comps[i],")",sep=""),
                           xlim=c(3,22),ylim=c(-80,400),error.bars=T,vectors=T,
                           legend=F)

  tst<-test.partitions(tmp,type='price',treat.var = "NTrt",control = '1 1',print=F,plot=F)

  tst<-cbind(comps=rep(comps[i],nrow(tst)),tst)

  stats.list<-rbind(stats.list,tst)
}


grid.arrange(plot.list[[1]],plot.list[[2]],plot.list[[3]],
             plot.list[[4]],plot.list[[5]],plot.list[[6]],
             plot.list[[7]],nrow=1)

g1<-arrangeGrob(plot.list[[1]],plot.list[[2]],plot.list[[3]],
                plot.list[[4]],plot.list[[5]],plot.list[[6]],
                plot.list[[7]],nrow=1)

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_price_plots.pdf",sep="")
scl<-2
ggsave(file=pth,g1,width = scl*9, height = scl*2)


# combine model outputs, make some plots

t1<-as.numeric(ifelse(stats.list$mn.pvals=="<0.001",0.001,stats.list$mn.pvals))
stats.list$mn.cols<-ifelse(t1<=0.01,'p<=0.01','n.s.')

lk<-unique(data2[,c('NAdd','NTrt')])
lk<-lk[order(lk$NTrt),]
lk$comps<-paste("1",lk$NTrt)
stats.list<-merge(stats.list,lk)
stats.list$variable<-factor(stats.list$variable,
                            levels=c('SRE.L','SCE.L','s.loss','SRE.G','SCE.G','s.gain','CDE','Total'))


h3<-ggplot(stats.list,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,colour='gray')+
  geom_point(aes(colour=mn.cols))+
  geom_hline(yintercept = 0)+
  facet_wrap(~variable,scales="free_y",ncol=3)+
  scale_color_manual(values=c('red','black'))+
  theme_bw()
h3


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_price_components_vs_NAdd.pdf",sep="")
scl<-2.5
ggsave(file=pth,h3,width = scl*4, height = scl*4)




head(stats.list)
bob<-stats.list[!(stats.list$variable %in% c('s.loss','s.gain')),]
bob$variable<-factor(bob$variable,levels=c('SRE.L','SRE.G','SCE.L','SCE.G','CDE','Total'))
levels(bob$variable)

h4<-ggplot(bob,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  geom_hline(yintercept = 0,colour='gray')+
  stat_smooth(method="lm", formula = y~ns(x,3),se = F,
              aes(colour=variable),lwd=0.5)+
  geom_point(aes(colour=variable,shape=mn.cols,fill=mn.cols),
             stroke=1,size=2)+
  scale_color_manual(values=gg_color_hue(8))+
  scale_shape_manual(values=c(22,23))+
  scale_fill_manual(values=c('white','black'))+
  scale_y_continuous('(ctrl-trt mean) - (ctrl-ctrl mean)')+
  scale_x_continuous('Nitrogen added (g/m2/yr)')+
  theme_bw()
h4

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_price_components_vs_NAdd_single_panel.pdf",sep="")
scl<-1.1
ggsave(file=pth,h4,width = scl*4, height = scl*3.5)



# re-jigger the names:
stats.list2<-stats.list
names(stats.list2)[names(stats.list2)=="variable"]<-"component"

sl2<-melt(stats.list2[,!(names(stats.list2) %in% c('delta.var','var.pvals'))],id.vars=c('comps','NAdd','NTrt','component','mn.pvals','mn.cols'))
sl2$mn.cols<-ifelse(sl2$variable=='ctrl.mean',NA,sl2$mn.cols)

bob2<-sl2[sl2$variable!='delta.mean',]
bob2$component<-factor(bob2$component,levels=c('SRE.L','SRE.G','SCE.L','SCE.G','CDE','Total','s.loss','s.gain'))

h5<-ggplot(bob2,
           aes(x=NAdd,y=value,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,aes(colour=component,linetype=variable),lwd=0.5)+
  facet_wrap(~component,scales="free_y",ncol=2)+
  geom_point(aes(colour=component,shape=mn.cols,fill=mn.cols),size=2,stroke=1)+
  scale_fill_manual(values=c('white','black'))+
  scale_shape_manual(values=c(22,23))+
  theme_bw()
h5


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_price_components_vs_NAdd_multi_panel.pdf",sep="")
scl<-1.8
ggsave(file=pth,h5,width = scl*3, height = scl*3.5)


##################


head(data2)
table(data2$Exp)

jim <- data2 %>% group_by(NTrt,NAdd,Field,Plot) %>%
                  summarise(S=length(unique(Species)),Biomass=sum(Biomass))

jim2<-melt(jim,id.vars=c('NAdd'),measure.vars = c('S','Biomass'))
jim2

plot(S~NAdd,data=jim,pch=21,bg=alpha('black',0.3))
plot(Biomass~NAdd,data=jim,pch=21,bg=alpha('black',0.3))


g1<-ggplot(jim2,aes(x=NAdd,y=value,variable))+
      geom_point()+
      stat_smooth(method='lm',formula = y~ns(x,3))+
      facet_wrap(~variable,scales = 'free_y')+
      theme_bw()
g1

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_general_plots.pdf",sep="")
scl<-1.8
ggsave(file=pth,g1,width = scl*3.3, height = scl*2.2)



head(pp2)

plot(c.rich~NAdd,data=pp2)
plot(c.rich~NTrt,data=pp2)
plot(c.rich~y.rich,data=pp2,pch=21,bg=alpha('black',0.1))

pp2$NO3<-as.numeric(sapply(as.character(pp2$NAdd), function(x) strsplit(x, split=" ")[[1]][2]))

plot(c.rich~NO3,data=pp2,pch=21,bg=alpha('black',0.1))


g2<-ggplot(pp2,aes(x=NO3,y=c.rich))+
        geom_point()+
        stat_smooth(method="lm",formula = y~ns(x,3),color='red')+
        theme_bw()
g2

lm1<-lm(c.rich~NO3,data=pp2)
plot(c.rich~NO3,data=pp2,pch=21,bg=alpha('black',0.1))
abline(lm1,col='red')
summary(lm1)

plot(CDE~c.rich,data=pp2,pch=21,bg=alpha('black',0.1),col=NA)






########### Now 1982 ######################

# pull up the earlier data:

# just year 1982
cdr82<-subset(cdr, cdr$Year==1982)

# just field D
cdr82D<-subset(cdr82, cdr82$Field=="D")


# focus on full range of nitrogen additions, excluding treatment 9 (which I don't understand)
data4<- cdr82D %>% filter(NTrt != 9)

# Define a set of grouping variables that organize the data,
group.vars<-c('Plot')
treat.vars<-c('NTrt','NAdd')
grouped.data4 <- data4 %>% group_by_(.dots=c(group.vars,treat.vars))

# calculate pairwise comparisons of sampled communities using the price equation:
# - self-comparisons of the same community are automatically excluded
res4<- pairwise.price(grouped.data4,species="Species",func="Biomass")

# Save a copy of this output, and prepare it for analysis:
pp4<-res4

# retain only comparisons against NTrt.x = 1
pp4<-pp4[pp4$NTrt.x==1,]

# create a single column keeping track of the paired set of treatments & other grouping variables:
pp4<-group.columns(pp4,gps=c(group.vars,treat.vars),drop=T)
head(pp4)
dim(pp4)



##########################

# via Price:

comps<-levels(pp4$NTrt)
comps<-comps[-1]

plot.list<-list()
stats.list<-c()
for(i in 1:length(comps)){

  tmp<-pp4[pp4$NTrt %in% c("1 1",comps[i]),]
  tmp$NTrt<-factor(as.character(tmp$NTrt),levels=c("1 1",comps[i]))

  plot.list[[i]]<-leap.zig(tmp,type='price',
                           main=paste("(",comps[i],")",sep=""),
                           xlim=c(3,22),ylim=c(-80,400),error.bars=T,vectors=T,
                           legend=F)

  tst<-test.partitions(tmp,type='price',treat.var = "NTrt",control = '1 1',print=F,plot=F)

  tst<-cbind(comps=rep(comps[i],nrow(tst)),tst)

  stats.list<-rbind(stats.list,tst)
}


# combine model outputs, make some plots

t1<-as.numeric(ifelse(stats.list$mn.pvals=="<0.001",0.001,stats.list$mn.pvals))
stats.list$mn.cols<-ifelse(t1<=0.01,'p<=0.01','n.s.')

lk<-unique(data2[,c('NAdd','NTrt')])
lk<-lk[order(lk$NTrt),]
lk$comps<-paste("1",lk$NTrt)
stats.list<-merge(stats.list,lk)
stats.list$variable<-factor(stats.list$variable,
                            levels=c('SRE.L','SCE.L','s.loss','SRE.G','SCE.G','s.gain','CDE','Total'))


h3<-ggplot(stats.list,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,colour='gray')+
  geom_point(aes(colour=mn.cols))+
  geom_hline(yintercept = 0)+
  facet_wrap(~variable,scales="free_y",ncol=3)+
  scale_color_manual(values=c('red','black'))+
  theme_bw()
h3


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_1982_price_components_vs_NAdd.pdf",sep="")
scl<-2.5
ggsave(file=pth,h3,width = scl*4, height = scl*4)


head(stats.list)
bob<-stats.list[!(stats.list$variable %in% c('s.loss','s.gain')),]
bob$variable<-factor(bob$variable,levels=c('SRE.L','SRE.G','SCE.L','SCE.G','CDE','Total'))
levels(bob$variable)

h4<-ggplot(bob,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  geom_hline(yintercept = 0,colour='gray')+
  stat_smooth(method="lm", formula = y~ns(x,3),se = F,
              aes(colour=variable),lwd=0.5)+
  geom_point(aes(colour=variable,shape=mn.cols,fill=mn.cols),
             stroke=1,size=2)+
  scale_color_manual(values=gg_color_hue(8))+
  scale_shape_manual(values=c(22,23))+
  scale_fill_manual(values=c('white','black'))+
  scale_y_continuous('(ctrl-trt mean) - (ctrl-ctrl mean)')+
  scale_x_continuous('Nitrogen added (g/m2/yr)')+
  theme_bw()
h4

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_1982_price_components_vs_NAdd_single_panel.pdf",sep="")
scl<-1.1
ggsave(file=pth,h4,width = scl*4, height = scl*3.5)



# re-jigger the names:
stats.list2<-stats.list
names(stats.list2)[names(stats.list2)=="variable"]<-"component"

sl2<-melt(stats.list2[,!(names(stats.list2) %in% c('delta.var','var.pvals'))],id.vars=c('comps','NAdd','NTrt','component','mn.pvals','mn.cols'))
sl2$mn.cols<-ifelse(sl2$variable=='ctrl.mean',NA,sl2$mn.cols)

bob2<-sl2[sl2$variable!='delta.mean',]
bob2$component<-factor(bob2$component,levels=c('SRE.L','SRE.G','SCE.L','SCE.G','CDE','Total','s.loss','s.gain'))

h5<-ggplot(bob2,
           aes(x=NAdd,y=value,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,aes(colour=component,linetype=variable),lwd=0.5)+
  facet_wrap(~component,scales="free_y",ncol=2)+
  geom_point(aes(colour=component,shape=mn.cols,fill=mn.cols),size=2,stroke=1)+
  scale_fill_manual(values=c('white','black'))+
  scale_shape_manual(values=c(22,23))+
  theme_bw()
h5


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_1982_price_components_vs_NAdd_multi_panel.pdf",sep="")
scl<-1.8
ggsave(file=pth,h5,width = scl*3, height = scl*3.5)






########### Now 1983 ######################

# pull up the earlier data:

# just year 1983
cdr83<-subset(cdr, cdr$Year==1983)

# just field D
cdr83D<-subset(cdr83, cdr83$Field=="D")


# focus on full range of nitrogen additions, excluding treatment 9 (which I don't understand)
data3<- cdr83D %>% filter(NTrt != 9)

# Define a set of grouping variables that organize the data,
group.vars<-c('Plot')
treat.vars<-c('NTrt','NAdd')
grouped.data3 <- data3 %>% group_by_(.dots=c(group.vars,treat.vars))

# calculate pairwise comparisons of sampled communities using the price equation:
# - self-comparisons of the same community are automatically excluded
res3<- pairwise.price(grouped.data3,species="Species",func="Biomass")

# Save a copy of this output, and prepare it for analysis:
pp3<-res3

# retain only comparisons against NTrt.x = 1
pp3<-pp3[pp3$NTrt.x==1,]

# create a single column keeping track of the paired set of treatments & other grouping variables:
pp3<-group.columns(pp3,gps=c(group.vars,treat.vars),drop=T)
head(pp3)
dim(pp3)



##########################

# via Price:

comps<-levels(pp3$NTrt)
comps<-comps[-1]

plot.list<-list()
stats.list<-c()
for(i in 1:length(comps)){

  tmp<-pp3[pp3$NTrt %in% c("1 1",comps[i]),]
  tmp$NTrt<-factor(as.character(tmp$NTrt),levels=c("1 1",comps[i]))

  plot.list[[i]]<-leap.zig(tmp,type='price',
                           main=paste("(",comps[i],")",sep=""),
                           xlim=c(3,22),ylim=c(-80,400),error.bars=T,vectors=T,
                           legend=F)

  tst<-test.partitions(tmp,type='price',treat.var = "NTrt",control = '1 1',print=F,plot=F)

  tst<-cbind(comps=rep(comps[i],nrow(tst)),tst)

  stats.list<-rbind(stats.list,tst)
}


# combine model outputs, make some plots

t1<-as.numeric(ifelse(stats.list$mn.pvals=="<0.001",0.001,stats.list$mn.pvals))
stats.list$mn.cols<-ifelse(t1<=0.01,'p<=0.01','n.s.')

lk<-unique(data2[,c('NAdd','NTrt')])
lk<-lk[order(lk$NTrt),]
lk$comps<-paste("1",lk$NTrt)
stats.list<-merge(stats.list,lk)
stats.list$variable<-factor(stats.list$variable,
                            levels=c('SRE.L','SCE.L','s.loss','SRE.G','SCE.G','s.gain','CDE','Total'))


h3<-ggplot(stats.list,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,colour='gray')+
  geom_point(aes(colour=mn.cols))+
  geom_hline(yintercept = 0)+
  facet_wrap(~variable,scales="free_y",ncol=3)+
  scale_color_manual(values=c('red','black'))+
  theme_bw()
h3


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001_1983_price_components_vs_NAdd.pdf",sep="")
scl<-2.5
ggsave(file=pth,h3,width = scl*4, height = scl*4)


head(stats.list)
bob<-stats.list[!(stats.list$variable %in% c('s.loss','s.gain')),]
bob$variable<-factor(bob$variable,levels=c('SRE.L','SRE.G','SCE.L','SCE.G','CDE','Total'))
levels(bob$variable)

h4<-ggplot(bob,aes(x=NAdd,y=delta.mean,variable,mn.cols))+
  geom_hline(yintercept = 0,colour='gray')+
  stat_smooth(method="lm", formula = y~ns(x,3),se = F,
              aes(colour=variable),lwd=0.5)+
  geom_point(aes(colour=variable,shape=mn.cols,fill=mn.cols),
             stroke=1,size=2)+
  scale_color_manual(values=gg_color_hue(8))+
  scale_shape_manual(values=c(22,23))+
  scale_fill_manual(values=c('white','black'))+
  scale_y_continuous('(ctrl-trt mean) - (ctrl-ctrl mean)')+
  scale_x_continuous('Nitrogen added (g/m2/yr)')+
  theme_bw()
h4

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_1983_price_components_vs_NAdd_single_panel.pdf",sep="")
scl<-1.1
ggsave(file=pth,h4,width = scl*4, height = scl*3.5)



# re-jigger the names:
stats.list2<-stats.list
names(stats.list2)[names(stats.list2)=="variable"]<-"component"

sl2<-melt(stats.list2[,!(names(stats.list2) %in% c('delta.var','var.pvals'))],id.vars=c('comps','NAdd','NTrt','component','mn.pvals','mn.cols'))
sl2$mn.cols<-ifelse(sl2$variable=='ctrl.mean',NA,sl2$mn.cols)

bob2<-sl2[sl2$variable!='delta.mean',]
bob2$component<-factor(bob2$component,levels=c('SRE.L','SRE.G','SCE.L','SCE.G','CDE','Total','s.loss','s.gain'))

h5<-ggplot(bob2,
           aes(x=NAdd,y=value,variable,mn.cols))+
  stat_smooth(method="lm", formula = y~ns(x,3),
              se = F,aes(colour=component,linetype=variable),lwd=0.5)+
  facet_wrap(~component,scales="free_y",ncol=2)+
  geom_point(aes(colour=component,shape=mn.cols,fill=mn.cols),size=2,stroke=1)+
  scale_fill_manual(values=c('white','black'))+
  scale_shape_manual(values=c(22,23))+
  theme_bw()
h5


pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Analyses/CedarCreek/e001/e001_1983_price_components_vs_NAdd_multi_panel.pdf",sep="")
scl<-1.8
ggsave(file=pth,h5,width = scl*3, height = scl*3.5)



