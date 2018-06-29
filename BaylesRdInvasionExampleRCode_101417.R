############################

# sCAFE Bayles Rd Harves 2007 Invaded example:

# Effects of Microstegium invasion on forest ecosystem function

############################

# load tools
library(gridExtra)
library(ggplot2)
library(reshape2)
library(MCMCglmm)

# This developing R package & installation details can be found at:
# https://github.com/ctkremer/priceTools
library(priceTools)


# Load data:
data<-read.csv("BaylesRdharvestdata2007_ed.csv",stringsAsFactors = F)

# Grouping data by key treatment/experimental columns:
grouped.data<-data %>% group_by(trt,plot)

# Run pairwise comparisons generating Price components:
res1<- pairwise.price(grouped.data,species="spp",func="mass")

# Organize output:
group.vars<-c('plot')
treat.vars<-c('trt')
pp1<-group.columns(res1,gps=c(group.vars,treat.vars),drop=F)

# Focus on the output where uninvaded plots are compared to either uninvaded (UN) plots or invaded (INV) plots... but not INV to UN or INV to INV
pp1<-pp1[pp1$trt %in% c('UN UN','UN INV'),]

# Separate out the comparisions isolating treatment effects
dat1<-pp1[pp1$trt %in% c('UN INV'),]

# Separate out the comparisons isolating controls
dat1.ctrl<-pp1[pp1$trt %in% c('UN UN'),]


#### Generating plots ####

# Plot vectors associated with treatment effec

# Richness-Composition vectors
s1<-leap.zig(dat1,type='bef',standardize=FALSE,
             xlim=c(10,40),ylim=c(50,300),error.bars=F,
             vectors=T,raw.points = F,legend=TRUE)+
  scale_y_continuous("Ecosystem function \n(biomass (g))",
                     breaks=c(50,100,150,200,250,300))+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+
  annotate("segment", x = mean(dat1$y.rich)-1, xend = mean(dat1$y.rich)+1, 
           y = mean(dat1$y.func), yend = mean(dat1$y.func),colour = "black")+
  ggtitle("Richness-Composition")
s1

# Community assembly vectors
s2<-leap.zig(dat1,type='cafe',standardize=FALSE,
             xlim=c(10,40),ylim=c(50,300),error.bars=F,
             vectors=T,raw.points = F,legend=TRUE)+
  scale_y_continuous("",breaks=c(50,100,150,200,250,300))+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+
  annotate("segment", x = mean(dat1$y.rich)-1, xend = mean(dat1$y.rich)+1, 
           y = mean(dat1$y.func), yend = mean(dat1$y.func),colour = "black")+
  ggtitle("Community Assembly")
s2

# 5-part Price vectors
s3<-leap.zig(dat1,type='price',standardize=FALSE,
             xlim=c(10,40),ylim=c(50,300),error.bars=F,
             vectors=T,raw.points = F,legend=TRUE)+
  scale_y_continuous("",breaks=c(50,100,150,200,250,300))+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+
  annotate("segment", x = mean(dat1$y.rich)-1, xend = mean(dat1$y.rich)+1, 
           y = mean(dat1$y.func), yend = mean(dat1$y.func),colour = "black")+
  ggtitle("5-part Price")
s3

grid.arrange(s1,s2,s3,nrow=2)



#### Statistics on vectors ####

pp1$SL.rich<-pp1$x.rich-pp1$c.rich
pp1$SG.rich<-pp1$y.rich-pp1$c.rich
pp1$trt<-factor(pp1$trt,levels = c('UN UN','UN INV'))

# Built-in testing tools from priceTools package
test.partitions(pp1,treat.var='trt',control='UN UN',type='cafe',standardize=F)

# Direct t-tests with unequal variance:
t.test(SL~trt,data=pp1)
t.test(SG~trt,data=pp1)
t.test(CDE~trt,data=pp1)
t.test(SL.rich~trt,data=pp1)
t.test(SG.rich~trt,data=pp1)


### Develop graphic for Appendix:

# Recast data from wide form to long form, focusing on Community assembly components
m1<-melt(pp1[,names(pp1) %in% c('trt','CDE','SL','SG')],id.vars = 'trt')
m1$variable<-factor(m1$variable,levels=c('SL','SG','CDE'))
m1$trt2<-ifelse(m1$trt=='UN UN','uninvaded','uninvaded vs. \ninvaded')

m2<-melt(pp1[,names(pp1) %in% c('trt','SL.rich','SG.rich')],id.vars = 'trt')
m2$variable<-factor(m2$variable,levels=c('SL.rich','SG.rich'),labels=c('SL','SG'))
m2$trt2<-ifelse(m2$trt=='UN UN','uninvaded','uninvaded vs. \ninvaded')

s2a<-leap.zig(dat1.ctrl,type='cafe',standardize=FALSE,
              xlim=c(10,40),ylim=c(50,300),error.bars=F,
              vectors=T,raw.points = F,legend=TRUE)+
  scale_y_continuous("Ecosystem function",
                     breaks=c(50,100,150,200,250,300))+
  ggtitle("Uninvaded vs. uninvaded")+
  theme_bw()+
  theme(legend.position = 'none')

s2b<-leap.zig(dat1,type='cafe',standardize=FALSE,
              xlim=c(10,40),ylim=c(50,300),error.bars=F,
              vectors=T,raw.points = F,legend=TRUE)+
  scale_y_continuous("",breaks=c(50,100,150,200,250,300))+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+
  annotate("segment", x = mean(dat1$y.rich)-1, xend = mean(dat1$y.rich)+1,
           y = mean(dat1$y.func), yend = mean(dat1$y.func),colour = "black")+
  theme_bw()+
  theme(legend.position='none')+
  ggtitle("Uninvaded vs. invaded")

# Plot distributions of Community Assembly components across individual pairwise comparisons
g2a<-ggplot(m1,aes(x=variable,y=value))+
  geom_boxplot(aes(fill=trt2))+
  geom_hline(yintercept = 0,linetype=2)+
  scale_fill_manual('',values=c('gray','red'))+
  scale_x_discrete('Component')+
  scale_y_continuous('Ecosystem function')+
  theme_bw()+
  theme(legend.position = 'none')

g2b<-ggplot(m2,aes(x=variable,y=value))+
  geom_boxplot(aes(fill=trt2))+
  geom_vline(xintercept=0,linetype=2)+
  scale_fill_manual('',values=c('gray','red'))+
  scale_x_discrete('Component')+
  scale_y_continuous('Richness')+
  theme_bw()+
  theme(legend.position = 'none')

grid.arrange(s2a,s2b,g2a,g2b,nrow=2)

plt<-arrangeGrob(s2a,s2b,g2a,g2b,nrow=2)
ggsave("app2_demo_fig.pdf",plt,width = 7,height=6)




### Multiple membership models ###

# Set up data for analysis
res2<-res1[res1$trt.x=='UN',]
res2$trt.y<-factor(res2$trt.y,levels=c('UN','INV'))
res2$SL.rich<-res2$x.rich-res2$c.rich
res2$SG.rich<-res2$y.rich-res2$c.rich
head(res2)

# Run models

mc1 <- MCMCglmm(fixed = SL ~ trt.y,
                random = ~idv(mult.memb( ~ plot.x + plot.y)),
                data = res2, pr = TRUE,
                nitt = 2000000, burnin = 500000, thin = 750, verbose = FALSE)
summary(mc1)

mc2 <- MCMCglmm(fixed = SG ~ trt.y,
                random = ~idv(mult.memb( ~ plot.x + plot.y)),
                data = res2, pr = TRUE,
                nitt = 2000000, burnin = 500000, thin = 750, verbose = FALSE)
summary(mc2)

mc3 <- MCMCglmm(fixed = CDE ~ trt.y,
                random = ~idv(mult.memb( ~ plot.x + plot.y)),
                data = res2, pr = TRUE,
                nitt = 2000000, burnin = 500000, thin = 750, verbose = FALSE)
summary(mc3)

mc4 <- MCMCglmm(fixed = SL.rich ~ trt.y,
                random = ~idv(mult.memb( ~ plot.x + plot.y)),
                data = res2, pr = TRUE,
                nitt = 2000000, burnin = 500000, thin = 750, verbose = FALSE)
summary(mc4)

mc5 <- MCMCglmm(fixed = SG.rich ~ trt.y,
                random = ~idv(mult.memb( ~ plot.x + plot.y)),
                data = res2, pr = TRUE,
                nitt = 2000000, burnin = 500000, thin = 750, verbose = FALSE)
summary(mc5)




