############################

### sCAFE Marine Algae Invasion case study:
# KBM
# Last modified 29 June 2018

# Updates: cleaned up/extra sections removed, to post to github, following
# a request for code.

############################

library(dplyr)
library(broom)
library(ggplot2)
library(gridExtra)

# Load priceTools package:
library(priceTools)

data<-read.csv(paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Data/Edited/BEF_CAFE_Lite/Marine_algae_invaders.csv",sep=""),stringsAsFactors = F)

# grouping:
grouped.data<- data %>% group_by(Treatment)
head(grouped.data)

# Run pairwise comparisons:
res1<- pairwise.price(grouped.data,species="Species",func="MeanBiomass")
head(res1)

# Format results
pp1<-res1
pp1$Treatment<-paste(pp1$Treatment.x, pp1$Treatment.y, sep=' ') 

# Select comparisons between plots where invasive alga was present vs. removed as the focal comparisons to plot:
pp1<-pp1[pp1$Treatment %in% c('present removed'),] #use removal as the treatment variable, present as the control variable
head(pp1)

# Generate visuals (Fig. 4b) of the consequences of removing invasive alga,
# using all three configurations of the Price equation components:

s1<-leap.zig(pp1,type='bef',standardize=FALSE,xlim=c(10,30),
             ylim=c(0,3500),error.bars=F,
             vectors=T,raw.points = F,legend=TRUE)+ 
  scale_y_continuous("Ecosystem function (wet biomass (g))",
                     breaks=c(0,500,1000,1500,2000,2500,3000,3500))+
  annotate("text", x = mean(pp1$x.rich), y = mean(pp1$x.func), 
           label = "*",size = 8)+
  annotate("segment", x = mean(pp1$y.rich)-1, xend = mean(pp1$y.rich)+1, 
           y = mean(pp1$y.func),yend = mean(pp1$y.func),colour = "black")+
  theme_bw()+
  ggtitle("Richness-Composition")

s2<-leap.zig(pp1,type='cafe',standardize=FALSE,xlim=c(10,30),
             ylim=c(0,3500),error.bars=F,
             vectors=T,raw.points = F,legend=TRUE)+
  scale_y_continuous("",breaks=c(0,500,1000,1500,2000,2500,3000,3500))+
  annotate("text", x = mean(pp1$x.rich), y = mean(pp1$x.func), 
           label = "*",size = 8)+
  annotate("segment", x = mean(pp1$y.rich)-1, xend = mean(pp1$y.rich)+1, 
           y = mean(pp1$y.func),yend = mean(pp1$y.func),colour = "black")+
  theme_bw()+
  ggtitle("Community Assembly")

s3<-leap.zig(pp1,type='price',standardize=FALSE,xlim=c(10,30),
             ylim=c(0,3500),error.bars=F,
             vectors=T,raw.points = F,legend=TRUE)+
  scale_y_continuous("",breaks=c(0,500,1000,1500,2000,2500,3000,3500))+
  annotate("text", x = mean(pp1$x.rich), y = mean(pp1$x.func), 
           label = "*",size = 8)+
  annotate("segment", x = mean(pp1$y.rich)-1, xend = mean(pp1$y.rich)+1, 
           y = mean(pp1$y.func),yend = mean(pp1$y.func),colour = "black")+
  theme_bw()+
  ggtitle("5-part Price")

# Combine plots in one graphic: 
grid.arrange(s1+ theme(aspect.ratio=1),s2+ theme(aspect.ratio=1),s3+ theme(aspect.ratio=1),nrow=1)

