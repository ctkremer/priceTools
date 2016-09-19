


#####################

to.dropbox<-c('/Users/colin/Dropbox/')    # select your own path to dropbox as needed

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Documentation/example_data.csv",sep='')
price.data <- read.csv(pth)

comm <- data.setup(list(price.data))
head(comm)

price.part(comm)


price.part2(comm)


#########################

# Leibold data:

dat<-read.csv('/Users/colin/Dropbox/sCAFE_SharedFolder/sCAFE_Data/Edited/Leibold/sCAFE_data_Leibold.csv',stringsAsFactors = F)

head(dat)

#####

# draw one open and one closed from the same nutrient level...
tmp<-dat[dat$Nutrient.level==1 & dat$openness!='limited',]
tmp$ID<-paste(tmp$Tank,tmp$Jul_Day,tmp$species.pool)

set.seed(13)

tmpX<-tmp[tmp$openness=='open',]
tmpX<-tmpX[tmpX$ID == sample(unique(tmpX$ID),1),]

tmpY<-tmp[tmp$openness=='closed',]
tmpY<-tmpY[tmpY$ID == sample(unique(tmpY$ID),1),]


comm <- data.setup(list(tmpX[,c('accepted.taxa.name','total.bv')],
                        tmpY[,c('accepted.taxa.name','total.bv')]))
comm.tr <- data.setup(list(tmpX[,c('accepted.taxa.name','fin.biovol')],
                           tmpY[,c('accepted.taxa.name','fin.biovol')]))
names(comm.tr)[2:3]<-c('tr.x','tr.y')

# not generalized:
comm.tr$tr.x <- ifelse(comm.tr$tr.x==0,NA,comm.tr$tr.x)
comm.tr$tr.y <- ifelse(comm.tr$tr.y==0,NA,comm.tr$tr.y)

comm.f <- merge(comm,comm.tr)

pc <- price.part2(comm.f)
price.part(comm)

########

comm
comm.f

mod.x <- lm(func.x~tr.x,data=comm.f[comm.f$xvec==1,])
mod.y <- lm(func.y~tr.y,data=comm.f[comm.f$yvec==1,])

# create output data frame given these regressions:
d1<-data.frame(est=predict(mod.x),res=resid(mod.x),a=coef(mod.x)[1],b1=coef(mod.x)[2])
d2<-data.frame(est=predict(mod.y),res=resid(mod.y),a=coef(mod.y)[1],b1=coef(mod.y)[2])
d1$species<-comm.f$species[as.numeric(row.names(d1))]
d2$species<-comm.f$species[as.numeric(row.names(d2))]
d<-merge(d1,d2,all=T,by=c('species'))
d<-merge(d,comm.f[,c('species','tr.x','tr.y')])

d<-d[,c(names(d)[1],sort(names(d)[2:ncol(d)]))]
d

d2<-merge(comm,d,by='species',all = T)

### Compute the 10-part price partition:

wc.bar<-sum(d2$wvec)/sum(d2$xvec)
wc.bar.p<-sum(d2$wvec)/sum(d2$yvec)

# SCE.L
SCE.L.t <- sum((d2$wvec-wc.bar)*(d2$b1.x*d2$tr.x),na.rm=T)
SCE.L.res <- sum(d2$res.x*(d2$wvec-wc.bar),na.rm=T)

SCE.L.t+SCE.L.res
price.part(comm)[3]

# SCE.G
SCE.G.t <- -sum((d2$wvec-wc.bar.p)*(d2$b1.y*d2$tr.y),na.rm=T)
SCE.G.res <- -sum(d2$res.y*(d2$wvec-wc.bar.p),na.rm=T)

SCE.G.t+SCE.G.res
price.part(comm)[4]


# CDE
del.a <- unique(d2$a.y[!is.na(d2$a.y)]) - unique(d2$a.x[!is.na(d2$a.x)])

CDE.t <- sum(d2$wvec*(d2$b1.x*(d2$tr.x-d2$tr.y)),na.rm=T)
CDE.r <- sum(d2$wvec*(del.a + (d2$b1.y-d2$b1.x)*d2$tr.x),na.rm=T)
CDE.txr <- sum(d2$wvec*sum((d2$b1.y-d2$b1.x)*(d2$tr.y-d2$tr.x)),na.rm=T)
CDE.res <- sum(d2$wvec*(d2$res.y-d2$res.x),na.rm=T)

# Not working...
CDE.t + CDE.r + CDE.txr + CDE.res
price.part(comm)[5]









####### OLD

sce.l<-lm(pc[[2]]$SCE.L.list~log10(comm.f$tr.X))
summary(sce.l)
plot(pc[[2]]$SCE.L.list~log10(comm.f$tr.X))
abline(sce.l,col='red')

sce.g<-lm(pc[[2]]$SCE.G.list~log10(comm.f$tr.Y))
summary(sce.g)
plot(pc[[2]]$SCE.G.list~log10(comm.f$tr.Y))
abline(sce.g,col='red')

#### Calculate components: ####

# Loss:
pr.sce.l<-predict(sce.l)
points(pr.sce.l~log10(comm.f$tr.X[comm.f$tr.X!=0]),col='red')

# trait SCE.L?
sum(pr.sce.l)

# residual SCE.L?
sum(na.omit(pc[[2]]$SCE.L.list)-pr.sce.l)

data.frame(x=na.omit(pc[[2]]$SCE.L.list),pr.sce.l,a=na.omit(pc[[2]]$SCE.L.list)-pr.sce.l,b=log10(comm.f$tr.X[comm.f$tr.X!=0]))
abline(0,0)

# Gain:
pr.sce.g<-predict(sce.g)

# trait SCE.G?
sum(pr.sce.g)

# residual SCE.G?
sum(na.omit(pc[[2]]$SCE.G.list)-pr.sce.g)


pc[[1]]
sum(pr.sce.l)
sum(pr.sce.g)



pc[[2]]$SCE.L.list


#############

plot(func.X~tr.X,data=comm.f[comm.f$xvec==1,])

plot(func.Y~tr.Y,data=comm.f[comm.f$yvec==1,])

tmp1<-comm.f[comm.f$xvec==1,]
m1<-lm(func.X~tr.X,data=tmp1)
summary(m1)

tmp2<-comm.f[comm.f$yvec==1,]
m2<-lm(func.Y~tr.Y,data=tmp2)
summary(m2)


tmp1$pr<-predict(m1)
tmp2$pr<-predict(m2)

# SCE.L_tr
# predict(m1)-coef(m1)[[1]]
sce.l.tr <- sum(coef(m1)[[2]] * (tmp1$tr.X) * (tmp1$wvec - mean(tmp1$wvec)))
sum(coef(m1)[[2]] * (tmp1$tr.X) * (tmp1$wvec - mean(tmp1$wvec)))

# SCE.L_res
sce.l.res <- sum(resid(m1) * (tmp1$wvec - mean(tmp1$wvec)))
sum(resid(m1) * (tmp1$wvec - mean(tmp1$wvec)))

sce.l.tr + sce.l.res

pc[[1]]

## Good!


# SCE.G_tr
# predict(m1)-coef(m1)[[1]]
sce.g.tr <- -sum(coef(m2)[[2]] * (tmp2$tr.Y) * (tmp2$wvec - mean(tmp2$wvec)))
sum(coef(m2)[[2]] * (tmp2$tr.Y) * (tmp2$wvec - mean(tmp2$wvec)))

# SCE.G_res
sce.g.res <- -sum(resid(m2) * (tmp2$wvec - mean(tmp2$wvec)))
sum(resid(m2) * (tmp2$wvec - mean(tmp2$wvec)))

sce.g.tr + sce.g.res

pc[[1]]


### CDE?

# CDE.t = 0
sum(tmp2$wvec*coef(m1)[[2]]*(tmp2$tr.Y-tmp2$tr.X))

# CDE.r
sum(sum(tmp2$yvec) * (coef(m2)[[1]]-coef(m1)[[1]]) + (coef(m2)[[2]] - coef(m1)[[2]])*(tmp2$tr.X[tmp2$wvec==1]))

# CDE.rxt
sum(tmp2$wvec*(coef(m2)[[2]] - coef(m1)[[2]])*(tmp2$tr.Y-tmp2$tr.X))

# busted CDE.res
sum(tmp2$wvec * (resid(m2) - resid(m1)))

# would work better as a whole data frame



comm.f

mm<-lm()

pc[[1]]
