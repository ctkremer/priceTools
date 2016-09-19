



dat1<-pp1[pp1$NTrt %in% c('1 8'),]


tmp<-process.data.bef(dat1,group.vars,standardize=T)

leap.zig.bef(tmp, loc.standardize=T, all.vectors=T)
str(tmp)


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

dat$ID<-paste(dat$Nutrient.level,dat$Jul_Day)
#table(dat[,c('Tank','Nutrient.level')])
#table(dat[,c('Nutrient.level','species.pool')])
table(dat[,c('Nutrient.level','openness')])
table(dat[,c('Nutrient.level','openness','species.pool')])

uids<-sort(unique(dat$ID))

sample(uids,size = 1)

targ<-"69 2 b 288"

comm<-dat[dat$ID==targ,]

dat[dat$ID==sample(uids,size = 1),]

# draw one open and one closed from the same nutrient level...
str(tmpX)
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
names(comm.tr)[2:3]<-c('tr.X','tr.Y')

comm.f <- merge(comm,comm.tr)

pc <- price.part2(comm.f)
price.part(comm)




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
