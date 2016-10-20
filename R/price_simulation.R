
######################## Price Simulation Code ########################

### Function for simulating community data to analyze with price partition

draw.sps<-function(nsp,fnc,ids,beta){
  #fitness.vals<-expit((beta)*(fnc-mean(fnc)))
  fitness.vals<-expit((beta/sd(fnc))*(fnc-mean(fnc)))
  weights<- fitness.vals/sum(fitness.vals)
  
  #sample(fnc,nsp,prob=weights)
  sample(ids,nsp,prob=weights)
}

price.sim<-function(pars){
  
  # set pars:
  x.mn.sp<-pars[1]
  x.sd.sp<-pars[2]
  n.x<-pars[3]
  y.mn.sp<-pars[4]
  y.sd.sp<-pars[5]
  lambda.L<-pars[6]
  lambda.G<-pars[7]
  beta.L<-pars[8]
  beta.G<-pars[9]
  chng.mn<-pars[10]
  chng.sd<-pars[11]
  
  # (1) Draw x community
  x.func<-rnorm(n.x,mean=x.mn.sp,sd=x.sd.sp)
  x.id<-seq(1,n.x)
  
  # (2) Determine number of sps lost and gained
  x.loss<-rpois(1,lambda=lambda.L)
  x.loss<-ifelse(x.loss > n.x, n.x, x.loss)	# protect against killing more species than we have. (maybe use a binomial distribution instead? avoids this risk...)
  x.gain<-rpois(1,lambda=lambda.G)
  
  # (3) Figure out which sps are lost
  x.sps.lost<-draw.sps(nsp=x.loss,fnc=x.func,
                       ids=x.id,beta=beta.L)	# draw ids
  x.func.new<-x.func[-x.sps.lost]			# save old functions for retained sps
  x.id.new<-x.id[-x.sps.lost]				# save ids for retained sps
  
  # (4) Figure out change in function
  x.func.new<-x.func.new + rnorm(length(x.id.new),mean=chng.mn,sd=chng.sd)
  
  # (5) Figure out which sps are gained
  y.func.pool<-rnorm(n=1000,mean=y.mn.sp,sd=y.sd.sp)	# simulate species pool
  x.sps.gained<-draw.sps(nsp=x.gain,fnc=y.func.pool,
                         ids=seq(1,length(y.func.pool)),beta=beta.G)	# draw sps id
  
  x.func.new<-c(x.func.new,y.func.pool[x.sps.gained])
  if(x.gain>0){
    x.id.new<-c(x.id.new,n.x+seq(1,x.gain))
  }
  
  # (6) Assemble the data frame
  comm<-merge(data.frame(id=x.id,x.func),
              data.frame(id=x.id.new,x.func.new),all=T)
  names(comm)<-c('id','x.func','y.func')
  
  
  ## Run Price partition:
  comm2<-data.setup(list(comm))
  res<-price.part(comm2,quiet=T)
  
  return(res)
}

