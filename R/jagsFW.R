##this is the baysian implementation with rjags
jagsFW=function(y,VAR,ENV,VARlevels=NULL,ENVlevels=NULL,ph=NULL,df=5,dfg=5,dfh=5,dfb=5,S=NULL,Sg=NULL,Sb=NULL,Sh=NULL,Ainv=NULL,inits=NULL,nchain=1,burnIn=1000,nIter=5000,thin=1,savedir=".",seed=NULL,n.adapt=0){
n=length(y)
var_y=var(y)
if(is.null(S)) S<-0.5*var_y*(df+2)  #S is the scale times df
if(is.null(Sg))Sg<-0.25*var_y*(dfg+2)
if(is.null(Sb))Sb<-0.5*sqrt(var_y)*(dfb+2)   
if(is.null(Sh))Sh<-0.5*sqrt(var_y)*(dfh+2)  

IDEL=getIDEL(VAR,ENV,VARlevels,ENVlevels)
IDE=IDEL$IDE
IDL=IDEL$IDL
VARlevels=IDEL$VARlevels
ENVlevels=IDEL$ENVlevels
if(is.null(ph)){
  ph=0
}else{
  ph=ph[ENVlevels]
  
}
ng=length(unique(IDL))
nh=length(unique(IDE))
data=list(y=y,ng=ng,n=n,IDL=IDL,IDE=IDE)
if(is.null(Ainv)){
  modelfile="IaIb.txt"
  cat("model
{ \n",
      paste(c("df","dfg","dfh","dfb","S","Sg","Sb","Sh"),"<-",c(df,dfg,dfh,dfb,S,Sg,Sb,Sh),"\n",sep=""),
      "
      for (i in 1 : n) {
      y[i] ~ dnorm(mu+g[IDL[i]]+h[IDE[i]]*(1+b[IDL[i]]),tau_e)
      #theta[i]<-h[IDE[i]]*(1+b[IDL[i]])
      }
      for ( i in 1:ng){
      g[i] ~ dnorm(0,tau_g)
      b[i] ~ dnorm(0,tau_b)
      }
      \n",   

paste("h[",c(1:nh),"]", "~" ,"dnorm(", ph,",tau_h)\n",sep=""),

"
##this should be priors, Bugs will use MCMC to sample from the unnormalized posteria.  
mu ~ dunif(-1E-5,1E05)  
tau_g ~ dgamma(dfg/2,Sg/2)
tau_b ~ dgamma(dfb/2,Sb/2)
tau_h ~ dgamma(dfh/2,Sh/2)
tau_e ~ dgamma(df/2,S/2)
var_g <- 1/tau_g
var_b<- 1/tau_b
var_h<-1/tau_h
var_e<-1/tau_e
}",file=modelfile)	
}else{
	data$Ainv=Ainv[VARlevels,VARlevels];
	data$g0=rep(0,ng);
	data$b0=rep(0,ng);
	modelfile="CaCb.txt"
	cat('model
{ 
df<-5
dfg<-5
dfp<-5
dfb<-5
S<-0.5*Vy*(df+2)  
Sg<-0.25*Vy*(dfg+2)
Sb<-0.5*sqrt(Vy)*(dfb+2)  
Sp<-0.5*sqrt(Vy)*(dfp+2)

for (i in 1 : n) {
      y[i] ~ dnorm(mu+g[IDL[i]]+h[IDE[i]]*(1+b[IDL[i]]),tau_e)
      #theta[i]<-h[IDE[i]]*(b[IDL[i]])
      #yhat[i]<-mu+g[IDL[i]]+h[IDE[i]]+theta[i]
      }
      
g[1:ng] ~ dmnorm(g0,Ainv*tau_g)
b[1:ng] ~ dmnorm(b0,Ainv*tau_b)      
   
for (i in 1:nh){
 h[i]~ dnorm(0,tau_h)
}       
##this should be priors, Bugs will use MCMC to sample from the unnormalized posteria.  
   mu ~ dunif(-1E+03,1E+03)  
   tau_g ~ dgamma(dfg/2,Sg/2)
   tau_b ~ dgamma(dfb/2,Sb/2)
   tau_h ~ dgamma(dfp/2,Sp/2)
   tau_e ~ dgamma(df/2,S/2)
   
   var_g <- 1/tau_g
   var_b<- 1/tau_b
   var_h<-1/tau_h
   var_e<-1/tau_e
   }
',file=modelfile)
	}
#save(data,file="data.rda");setwd(datadir)

parameters<-c("mu","g","b","h","var_g","var_b","var_h","var_e")

############################################# 
# initialize
########################################################################################## 
inits=initialize(y,ng=ng,nh=nh,model="jags",inits=inits,seed=seed,nchain=nchain)
jags.m<-jags.model(file=modelfile,data=data,inits=inits,n.chains=length(inits),n.adapt=n.adapt)
lapply(inits,function(a)cat("seed",a$.RNG.seed,"\n"))
#list the sampling order for jags 
fnlist <- function(x,file){ z <- deparse(substitute(x))
                       cat(z, "\n",file=file)
                       nams=names(x) 
                       for (i in seq_along(x) ) cat(nams[i],  x[[i]], "\n",file=file,append=T)}
fnlist(list.samplers(jags.m),file=file.path(savedir,"jagsSampler.txt"))
samps<-coda.samples(jags.m,parameters,n.iter=nIter,thin=thin)
save(samps,file=file.path(savedir,paste("jags_samps.rda",sep="")))
ans=list()
for(i in 1:nchain){
	
	xi=samps[[i]][round(burnIn/thin):round(nIter/thin),,drop=F]
	tmp=apply(xi,2,mean)
	postMean=list()
	h=tmp[paste("h[",1:nh,"]",sep="")]
	b=tmp[paste("b[",1:ng,"]",sep="")]
	g=tmp[paste("g[",1:ng,"]",sep="")]
  names(h)=ENVlevels
  names(g)=VARlevels
  names(b)=VARlevels
	postMean=setFW(g=g,b=b,h=h,y=y,VAR=VAR,ENV=ENV,IDL=IDL,IDE=IDE,VARlevels=VARlevels,ENVlevels=ENVlevels,mu=tmp['mu'],var_g=tmp['var_g'],var_e=tmp['var_e'],var_b=tmp['var_b'],var_h=tmp['var_h'])
	#save(postMean,file=file.path(savedir,paste("postMeanInit",i,".rda",sep="")))
ans[[i]]=postMean
}
names(ans)=paste("Init",c(1:length(inits)),sep="")
file.remove(modelfile)
postMean=ans
class(postMean)=c("postMean","list")
save(postMean,file=file.path(savedir,"postMean_jags.rda"))
return(ans)
#if(length(inits)>1){return(gelman.diag(samps))}
}






