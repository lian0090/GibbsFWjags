subsetVAR=function(VAR,ENV,y,n.subsetVAR,seed,G){
	out=list()
 if(!missing(seed)){
	set.seed(seed)
	}
	VAR=as.character(VAR)
	ENV=as.character(ENV)
   
   nobs=length(VAR)
   VARlevels=unique(VAR)
   VARselect=unique(VAR)[sample(1:length(VARlevels),n.subsetVAR)]
   whselect=which( VAR %in% VARselect)
   out$VAR=VAR[whselect]
   out$ENV=ENV[whselect]
   out$y=y[whselect]
  
 
     if(!missing(G)){
	out$G=G[VARselect,VARselect]
 
 }
 return(out) 
   
}

sample.missing=function(VAR,ENV,y,pro.missing,seed,G,savedir,runModels=T){
	##VAR should be a vector or character specifying the name of VAR in data
	##ENV should be a vector or character specifying the name of ENV in data
	##y should be a vector or character specifying the name of y in the data
if(!missing(seed)){
	set.seed(seed)
}
VAR=as.character(VAR)
ENV=as.character(ENV)
dat=data.frame(cbind(VAR=VAR,ENV=ENV,y=y),stringsAsFactors=F)
dat$y=as.numeric(y)
nobs=length(y)
VARlevels=unique(VAR)
ng=length(VARlevels)
ENVlevels=unique(ENV)
nh=length(ENVlevels)

#randomly sample pro.missing Environments as missing for each VAR
nh.remove=round(nh*pro.missing)
if(nh.remove<1)nh.remove=1
 datfull=dat
 whichmissing=vector()
 for(i in 1:ng){
 whichmissing=c(whichmissing,which((dat$ENV %in% ENVlevels[sample(1:nh,nh.remove)]) & (dat$VAR==VARlevels[i])))
 }
 train=rep(1,400)
 train[whichmissing]=0
 datfull$train=train
 #for(i in 1:ng){
 #	dat=dat[-which((dat$ENV %in% ENVlevels[sample(1:nh,nh.remove)]) & (dat$VAR==VARlevels[i])),]			
 #	}
 
dat=datfull[-whichmissing,]
save(G,file=file.path(savedir,"G.rda"))
save(datfull,file=file.path(savedir,"datfull.rda"))
save(dat,file=file.path(savedir,"dat.rda"))

if(runModels==T){
lm1.1=FW(dat$y,dat$VAR,dat$ENV,method="OLS")
lm2.1=FW(dat$y,dat$VAR,dat$ENV,saveAt=file.path(savedir,"GibbsI"),method="Gibbs")

	if(!missing(G)){
		lm2.2=FW(dat$y,dat$VAR,dat$ENV,A=G,saveAt=file.path(savedir,"GibbsA"),method="Gibbs")
		corr3=summaryCor(datfull$y,datfull$VAR,datfull$ENV,predictedValue=lm2.2)
	}else{
	corr3=rep(NA,3)
	}
	
corr1=summaryCor(datfull$y,datfull$VAR,datfull$ENV,predictedValue=lm1.1)
corr2=summaryCor(datfull$y,datfull$VAR,datfull$ENV,predictedValue=lm2.1)
corr=rbind(corr1,corr2,corr3)
rownames(corr)=c("lm","Gibbs","GibbsV")
return(corr)

}

	
}