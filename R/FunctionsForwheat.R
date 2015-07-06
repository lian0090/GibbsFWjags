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
   VAR=VAR[whselect]
   ENV=ENV[whselect]
   y=y[whselect]
   out$VAR=VAR
   out$ENV=ENV
   out$y=y
   if(!missing(G)){
	G=G[VARselect,VARselect]
	return(list(dat=out,G=G))	
 }else{
  	G=G[VARselect,VARselect]
	return(list(dat=out))
  }
  
   
}

sample.missing=function(VAR,ENV,y,pro.missing,seed,G,savedir){
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

data_select=dat
 
 
 
 for(i in 1:ng){
 	data_select=data_select[-which((data_select$ENV %in% ENVlevels[sample(1:nh,nh.remove)]) & (data_select$VAR==VARlevels[i])),]			
 	}
 
lm1.1=lmFW(data_select$y,data_select$VAR,data_select$ENV,savedir=file.path(savedir,"lm"))
lm2.1=GibbsFW(data_select$y,data_select$VAR,data_select$ENV,savedir=file.path(savedir,"GibbsNoV"))$Init1
if(!missing(G)){
	lm2.2=GibbsFW(data_select$y,data_select$VAR,data_select$ENV,VARlevels=colnames(G),A=G,savedir=file.path(savedir,"GibbsV"))$Init1
corr3=summaryCor(dat$y,dat$VAR,dat$ENV,predictedValue=lm2.2)

}else{
	corr3=rep(NA,4)
	}
corr1=summaryCor(dat$y,dat$VAR,dat$ENV,predictedValue=lm1.1)
corr2=summaryCor(dat$y,dat$VAR,dat$ENV,predictedValue=lm2.1)
corr=rbind(corr1,corr2,corr3)
rownames(corr)=c("lm","Gibbs","GibbsV")
return(corr)
	
}