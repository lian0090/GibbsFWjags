subsetVAR=function(VAR,ENV,y,n.subsetVAR,seed,G=NULL,pro.missing,savedir,runModels=T)
{
	out=list()
	if(!missing(seed)){
		set.seed(seed)
	}
	VAR=as.character(VAR)
	ENV=as.character(ENV)
  	VARlevels=unique(VAR)
	VARselect=unique(VAR)[sample(1:length(VARlevels),n.subsetVAR)]
	whselect=which( VAR %in% VARselect)
	VAR=VAR[whselect]
	ENV=ENV[whselect]
	y=y[whselect]
	dat=data.frame(cbind(VAR=VAR,ENV=ENV,y=y),stringsAsFactors=F)
	dat$y=as.numeric(y)
	dat$yNA=dat$y
	if(!is.null(G)){
		G=G[VARselect,VARselect]
		save(G,file=file.path(savedir,"G.rda"))
	}
	nobs=length(y)
	VARlevels=unique(VAR)
	ng=length(VARlevels)
	ENVlevels=unique(ENV)
	nh=length(ENVlevels)
	#randomly sample pro.missing Environments as missing for each VAR
	nh.remove=round(nh*pro.missing)
	if(nh.remove<1)nh.remove=1
	whichmissing=vector()
	if(!missing(seed)){
		set.seed(seed)
	}
	for(i in 1:ng){
 		whichmissing=c(whichmissing,which((dat$ENV %in% ENVlevels[sample(1:nh,nh.remove)]) & (dat$VAR==VARlevels[i])))
 	}
	dat$yNA[whichmissing]=NA
	save(dat,file=file.path(savedir,"dat.rda"))
   
	if(runModels==T){
		OLS=FW(dat$yNA,VAR,ENV,method="OLS")
		save(OLS,file=file.path(savedir,"OLS.rda"))
		GibbsI=FW(dat$yNA,VAR,ENV,saveAt=file.path(savedir,"GibbsI"),method="Gibbs")
        save(GibbsI,file=file.path(savedir,"GibbsI.rda"))
		if(!missing(G)){
			GibbsA=FW(dat$yNA,VAR,ENV,A=G,saveAt=file.path(savedir,"GibbsA"),method="Gibbs")
			save(GibbsA,file=file.path(savedir,"GibbsA.rda"))
			corr3=get_corYhatYmean(GibbsA,yFull=y)
		}else{
			corr3=rep(NA,3)
		}
		corr1=get_corYhatYmean(OLS,yFull=y)
		corr2=get_corYhatYmean(GibbsI,yFull=y)
		corr=rbind(corr1,corr2,corr3)
		rownames(corr)=c("OLS","GibbsI","GibbsA")
		return(corr)
	}
   
}

get_corYhatYmean=function(FWobj,yFull)
{
	for(i in 1:length(FWobj)){
		assign(names(FWobj)[i],FWobj[[i]])
	}
	y=yFull
	if(length(whichNa)>0){
		ymean_train=aggregate(y[-whichNa],by=list(VAR[-whichNa],ENV[-whichNa]),mean)[,3]
		ymean_vali=aggregate(y[whichNa],by=list(VAR[whichNa],ENV[whichNa]),mean)[,3]
		yhat_train=aggregate(yhat[-whichNa,],by=list(VAR[-whichNa],ENV[-whichNa]),mean)[,-c(1:2)]
		yhat_vali=aggregate(yhat[whichNa,],by=list(VAR[whichNa],ENV[whichNa]),mean)[,-c(1:2)]
		corr=c(cor(ymean_train,yhat_train),cor(yhat_vali,ymean_vali))
 	}else{
 		yhat_mean=aggregate(yhat,by=list(VAR,ENV),mean)[,3]
		ymean=aggregate(y,by=list(VAR,ENV),mean)[,3]
		corr=cor(yhat_mean,ymean)
		corr=c(corr,corr)
	}
	names(corr)=c(rep("train",ncol(yhat)),rep("vali",ncol(yhat)))
	return(corr)
}

