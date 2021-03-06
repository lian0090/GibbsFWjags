require(FW)
require(lme4)

summaryCor=function(y_full,VAR_full,ENV_full,realizedValue,predictedValue){
  ghat=predictedValue$g
  bhat=predictedValue$b
  hhat=predictedValue$h
  
  if(!missing(realizedValue)){
  g=realizedValue$g
  h=realizedValue$h
  if(trueModel=="FW"){
  b=realizedValue$b
  }
  
  }
  VARlevels=predictedValue$VARlevels
  ENVlevels=predictedValue$ENVlevels
  VAR_fitted=predictedValue$VAR
  ENV_fitted=predictedValue$ENV
  y_fitted=predictedValue$y
  ymean_fitted=aggregate(y_fitted,by=list(VAR_fitted,ENV_fitted),mean)
  colnames(ymean_fitted)=c("VAR","ENV","ymean")
  ymean_full=aggregate(y_full,by=list(VAR_full,ENV_full),mean)
   colnames(ymean_full)=c("VAR","ENV","ymean")

  yhat_full=ghat[ymean_full$VAR]+(1+bhat[ymean_full$VAR])*hhat[ymean_full$ENV]
  IDEL_full=paste(ymean_full$VAR,ymean_full$ENV,sep="_")
  IDEL_fitted=paste(ymean_fitted$VAR,ymean_fitted$ENV,sep="_")

  isfitted= (IDEL_full  %in% IDEL_fitted)
  which_predicted=which(!isfitted)
  which_fitted=which(isfitted)
 
  
  ymean_fitted=ymean_full[which_fitted,]
  yhat_fitted=yhat_full[which_fitted]
   
   
  corr1=rep(NA,3)
  names(corr1)=c("ymean_yhat_full","ymean_yhat_fitted","ymean_yhat_predicted") 
  corr1[1]=cor(ymean_full$ymean,yhat_full)
  corr1[2]=cor(ymean_fitted$ymean,yhat_fitted)

  	if(length(which_predicted)>0){
	  ymean_predicted=ymean_full[which_predicted,]
      yhat_predicted=yhat_full[which_predicted]
      corr1[3]=cor(ymean_predicted$ymean,yhat_predicted)
     }
  
  ENVmeantrain=getENVmean(y_fitted,ENV_fitted,ENVlevels)
  ENVmeanfull=getENVmean(y_full,ENV_full,ENVlevels)
    

  
  if(!missing(realizedValue)){
  corr2=rep(NA,6)
  #dat is a data.frame with the IDL and IDE combinations in the data set
	if(trueModel=="FW"){
	corr2[1]=cor(b[VARlevels],bhat[VARlevels])
	}else{corr2[1]=NA}
    corr2[2]=cor(g[VARlevels],ghat[VARlevels])
    corr2[3]=cor(h[ENVlevels],hhat[ENVlevels])
    corr2[4]=cor(h[ENVlevels],ENVmeantrain)
    corr2[5]=cor(h[ENVlevels],ENVmeanfull)
    corr2[6]=cor(ENVmeantrain,ENVmeanfull)
    names(corr2)=c("b_bhat","g_ghat","h_hhat","h_ENVmeantrain","h_ENVmeanfull","ENVmeantrain_ENVmeanfull")
    corr3=rep(NA,4)
    names(corr3)=c("ytrue_yhat_full","ytrue_yhat_fitted","ytrue_yhat_predicted","ytrue_ymean_full")
    trueGxE=realizedValue$trueGxE
    corr3[1]= cor(trueGxE[paste(ymean_full$VAR,ymean_full$ENV,sep=":"),"t"],getyhat(predictedValue,VAR=ymean_full$VAR,ENV=ymean_full$ENV))

    corr3[2]=cor(trueGxE[paste(ymean_fitted$VAR,ymean_fitted$ENV,sep=":"),"t"],getyhat(predictedValue,VAR=ymean_fitted$VAR,ENV=ymean_fitted$ENV))
    
	if(length(which_predicted)>0){
	corr3[3]=cor(trueGxE[paste(ymean_predicted$VAR,ymean_predicted$ENV,sep=":"),"t"],getyhat(predictedValue,VAR=ymean_predicted$VAR,ENV=ymean_predicted$ENV))
	}
	##correlation between ymean and ytrue, so what we can see which is a better thing to use represent correct yhat
	corr3[4]=cor(ymean_full$ymean,trueGxE[paste(ymean_full$VAR,ymean_full$ENV,sep=":"),"t"])
	
    }else{
    	corr2=cor(ENVmeantrain,ENVmeanfull)
    	names(corr2)="ENVmeantrain_ENVmeanfull"
    	corr3=NULL
    	}
            
	corr=c(corr1,corr2,corr3)
	
	return(corr)
}


getENVmean=function(y,ENV,ENVlevels){
  ENVmean=aggregate(y,by=list(ENV),mean)
  ENVname=ENVmean[,1]
  ENVmean=ENVmean[,2]
  names(ENVmean)=ENVname
  ENVmean=ENVmean[ENVlevels]
  ENVmean=ENVmean-mean(ENVmean)
  return(ENVmean)
}

##function to produce summary correlations
getyhat=function(Param,VAR,ENV){
  VAR=as.character(VAR)
  ENV=as.character(ENV)
  ##only unique levels of IDL and IDE combinations are kept
  yhat=Param$g[VAR]+(1+Param$b[VAR])*Param$h[ENV]
    return(yhat)
}

corYhat=function(Param1=NULL,Param2=NULL,VAR,ENV){
	

    return(cor(getyhat(Param1,VAR,ENV),getyhat(Param2,VAR,ENV)))

}
##









fitmodel=function(datfull,y,VAR,ENV,VARlevels, ENVlevels,ph=NULL,A=NULL,Ainv=NULL,model,nIter,burnIn,thin,seed=NULL,savedir=".",realizedValue=NULL){
  corr=NULL
  
  for(i in 1:length(model) ){
  	modeli=model[i]
  	savediri=file.path(savedir,modeli)
  	if(! (modeli %in% c("lm","GibbsV","GibbsNoV","jags"))){
  		 error("no model:",modeli)
  		 }
        
    if(modeli=="lm"){
      predictedValue=FW(y,VAR,ENV,VARlevels,ENVlevels,saveAt=savediri,method="OLS")
      
    }
    if (modeli=="GibbsV"){
      if(is.null(A)){   
 	   predictedValue=NULL
 	 }else{
   predictedValue=FW(y=y,VAR=VAR,ENV=ENV,VARlevels=VARlevels,ENVlevels=ENVlevels,nIter=nIter,burnIn=burnIn,thin=thin,A=A,seed=seed,saveAt=savediri,method='Gibbs')[[1]];	
 	 }
 	}
 	if (modeli=="GibbsNoV"){
 	 predictedValue=FW(y=y,VAR=VAR,ENV=ENV,VARlevels=VARlevels,ENVlevels=ENVlevels,nIter=nIter,burnIn=burnIn,thin=thin,A=NULL,seed=seed,saveAt=savediri,method='Gibbs')[[1]];	
 		
 	}
    
 if(modeli=="jags"){
          #jags will start sampling from the last member to the first member
          predictedValue = jagsFW(y=y,VAR=VAR,ENV=ENV,VARlevels=rev(VARlevels),ENVlevels=rev(ENVlevels),Ainv=Ainv,ph=ph[rev(ENVlevels)],burnIn=burnIn,nIter=nIter,thin=thin,n.adapt=0,seed=seed,savedir=savediri)[[1]];
        }
    if(!is.null(realizedValue) & !is.null(predictedValue)){
    	corri=summaryCor(datfull$y,datfull$VAR,datfull$ENV,realizedValue,predictedValue)
    }else{
    	corri=NA
    }
      corr=cbind(corr,corri)
  }
  colnames(corr)=model
  return(corr)
}




#########################################################
#simulate data
#########################################################

SimuData=function(nh=10,ng=10,nrep=2,mu=100,var_g=1,var_h=1,var_b=1,var_e=2,A=NULL,savedir,design="halfVAR",runModel=T,pro.missing=0.5,burnIn=3000,nIter=5000,thin=5,model,trueModel="FW",seed=NULL,useph=F){
  #model can be jags, Gibbs, lm
  if(! design %in% c("balance","halfENV","halfVAR","randomE",'randomG')){
    stop("design must be one of the mothods: balance, halfENV, halfVAR, randomE, randomG")
  }  
  if(!file.exists(savedir))dir.create(savedir,recursive=T)
  if(!is.null(A)){  
    g=mvrnorm(n=1,mu=rep(0,ng),Sigma=A*var_g)
    if(trueModel=="FW"){
    b=mvrnorm(n=1,mu=rep(0,ng),Sigma=A*var_b)
    }else{
    	b=NA
    	}
    if("jags"%in%model){
    	Ainv=solve(A);
    	}else{Ainv=NULL}
  }else{	
    Ainv=NULL
    g=rnorm(ng,0,sd=sqrt(var_g))
    if(trueModel=="FW"){
    	b=rnorm(ng,0,sd=sqrt(var_b))
    	}else{
    		b=NA
    		}
  }
  
  h=rnorm(nh,0,sd=sqrt(var_h));
  if(trueModel=="Normal"){
  	o=rnorm(ng*nh,0,sd=sqrt(var_g*var_h))
  }else{o=NA}
  e=rnorm(ng*nh*nrep,0,sd=sqrt(var_e));	
  #line effect
  VAR=paste("V",as.character(rep(c(1:ng),each=nh*nrep)),sep="")
  ENV=paste("E",as.character(rep(rep(c(1:nh),each=nrep),ng)),sep="")
  
  IDEL=getIDEL(VAR,ENV,VARlevels=paste("V",1:ng,sep=""),ENVlevels=paste("E",1:nh,sep=""))
  IDE=IDEL$IDE
  IDL=IDEL$IDL
  VARlevels=IDEL$VARlevels
  ENVlevels=IDEL$ENVlevels
  names(g)=VARlevels
  names(h)=ENVlevels
  if(!is.null(A)){
  	colnames(A)=rownames(A)=VARlevels 
    save(A,file=file.path(savedir,"A.rda"))
    }
  if(!is.null(Ainv)){
  	colnames(Ainv)=rownames(Ainv)=VARlevels 
    save(Ainv,file=file.path(savedir,"Ainv.rda"))
  }  
  
  
  trueGxE=expand.grid(VAR=VARlevels,ENV=ENVlevels)
  rownames(trueGxE)=paste(trueGxE$VAR,trueGxE$ENV,sep=":")
  if(trueModel=="FW"){
  names(b)=VARlevels	
  trueGxE$t=mu+g[trueGxE$VAR]+(1+b[trueGxE$VAR])*h[trueGxE$ENV]
  y=mu+g[IDL]+h[IDE]+b[IDL]*h[IDE]+e
  }
  if(trueModel=="Normal"){
  	 names(o)=rownames(trueGxE)
  	 trueGxE$t=mu+g[trueGxE$VAR]+h[trueGxE$ENV]+o
  	 y=mu+g[VAR]+h[ENV]+o[paste(VAR,ENV,sep=":")]+e
  }
  dat=data.frame(y)
  dat$VAR=VAR
  dat$ENV=ENV
  dat$IDL=IDL
  dat$IDE=IDE
  realizedValue=list(mu=mu,g=g,h=h,b=b,o=o,var_g=var(g),var_h=var(h),var_b=var(b),var_e=var(e),trueGxE=trueGxE)
  if(!file.exists(savedir)) dir.create(savedir)
     
  save(realizedValue,file=file.path(savedir,"realizedValue.rda"))
  datfull=dat
  if(design!="balance"){
    
    save(datfull,file=file.path(savedir,"datfull.rda"))
  }
  if(design=="halfENV"){
    #unbalanced data: divide ENV into high and low groups, 
    #first half lines in low environments, second five lines in high environments
    lowE=names(sort(h)[1:(nh/2)])
    highE=names(sort(h)[(nh/2+1):nh])
    
    for(ENVi in lowE){
      dat=dat[-which(dat$IDL%in%((ng/2+1):ng) & dat$ENV==ENVi),]
      
    }
    for(ENVi in highE){
      dat=dat[-which(dat$IDL%in%c(1:(ng/2)) & dat$ENV==ENVi),]
    }
  }
  if(design=="halfVAR"){
    #unbalanced data: divide VAR into high and low groups,
    # high half lines in first env group, low half lines in second  env group
    lowG=names(sort(g)[1:(ng/2)])
    highG=names(sort(g)[(ng/2+1):ng])
    
    for(VARi in lowG){
      dat=dat[-which(dat$IDE%in%((nh/2+1):nh) & dat$VAR==VARi),]
      
    }
    for(VARi in highG)  {
      dat=dat[-which(dat$IDE%in%c(1:(nh/2)) & dat$VAR==VARi),]
    }
  }
  #randomly missing a porportion pro.missing of genotypes from each enrionment.
  if(design=="randomG"){
    ng.remove=round(ng*pro.missing)
    for(i in 1:nh)dat=dat[-which((dat$IDL %in% sample(1:ng,ng.remove)) & dat$IDE==i),]			
  }
  
    #randomly missing a porportion pro.missing of environments from each variety.
  if(design=="randomE"){
    nh.remove=round(nh*pro.missing)
    if(nh.remove<1)nh.remove=1
   for(i in 1:ng)dat=dat[-which((dat$ENV %in% ENVlevels[sample(1:nh,nh.remove)]) & (dat$VAR==VARlevels[i])),]
  }
  ##
  #if(design=="reps"){
  #  nrep.remove=round(nrep*pro.missing)
  #}
  

  save(dat,file=file.path(savedir,"dat.rda"))
  
  if(runModel==T) {	
    if(useph==T){
      ph=getENVmean(dat$y,dat$ENV,ENVlevels)
    }else{
      ph=rep(0,nh)
      names(ph)=ENVlevels
    }
    corr=fitmodel(datfull=datfull,y=dat$y,VAR=dat$VAR,ENV=dat$ENV,VARlevels=VARlevels,ENVlevels=ENVlevels,ph=ph,A=A,Ainv=Ainv,model=model,nIter=nIter,burnIn=burnIn,thin=thin,seed=seed,savedir=savedir,realizedValue=realizedValue)
    colnames(corr)=paste(design,colnames(corr),sep="_")		
    save(corr,file=file.path(savedir,"corr.rda"))
  }		
    
  return(corr)
}

##return significantly different pairs
array_sigdiff=function(dat){
  #three dimentional array data
  #comparisons was done among columns (the third dimension )
  #different parameters within the same comparison was in rows (second dimension)
  #samples was arranged according to the  first dimension of the array 
  nrecords=dim(dat)[1]
  col_names=dimnames(dat)[[3]]
  ncol=length(col_names)
  row_names=dimnames(dat)[[2]]
  nrow=length(row_names)
  #get pairdiff
  #Order columns by the mean
  origroupOrder=col_names
  ToOrder=order(apply(dat,3,mean),decreasing=T)
  dat=dat[,,ToOrder]
  groups=col_names[ToOrder]
  cmb=combn(groups,2)
  npairs=ncol(cmb)
  pairdiff=array(dim=c(nrecords,nrow,npairs))
  for(i in 1:dim(pairdiff)[1]){
    pairdiff[i,,]=apply(cmb,2,function(a){dat[i,,a[1]]-dat[i,,a[2]]})
    
  }
  dimnames(pairdiff)[1:2]=dimnames(dat)[1:2]
  
  #get sigdiff
  sigdiff=array(dim=c(nrow,npairs))
  for(i in 1:npairs){
    diff=pairdiff[,,i]
    mean_diff=apply(diff,2,mean)
    sd_diff=apply(diff,2,sd)
    p_value=2*pnorm(q=abs(mean_diff),mean=0,sd=sd_diff/sqrt(nrecords),lower.tail=F)
    sigdiff[,i]=p_value<0.05
  }
  sigdiff=apply(sigdiff,1,function(a)cmb[,a,drop=F])
  names(sigdiff)=row_names
  ##The order of sigdiff should be from lower to higher, L1, L2, L3: L2 pushed out and increase a letter, then L3 is pushed out and increase a letter.
  #sigdiff=sigdiff[,ncol(sigdiff):1,drop=F]
  #bootlab=tstlab(sigdiff,ng=length(groups),groupnames=groups)
  #bootlab=bootlab[origroupOrder]
  return(sigdiff)
}


