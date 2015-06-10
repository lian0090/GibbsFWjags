fitmodel=function(y,IDL,IDE,A=NULL,Ainv=NULL,model,nIter,burnIn,thin,seed=NULL,savedir=".",realizedValue=NULL){
  corr=NULL
  for(modeli in model){
    if(modeli=="lm"){
      predictedValue=lmFW(y,IDL,IDE)
    }else
      if(modeli=="Gibbs"){
        predictedValue=GibbsFW(y=y,VAR=IDL,ENV=IDE,nIter=nIter,burnIn=burnIn,thin=thin,A=A,seed=seed,savedir=savedir)[[1]];
      }else
        if(modeli=="jags"){
          predictedValue = jagsFW(y=y,IDL=IDL,IDE=IDE,Ainv=Ainv,burnIn=burnIn,nIter=nIter,thin=thin,n.adapt=0,seed=seed,savedir=savedir)[[1]];
        }else{
          error("no model:",modeli)
        }
    if(!is.null(realizedValue)){			
      corr=cbind(corr,summaryCor(IDL,IDE,realizedValue,predictedValue));
    }
  }
  colnames(corr)=model
  return(corr)
}




#########################################################
#simulate data
#########################################################
SimuData=function(parameters,savedir,ub="halfVAR",pro.missing=0.5,runModels=T,burnIn=1000,nIter=5000,thin=thin,model,seed=NULL){
  #model can be jags, Gibbs, lm
  #parameters is a list with var_g,var_b,var_h,var_e,ng,nh,nrep,mu,and(or) A.
  if(!file.exists(savedir))dir.create(savedir,recursive=T)
  for(i in 1:length(parameters)){
    assign(names(parameters)[i],parameters[[i]])
  }
  if(!is.null(parameters$A)){	
    save(A,file=file.path(savedir,"A.rda"))
    g=mvrnorm(n=1,mu=rep(0,ng),Sigma=A*var_g)
    b=mvrnorm(n=1,mu=rep(0,ng),Sigma=A*var_b)
    if("jags"%in%model){Ainv=solve(A);save(Ainv,file=file.path(savedir,"Ainv.rda"))}
  }else{	
    A=NULL
    Ainv=NULL
    g=rnorm(ng,0,sd=sqrt(var_g))
    b=rnorm(ng,0,sd=sqrt(var_b))
  }
  
  h=rnorm(nh,0,sd=sqrt(var_h));
  e=rnorm(ng*nh*nrep,0,sd=sqrt(var_e));	
  #line effect
  IDL=rep(c(1:ng),each=nh*nrep)
  IDE=rep(rep(c(1:nh),each=nrep),ng)
  y=mu+g[IDL]+h[IDE]+b[IDL]*h[IDE]+e
  
  dat=data.frame(y)
  dat$IDL=IDL
  dat$IDE=IDE
  names(g)=c(1:ng)
  names(h)=c(1:nh)
  names(b)=c(1:ng)
  realizedValue=list(mu=mu,g=g,h=h,b=b,var_g=var(g),var_h=var(h),var_b=var(b),var_e=var(e))
  save(realizedValue,file=file.path(savedir,"realizedValue.rda"))
  if(!file.exists(file.path(savedir,"balance"))) dir.create(file.path(savedir,"balance"))
  save(dat,file=file.path(savedir,"balance/dat.rda"))
  if(runModels==T) {
    balance.sum=fitmodel(y=y, IDL=IDL, IDE=IDE,A=A,Ainv=Ainv, nIter=nIter, burnIn=burnIn, thin=thin, model=model, seed=seed,savedir=file.path(savedir,"balance"),realizedValue=realizedValue)
    colnames(balance.sum)=paste("balance",colnames(balance.sum),sep="_")
  }
  if(! ub %in% c("halfENV","halfVAR","random")){
    stop("ub must be one of the mothods: halfENV, halfVAR, random")
  }	
  if(ub=="halfENV"){
    #unbalanced data: divide ENV into high and low groups, 
    #first half lines in low environments, second five lines in high environments
    lowE=as.integer(names(sort(h)[1:(nh/2)]))
    highE=as.integer(names(sort(h)[(nh/2+1):nh]))
    
    for(i in 1:nh){
      if(i %in% lowE) dat=dat[-which(dat$IDL%in%c(1:(ng/2)) & dat$IDE==i),]
      else dat=dat[-which(dat$IDL%in%((ng/2+1):ng) & dat$IDE==i),]
    }
  }
  if(ub=="halfVAR"){
    #unbalanced data: divide VAR into high and low groups,
    # high half lines in first env group, low half lines in second  env group
    lowG=as.integer(names(sort(g)[1:(ng/2)]))
    highG=as.integer(names(sort(g)[(ng/2+1):ng]))
    
    for(i in 1:ng){
      if(i %in% lowG) dat=dat[-which(dat$IDE%in%c(1:(nh/2)) & dat$IDL==i),]
      else dat=dat[-which(dat$IDE%in%((nh/2+1):nh) & dat$IDL==i),]
    }
  }
  #randomly missing a porportion pro.missing of genotypes from each enrionment.
  if(ub=="random"){
    ng.remove=round(ng*pro.missing)
    for(i in 1:nh)dat=dat[-which((dat$IDL %in% sample(1:ng,ng.remove)) & dat$IDE==i),]			
  }
  ##
  #if(ub=="reps"){
  #  nrep.remove=round(nrep*pro.missing)
  #}
  
  if(!file.exists(file.path(savedir,ub))) dir.create(file.path(savedir,ub))
  save(dat,file=file.path(savedir,ub,"dat.rda"))
  y=dat$y
  IDL=dat$IDL
  IDE=dat$IDE
  
  if(runModels==T) {		
    ub.sum=fitmodel(y=y,IDL=IDL,IDE=IDE,A=A,Ainv=Ainv,model=model,nIter=nIter,burnIn=burnIn,thin=thin,seed=seed,savedir=file.path(savedir,ub),realizedValue=realizedValue)
                      colnames(ub.sum)=paste(ub,colnames(ub.sum),sep="_")		
                      corr=cbind(balance.sum,ub.sum)	
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


