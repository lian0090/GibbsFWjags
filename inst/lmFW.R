#this is the code for ordinary linear regression
colmult=function(Z1,Z2){
    #return Z3, Z3=(Z1*Z2[,1],Z1*Z2[,2],..Z1*Z2[,ncol(Z2)])
    if(!is.matrix(Z1) | !is.matrix(Z2)){stop("columult arguments must be matrix ")}
		p2=ncol(Z1)*ncol(Z2)
	Z=matrix(nrow=nrow(Z1),ncol=p2)
    colID.Z1=rep(0,ncol(Z))
	colID.Z2=rep(0,ncol(Z))
	for(j in 1:ncol(Z2)){	
	for(i in c(1:ncol(Z1))){
	col.ij=	(j-1)*ncol(Z1)+i
	Z[,col.ij]=Z1[,i]*Z2[,j]	
	colID.Z1[col.ij]=i
	colID.Z2[col.ij]=j
		}
	}
	
	return(list(Z=Z,colID.Z1=colID.Z1,colID.Z2=colID.Z2))
	}
	
lmFW=function(y,VAR,ENV,VARlevels=NULL,ENVlevels=NULL,savedir=".",ENVmethod="lmer"){
  IDEL=getIDEL(VAR,ENV,VARlevels,ENVlevels)
  for(i in 1:length(IDEL)){
  	assign(names(IDEL)[i],IDEL[[i]])
  }
  nrep=aggregate(y,by=list(VAR,ENV),FUN=length)[,3]
   if(ENVmethod=="lm"){
   ZE=model.matrix(~-1+fENV)
   ZV=model.matrix(~-1+fVAR)
  	if(any(nrep==1)){
  	  	lm0=lm(y~-1+ZE+ZV)
  	}else {
  		#all nrep >1
  		  ZX=colmult(ZE,ZV)$Z
  		lm0=lm(y~-1+ZE+ZV+ZX) 
  		lm0=lm(y~-1+ZE+ZV)
	 	}
	 	h=coef(lm0)[paste("ZEfENV",ENVlevels,sep="")]
	 	h=h-mean(h)
  	}
   if(ENVmethod=="mean")	{
  h=tapply(y,INDEX=IDE,mean)-mean(y)
  }
  if(ENVmethod=="lmer"){
  	if(any(nrep==1)){
   	lm0=lmer(y~(1|ENV)+(1|VAR))
  	}else{
  		
 		lm0=lmer(y~(1|ENV)+(1|VAR)+(1|ENV:VAR))
 		lm0=lmer(y~(1|ENV)+(1|VAR))

 	}	
 	  		h=ranef(lm0)$ENV[ENVlevels,1]

  }
  names(h)=ENVlevels
  n.var=length(VARlevels)
  ZX=sweep(model.matrix(~fVAR-1),1,h[IDE],"*")
  lm1=lm(y~-1+fVAR+ZX)
  gb=coef(lm1)
  g=gb[paste("fVAR",VARlevels,sep="")]
  b=gb[paste("ZXfVAR",VARlevels,sep="")]-1
  names(g)=VARlevels
  names(b)=VARlevels

  LSvalue=setFW(g=g,b=b,h=h,y=y,VAR=VAR,ENV=ENV,IDL=IDL,IDE=IDE,VARlevels=VARlevels,ENVlevels=ENVlevels,mu=0) 
  class(LSvalue)=c("FW","list")
  save(LSvalue,file=file.path(savedir,"LSvalue.rda"))
  return(LSvalue)
}





