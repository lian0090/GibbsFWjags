#this is the code for ordinary linear regression
lmFW=function(y,VAR,ENV,VARlevels,ENVlevels,savedir="."){
  IDEL=getIDEL(VAR,ENV,VARlevels,ENVlevels)
  IDE=IDEL$IDE
  IDL=IDEL$IDL
  VARlevels=IDEL$VARlevels
  ENVlevels=IDEL$ENVlevels
  fVAR=IDEL$fVAR
  h=tapply(y,INDEX=IDE,mean)-mean(y)
  n.var=length(VARlevels)
  ZX=sweep(model.matrix(~fVAR-1),1,h[IDE],"*")
  lm1=lm(y~-1+fVAR+ZX)
  gb=coef(lm1)
  g=gb[paste("fVAR",VARlevels,sep="")]
  b=gb[paste("ZXfVAR",VARlevels,sep="")]-1
  names(g)=VARlevels
  names(b)=VARlevels
  names(h)=ENVlevels
  fitted.values=g[IDL]+(1+b[IDL])*h[IDE]
  LSvalue=list(mu=0,g=g,b=b,h=h,fitted.values=fitted.values,y=y,VARlevels=VARlevels,ENVlevels=ENVlevels,IDL=IDL,IDE=IDE)
  class(LSvalue)=c("FW","list")
  save(LSvalue,file=file.path(savedir,"LSvalue.RData"))
  return(LSvalue)
}





