get_cor_ymean=function(g,b,h,y,VAR,ENV){
  ymean=aggregate(y,by=list(VAR,ENV),mean)
  VAR=ymean[,1]
  ENV=ymean[,2]
  ymean=ymean[,3]
  yhat=g[VAR]+(1+b[VAR])*h[ENV]
  return(cor(yhat,ymean,use="na.or.complete"))
}

setFW=function(g,b,h,y,VAR,ENV,...){
  eclipse=list(...)
  
  yhat=g[VAR,]+(1+b[VAR,])*h[ENV,]
 # ENVmean=aggregate(y,by=list(ENV),function(a)mean(a,na.rm=T))
 # nameENVmean=ENVmean[,1]
 # ENVmean=ENVmean[,2]
 # names(ENVmean)=nameENVmean
 # ENVmean=ENVmean[names(h)]
 # corENVmean_hhat=appy(h,2,function(a)cor(ENVmean,a,use="na.or.complete"))
 # cor_y_yhat=apply(yhat,2,function(a)cor(y,a,use="na.or.complete"))
 # cor_ymean_yhat=get_cor_ymean(g=g,b=b,h=h,y=y,VAR=VAR,ENV=ENV)
 # out=list(g=g,b=b,h=h,y=y,VAR=VAR,ENV=ENV,ENVmean=ENVmean,corENVmean_hhat=corENVmean_hhat,cor_y_yhat=cor_y_yhat,cor_ymean_yhat=cor_ymean_yhat,yhat=yhat)
  out=list(g=g,b=b,h=h,y=y,yhat=yhat,VAR=VAR,ENV=ENV)
  out=c(out,eclipse)	
  class(out)=c("FW","list")
  
  return(out)
  
}

print.postMean=function(postMean){
  for(i in 1:length(postMean)){
    print((postMean[[i]])[c("var_e","var_g","var_b","var_h","mu","g","b","h")] )
  }  
}