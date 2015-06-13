####################################################################################
### function to plot FW object
####################################################################################
plot.FW=function(FWobj,plotVAR=NULL,main=NULL){
  #plotVAR: the variety names for which the plot should be generated
  namesFWobj=names(FWobj)
  for(i in 1:length(namesFWobj)){
    assign(namesFWobj[i],FWobj[[i]])
  }
  if(!is.null(plotVAR)){
    plotIDL=which(VARlevels %in% plotVAR)
    VARlevels=VARlevels[plotIDL]
    whIDL=which(IDL %in% plotIDL)
    IDL=IDL[whIDL]
    IDE=IDE[whIDL]
    fitted.values=fitted.values[whIDL]
    y=y[whIDL]
  }
  yhat=aggregate(fitted.values,by=list(IDL,IDE),mean)
  y=data.frame(aggregate(y,by=list(IDL,IDE),mean))
  colnames(yhat)=c("IDL","IDE","yhat")
  yhat$yhat=yhat$yhat+mu
  h=h+mu
  IDE=yhat$IDE
  IDL=yhat$IDL
  yhat=yhat$yhat
  y=y[,3]
  uniqIDL=sort(as.numeric(unique(IDL)))
  n.IDL=length(uniqIDL)
  plot(c(min(c(y,yhat)),max(c(y,yhat)))~ c(min(h),min(h)+(max(h)-min(h))*1.05),type="n",xlab="",ylab="Variety performance",main=main)
  sorth=sort(h)
  sorth1=sorth[seq(1,length(h),by=2)]
  sorth2=sorth[seq(2,length(h),by=2)]
  axis(side=1,at=sorth1,labels=names(sorth1),line=1,las=2)
  axis(side=3,at=sorth2,labels=names(sorth2),line=1,las=2)
  cols=NULL
  pchs=NULL
  for(i in 1:n.IDL){  
    col=i+1
    pch=1
    cols=c(cols,col)
    pchs=c(pchs,pch)
    IDLi=uniqIDL[i]
    wh.i=which(IDL==IDLi)
    yhat.i=yhat[wh.i]
    y.i=y[wh.i]
    IDE.i=IDE[wh.i]
    p.i=h[IDE.i]
    order.E.i=order(p.i)
    sortp.i=sort(p.i)
    lines(yhat.i[order.E.i]~sortp.i,col=col,type="l")
    points(y.i[order.E.i]~sortp.i,col=col,pch=pch)
    whmax=which.max(p.i)
    #text(x=min(p.i)+1.05*(max(p.i)-min(p.i)),y=y.i[whmax],labels=IDLi,col=col)
    
  }
  sorth=sort(h[unique(IDE)])
  lines((sorth) ~ sorth, lty=2,col=1)
  legend("bottomright",legend=c(VARlevels, "slope = 1"),lty=c(rep(1,n.IDL),2),col=c(cols,1))
}
####################################################################################
### print postMean and FW object
####################################################################################
print.postMean=function(postMean){
  for(i in 1:length(postMean)){
    print((postMean[[i]])[c("var_e","var_g","var_b","var_h","mu","g","b","h")] )
  }  
}
print.FW=function(FWobj){
  cat("FW object\n")
  print(FWobj[c("mu","g","b","h")])
}


getIDEL=function(VAR,ENV,VARlevels=NULL,ENVlevels=NULL){
  VAR=as.character(VAR)
  ENV=as.character(ENV)
  if(is.null(VARlevels)){
  VARlevels=sort(unique(VAR))
  }
  if(is.null(ENVlevels)){
    ENVlevels=sort(unique(ENV))
  }
  fVAR=factor(VAR,levels=VARlevels,ordered=T)
  fENV=factor(ENV,levels=ENVlevels,ordered=T)
  IDL=as.numeric(fVAR)
  IDE=as.numeric(fENV)
  out=list()
  out$IDL=IDL
  out$IDE=IDE
  out$VARlevels=VARlevels
  out$ENVlevels=ENVlevels
  out$fVAR=fVAR
  out$fENV=fENV
  return(out)
}

####################################################################################
### other plot functions 
####################################################################################
plotYhatvsE=function(yhat=NULL,h=NULL,mu=NULL,g=NULL,b=NULL,IDL=NULL,IDE=NULL,main=NULL){
	#yhat is a dataframe with IDL,IDE and yhat
	# IDL and IDE are the IDLiety index for the predicted yhat values
	#either provide yhat and h or provide h,mu,g,b,IDL,IDE.
	if(is.null(yhat)){
	yhat=data.frame(aggregate(g[IDL]+(1+b[IDL])*h[IDE],by=list(IDL,IDE),mean))
    colnames(yhat)=c("IDL","IDE","yhat")
    if(!is.null(mu)){yhat$yhat=yhat$yhat+mu}
    }
	y=yhat$yhat
	IDE=yhat$IDE
	IDL=yhat$IDL
	n.IDL=length(unique(IDL))
	plot(c(min(y),max(y))~c(min(h),min(h)+(max(h)-min(h))*1.05),type="n",xlab="environment values",ylab="fitted variety values",main=main)
	for(i in 1:n.IDL){	
		wh.i=which(IDL==i)
		y.i=y[wh.i]
		IDE.i=IDE[wh.i]
		p.i=h[IDE.i]
		order.E.i=order(p.i)
		lines(y.i[order.E.i]~sort(p.i),col=i,type="b")
		whmax=which.max(p.i)
		text(x=min(p.i)+1.05*(max(p.i)-min(p.i)),y=y.i[whmax],labels=i,col=i)
	}
}

summaryplot=function(IDL,IDE,realizedValue,postMean,LSvalue,plotdir,samps){
	n.IDL=length(unique(IDL))
    n.IDE=length(unique(IDE))
    extend.IDL=rep(1:n.IDL,each=n.IDE)
    extend.IDE=rep(1:n.IDE,n.IDL)
   # predict.group=setdiff(paste(extend.IDL,extend.IDE,sep="_"),paste(IDL,IDE,sep="_"))
   # if(length(predict.group)>0){predict.IDE=as.numeric(gsub("_.*","",predict.group))
   # predict.IDL=as.numeric(gsub(".*_","",predict.group))}

	pdf("plot.pdf")
	plot(realizedValue$h,postMean$h)
    plot(realizedValue$b,postMean$b)
	plot(realizedValue$g,postMean$g)
	plot(realizedValue$h,LSvalue$h)
	plot(realizedValue$b,LSvalue$b)
	plot(realizedValue$g,LSvalue$g)
	plotYhatvsE(h=postMean$h,mu=postMean$mu,g=postMean$g,b=postMean$b,IDL=IDL,IDE=IDE,main="baysian")
	plotYhatvsE(h=LSvalue$h,mu=LSvalue$mu,g=LSvalue$g,b=LSvalue$b,IDL=IDL,IDE=IDE,main="FWlm")
	#plotYhatvsE(LSvalue$yhat,realizedValue$h,main="FWlm against realized value of E")
	#plotYhatvsE(postMean$yhat,realizedValue$h,main="baysian against realized value of E")
	plotYhatvsE(h=realizedValue$h,mu=realizedValue$mu,g=realizedValue$g,b=realizedValue$b,IDL=IDL,IDE=IDE,main="realized variety values against realized E ")
	plotYhatvsE(h=postMean$h,mu=postMean$mu,g=postMean$g,b=postMean$b,IDL=extend.IDL,IDE=extend.IDE,main="baysian extend")
	plotYhatvsE(h=LSvalue$h,mu=LSvalue$mu,g=LSvalue$g,b=LSvalue$b,IDL=extend.IDL,IDE=extend.IDE,main="FWlm extend")
	plotYhatvsE(h=realizedValue$h,mu=realizedValue$mu,g=realizedValue$g,b=realizedValue$b,IDL=extend.IDL,IDE=extend.IDE,main="realized variety values against realized E extend ")
#trace plot
	if(!is.null(samps)){
	require(coda)
	plot(samps)}
	dev.off()
	system(paste("open plot.pdf"))
}

setFW=function(g,b,h,y,VAR,ENV,...){
  eclipse=list(...)
  fitted.values=g[VAR]+(1+b[VAR])*h[ENV]
  ENVmean=aggregate(y,by=list(ENV),mean)
  nameENVmean=ENVmean[,1]
  ENVmean=ENVmean[,2]
  names(ENVmean)=nameENVmean
  ENVmean=ENVmean[names(h)]
  corENVmean=cor(ENVmean,h)
  corfitted=cor(y,fitted.values)
  cor_ymean=get_cor_ymean(g=g,b=b,h=h,y=y,VAR=VAR,ENV=ENV)
  out=list(g=g,b=b,h=h,y=y,VAR=VAR,ENV=ENV,ENVmean=ENVmean,corENVmean=corENVmean,corfitted=corfitted,cor_ymean=cor_ymean,fitted.values=fitted.values,mu=0)
  out=c(out,eclipse)	
  class(out)=c("FW","list")
  
  return(out)
  
}
##function to produce summary correlations
getyhat=function(Param,VAR,ENV){
  VAR=as.character(VAR)
  ENV=as.character(ENV)
  ##only unique levels of IDL and IDE combinations are kept
  yhat=data.frame(aggregate(Param$g[VAR]+(1+Param$b[VAR])*Param$h[ENV],by=list(VAR,ENV),mean))[,3]
  if("mu" %in% names(Param)){yhat=yhat+Param$mu}
  return(yhat)
}

corYhat=function(Param1=NULL,Param2=NULL,VAR,ENV){
	

    return(cor(getyhat(Param1,VAR,ENV),getyhat(Param2,VAR,ENV)))

}
##
get_cor_ymean=function(g,b,h,y,VAR,ENV,corOnly=T){
  ymean=aggregate(y,by=list(VAR,ENV),mean)
  VAR=ymean[,1]
  ENV=ymean[,2]
  ymean=ymean[,3]
  yhat=g[VAR]+(1+b[VAR])*h[ENV]
  return(cor(yhat,ymean))
}
groupMean_cor=function(FWobj,y_full,VAR_full,ENV_full,corOnly=T){
  out=list()
  g=FWobj$g
  b=FWobj$b
  h=FWobj$h
  VAR_fitted=FWobj$VAR
  ENV_fitted=FWobj$ENV
  y_fitted=g[VAR_fitted]+(1+b[VAR_fitted])*h[ENV_fitted]
  ymean_fitted=aggregate(y_fitted,by=list(VAR_fitted,ENV_fitted),mean)
  VAR_fitted=ymean_fitted[,1]
  ENV_fitted=ymean_fitted[,2]
  IDEL=paste(VAR_fitted,ENV_fitted,sep="_")
  ymean_full=aggregate(y_full,by=list(VAR_full,ENV_full),mean)
  VAR_full=ymean_full[,1]
  ENV_full=ymean_full[,2]
  ymean_full=ymean_full[,3]
  yhat_full=g[VAR_full]+(1+b[VAR_full])*h[ENV_full]
  
  isfitted= (paste(VAR_full,ENV_full,sep="_")  %in% IDEL)
  which_predicted=which(!isfitted)
  which_fitted=which(isfitted)
  
  ymean_predicted=ymean_full[which_predicted]
  yhat_predicted=yhat_full[which_predicted]
  ymean_fitted=ymean_full[which_fitted]
  yhat_fitted=yhat_full[which_fitted]
  
  if(corOnly==F){
  out$ymean_predicted= ymean_predicted
  out$yhat_predicted=yhat_predicted
  out$ymean_fitted=ymean_fitted
  out$yhat_fitted=yhat_fitted
  out$ymean_full=ymean_full
  out$yhat_full=yhat_full
  }
  corr=c(cor(ymean_full,yhat_full),cor(ymean_fitted,yhat_fitted),cor(ymean_predicted,yhat_predicted))
  names(corr)=c("full","fitted","predicted")
  out$corr=corr
return(out)
}





summaryCor=function(VAR,ENV,realizedValue,predictedValue){
	corr=rep(0,6)
  uniqVAR=unique(VAR)
  uniqENV=unique(ENV)
	n.VAR=length(uniqVAR)
  n.ENV=length(uniqENV)
  extend.VAR=rep(uniqVAR,each=n.ENV)
  extend.ENV=rep(uniqENV,n.VAR)
  
  #dat is a data.frame with the IDL and IDE combinations in the data set
	  corr[1]=cor(realizedValue$b[uniqVAR],predictedValue$b[uniqVAR])
    corr[2]=cor(realizedValue$g[uniqVAR],predictedValue$g[uniqVAR])
    corr[3]=cor(realizedValue$h[uniqENV],predictedValue$h[uniqENV])
    corr[4]= corYhat(Param1=realizedValue,Param2=predictedValue,VAR=VAR,ENV=ENV)
	names(corr)=c("b","g","h","yhat","predicted yhat only","extended yhat")
		
	if(length(unique(paste(VAR,ENV)))<length(extend.VAR)){
	  predict.group=setdiff(paste(extend.VAR,extend.ENV,sep="_"),paste(VAR,ENV,sep="_"))
	  predict.VAR=gsub("_.*","",predict.group)
	  predict.ENV=gsub(".*_","",predict.group)
	  corr[5]=corYhat(Param1=realizedValue,Param2=predictedValue,VAR=predict.VAR,ENV=predict.ENV)  
	  corr[6]=corYhat(Param1=realizedValue,Param2=predictedValue,VAR=extend.VAR,ENV=extend.ENV) 
	}
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


