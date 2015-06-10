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
  plot(c(min(c(y,yhat)),max(c(y,yhat)))~ c(min(h),min(h)+(max(h)-min(h))*1.05),type="n",xlab="Environment values",ylab="Variety performance",main=main)
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

##function to produce summary correlations
getyhat=function(Param,IDL,IDE){
  IDL=as.integer(IDL)
  IDE=as.integer(IDE)	
  ##only unique levels of IDL and IDE combinations are kept
  yhat=data.frame(aggregate(Param$g[IDL]+(1+Param$b[IDL])*Param$h[IDE],by=list(IDL,IDE),mean))[,3]
  if("mu" %in% names(Param)){yhat=yhat+Param$mu}
  return(yhat)
}

corYhat=function(Param1=NULL,Param2=NULL,IDL,IDE){
	

    return(cor(getyhat(Param1,IDL,IDE),getyhat(Param2,IDL,IDE)))

}


summaryCor=function(IDL,IDE,realizedValue,predictedValue){
	corr=rep(0,6)
	n.IDL=length(unique(IDL))
    n.IDE=length(unique(IDE))
    extend.IDL=rep(1:n.IDL,each=n.IDE)
    extend.IDE=rep(1:n.IDE,n.IDL)
 
	
	#dat is a data.frame with the IDL and IDE combinations in the data set
	corr[1]=cor(realizedValue$b,predictedValue$b)
    corr[2]=cor(realizedValue$g,predictedValue$g)
    corr[3]=cor(realizedValue$h,predictedValue$h)
    corr[4]= corYhat(Param1=realizedValue,Param2=predictedValue,IDL=IDL,IDE=IDE)
	names(corr)=c("b","g","h","yhat","predicted yhat only","extended yhat")
		
	if(length(unique(paste(IDL,IDE)))<length(extend.IDL)){
	  predict.group=setdiff(paste(extend.IDL,extend.IDE,sep="_"),paste(IDL,IDE,sep="_"))
	  predict.IDL=as.integer(gsub("_.*","",predict.group))
	  predict.IDE=as.integer(gsub(".*_","",predict.group))
	  corr[5]=corYhat(Param1=realizedValue,Param2=predictedValue,IDL=predict.IDL,IDE=predict.IDE)  
	  corr[6]=corYhat(Param1=realizedValue,Param2=predictedValue,IDL=extend.IDL,IDE=extend.IDE) 
	}
	return(corr)
}




