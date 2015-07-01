
require(R.utils)
plotcorr=function(corr,methoddim=2,cornamesdim=3,methods,corname,plotfile){
	if(!missing(plotfile)){
		pdf(plotfile,width=9,height=3)
	}
	tmp=extract(corr,dims=c(methoddim,cornamesdim),indices=list(methods[1],corname),drop=T)
	nobs=length(tmp)
	 plot(tmp,ylim=range(corr),xlim=c(1,nobs*1.1),col=1)
if(length(methods>1)){
for(i in 2:length(methods)){
tmp=extract(corr,dims=c(methoddim,cornamesdim),indices=list(methods[i],corname),drop=T)
points(tmp,col=i)
 }
 }
 legend(x="bottomright",col=c(1:length(methods)),pch=rep(1,length(methods)),legend=methods)
 if(!missing(plotfile)){
 	dev.off()
system(paste("open",filename))
 }
}

plotdiffcorr=function(corr,methoddim,cornamesdim,methods,corname,...){
		tmp1=extract(corr,dims=c(methoddim,cornamesdim),indices=list(methods[1],corname),drop=T)
        tmp2=extract(corr,dims=c(methoddim,cornamesdim),indices=list(methods[2],corname),drop=T)
        diffcorr=tmp2-tmp1
        n.sample=length(diffcorr)
	    boxplot(diffcorr,...)
        abline(h=0,col="red")
        points(diffcorr~seq(0.5,1.5,length.out=n.sample),pch=3,col='red')
}

