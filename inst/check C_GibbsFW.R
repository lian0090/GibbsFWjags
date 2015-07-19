cd /Users/lianlian/Dropbox/github/FW/src
R CMD SHLIB sample_beta.c C_GibbsFW.c -o C_GibbsFW.so
setwd("~/Dropbox/github/FW/src")
library(GibbsFW)
dyn.load("C_GibbsFW.so")
source("~/Dropbox/github/FW/R/GibbsFW.R")
load("~/Dropbox/github/FW/data/wheat.rda")
G=wheat.G50
whNA=which(is.na(wheat.Y50$yNA))
attach(wheat.Y50)
gf3=GibbsFW(yNA,wheat.Y50$VAR,wheat.Y50$ENV,A=wheat.G50)
gf2=GibbsFW(yNA,wheat.Y50$VAR,wheat.Y50$ENV)

ymean_vali=aggregate(wheat.Y50$y[whNA],by=list(wheat.Y50$VAR[whNA],wheat.Y50$ENV[whNA]),mean)[,3]
yhat_vali=aggregate(gf3$yhat[whNA],by=list(wheat.Y50$VAR[whNA],wheat.Y50$ENV[whNA]),mean)[,3]
ymean_train=aggregate(wheat.Y50$y[-whNA],by=list(wheat.Y50$VAR[-whNA],wheat.Y50$ENV[-whNA]),mean)[,3]
yhat_train=aggregate(gf3$yhat[-whNA],by=list(wheat.Y50$VAR[-whNA],wheat.Y50$ENV[-whNA]),mean)[,3]
cor(ymean_vali,yhat_vali)


yhat_vali=aggregate(gf2$yhat[whNA],by=list(wheat.Y50$VAR[whNA],wheat.Y50$ENV[whNA]),mean)[,3]
cor(ymean_vali,yhat_vali)
-2*(sum(dnorm(gf3$y[-whNA]-gf3$post_yhat[-whNA],sd=sqrt(gf3$var_e),log=T))-300*dnorm(0,sd=sqrt(gf3$var_e),log=T))+2*gf3$pD
 -2*(sum(dnorm(gf2$y[-whNA]-gf3$post_yhat[-whNA],sd=sqrt(gf2$var_e),log=T))-300*dnorm(0,sd=sqrt(gf2$var_e),log=T))+2*gf2$pD