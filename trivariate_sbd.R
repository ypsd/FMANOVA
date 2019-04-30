#rm(list=ls())
library(depth)
library(ddalpha)
#compute MSBD for trivariate functional data x1, x2 and x3
#y: n by 3p matrix; x1: the first 1:p columns; x2: (p+1):(2*p) columns;  x2: (2*p+1):(3*p) columns
trivmbd=function(y){
  yd=dim(y)
  n=yd[1]
  p=yd[2]/3
  x1=y[,(1:p)]
  x2=y[,(p+1):(2*p)]
  x3=y[,(2*p+1):(3*p)]
  dp=NULL
  for (j in 1:n){
    one=NULL
    for (i in 1:p){
      one=c(one,depth.simplicial(c(x1[j,i],x2[j,i],x3[j,i]), cbind(x1[,i],x2[,i],x3[,i]), exact=F,k=0.2))
    }
    dp=c(dp,mean(one))
  }
  dp
}
