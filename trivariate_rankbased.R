### for trivariate functional data
### y: n by 3*p matrix; x1: the first 1:p columns; x2: (p+1):(2*p) columns; x3: (2*p+1):(3*p) columns
trivariate_rankbased<-function(y,z)
{
  if (y[2]==z[2])
  { p=y[2]/3
  n1=y[1]
  n2=z[1]
  q<-max(n1,n2)
  w<-matrix(NA,nrow=q,ncol=3*p)
  
  com<-cbind(y,z)
  for (i in 1:q)
  {
    w[i,]<-com[,sample(1:(n1+n2),1,replace=TRUE)]
  }
  depth_y<-trivmbd(y)
  depth_z<-trivmbd(z)
  depth_w<-trivmbd(w)
  order_y<-c()
  order_z<-c()
  for (i in 1:n1)
  {
    order_y[i]<-sum(which(depth_w<=depth_y[i]))/q
    order_z[i]<-sum(which(depth_w<=depth_z[i]))/q
  }
  index<-order(c(order_y,order_z),decreasing=FALSE)
  test_st<-sum(index(-(1:n1)))
  critical<-sample(1:n1+n2,1000)
  if (sum(which(abs(test_st-critical)>=(n1+n2-1)^2/12))>=900)
    return (paste(y,sep="","and",sep="",z,sep="","are significantly different"))
  else
  {
    return (paste(y,sep="","and",sep="",z,sep="","are not significantly different"))
  }
  }
  else stop(paste(y,sep="","and",sep="",z,sep="","dont have same rows"))
}