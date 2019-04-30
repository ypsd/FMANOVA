### for bivariate functional data
### n by 2*p matrix
rank_based<-function(y,z)
{
  if (y[2]==z[2])
 { p=y[2]/2
  n1=y[1]
  n2=z[1]
  q<-max(n1,n2)
  w<-matrix(NA,nrow=q,ncol=2*p)
  
  com<-cbind(y,z)
  for (i in 1:q)
  {
    w[i,]<-com[,sample(1:(n1+n2),1,replace=TRUE)]
  }
  depth_y<-sbd(y)
  depth_z<-sbd(z)
  depth_w<-sbd(w)
  order_y<-c()
  order_z<-c()
  for (i in 1:n1)
  {
    order_y[i]<-length(which(depth_w<=depth_y[i]))/q
    order_z[i]<-length(which(depth_w<=depth_z[i]))q
  }
  index<-order(c(order_y,order_z),decreasing=FALSE)
  test_st<-length(index(-(1:n1)))
  critical<-sample(1:n1+n2,1000)
  if (length(which(abs(test_st-critical)>=(n1+n2-1)^2/12))>=900)
  return (paste(y,sep="","and",sep="",z,sep="","are significantly different"))
  else
  {
    return (paste(y,sep="","and",sep="",z,sep="","are not significantly different"))
  }
  }
 else stop(paste(y,sep="","and",sep="",z,sep="","dont have same rows"))
}