### we write fuction for testing 4 group differences in bivariate case and trivariate case, separately.
###4 group differences in bivariate case corresponds to Canadian weather applications (Application 2)
#### 4 group differences in trivariate case corresponds to Spanish weather applications (Application 3)

##bivariate case
### We assume y is a 2*p by n matrix
biv_group_test<-function(y,n1,n2,n3,n4)
{
  p<-y[1]/2
  whole_mean<-apply(y,1,mean)
  group1_mean<-apply(y[,1:n1],1,mean)
  group2_mean<-apply(y[,(n1+1):(n1+n2)],1,mean)
  group3_mean<-apply(y[,(n1+n2+1):(n1+n2+n3)],1,mean)
  group4_mean<-apply(y[,-(1:(n1+n2+n3))],1,mean)
  Total<-array(0,dim=c(2,2,p))
  Error<-array(0,dim=c(2,2,p))
  for (i in 1:p)
  {
    for (k in 1:n)
    {
      term<-c((y-whole_mean)[i,k],(y-whole_mean)[i+p,k])
    Total[,,i]<-Total[,,i]+term%*%t(term)
    }
  for (w in 1:n1)
  {
    term<-c((y-group1_mean)[i,w],(y-group1_mean)[i+p,w])
    Error[,,i]<-Error[,,i]+term%*%t(term)
  }
    for (w in (n1+1):(n1+n2))
    {
      term<-c((y-group2_mean)[i,w],(y-group2_mean)[i+p,w])
      Error[,,i]<-Error[,,i]+term%*%t(term)
    }
    for (w in (n1+n2+1):(n1+n2+n3))
    {
      term<-c((y-group3_mean)[i,w],(y-group3_mean)[i+p,w])
      Error[,,i]<-Error[,,i]+term%*%t(term)
    }
    for (w in (n1+n2+n3+1):(n1+n2+n3+n4))
    {
      term<-c((y-group4_mean)[i,w],(y-group4_mean)[i+p,w])
      Error[,,i]<-Error[,,i]+term%*%t(term)
    } 
  }
  tmatrix<-matrix(0,nrow=2,ncol=2)
  ematrix<-matrix(0,nrow=2,ncol=2)

  for (i in 1:p)
  {
    tmatrix<-tmatrix+Total[,,i]
    ematrix<-ematrix+Error[,,i]
  }
  tmatrix<-tmatrix/p
  ematrix<-ematrix/p
  lambda<-det(ematrix)/det(tmatrix)
  return (lambda)
}

critical_biv_group_test<-function(y,n1,n2,n3,n4)
{
  p<-y[1]/2
  whole_mean<-apply(y,1,mean)
  len<-1:(n1+n2+n3+n4)
  for (t in 1:1000)
  {
  random1<-sample(len,n1,replace=FALSE)
  random2<-sample(diff(len,random1),n2,replace=FALSE)
  random3<-sample(diff(len,c(random1,random2)),n3,replace=FALSE)
  random4<-diff(len,c(random1,random2,random3))
  
  group1_mean<-apply(y[,random1],1,mean)
  group2_mean<-apply(y[,random2],1,mean)
  group3_mean<-apply(y[,random3],1,mean)
  group4_mean<-apply(y[,random4],1,mean)
  
  Total<-array(0,dim=c(2,2,p))
  Error<-array(0,dim=c(2,2,p))
  for (i in 1:p)
  {
   
    for (k in 1:n)
    {
      term<-c((y-whole_mean)[i,k],(y-whole_mean)[i+p,k])
      Total[,,i]<-Total[,,i]+term%*%t(term)
    }
    for (w in 1:n1)
    {
      term<-c((y-group1_mean)[i,w],(y-group1_mean)[i+p,w])
      Error[,,i]<-Error[,,i]+term%*%t(term)
    }
    for (w in (n1+1):(n1+n2))
    {
      term<-c((y-group2_mean)[i,w],(y-group2_mean)[i+p,w])
      Error[,,i]<-Error[,,i]+term%*%t(term)
    }
    for (w in (n1+n2+1):(n1+n2+n3))
    {
      term<-c((y-group3_mean)[i,w],(y-group3_mean)[i+p,w])
      Error[,,i]<-Error[,,i]+term%*%t(term)
    }
    for (w in (n1+n2+n3+1):(n1+n2+n3+n4))
    {
      term<-c((y-group4_mean)[i,w],(y-group4_mean)[i+p,w])
      Error[,,i]<-Error[,,i]+term%*%t(term)
    }
  }
  tmatrix<-matrix(0,nrow=2,ncol=2)
  ematrix<-matrix(0,nrow=2,ncol=2)
  
  for (i in 1:p)
  {
    tmatrix<-tmatrix+Total[,,i]
    ematrix<-ematrix+Error[,,i]
  }
  tmatrix<-tmatrix/p
  ematrix<-ematrix/p
  critical_lambda<-det(ematrix)/det(tmatrix)
  critical[t]<-critical_lambda
  }
  return (critical)
}



compare_biv_group<-function(lambda,critical)
{
  test<-length(which(lambda<=critical))/length(critical)
  if (test>=0.9)
  {
    return (paste("huge difference among the groups"))
  }
  else
  {
    return (paste("differences are not significant"))
  }
}






