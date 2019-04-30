## we implement pairwise group difference using the t-test 
## compare any two group in univariate case
##   y1 p*n1 matrix
##   y2 p*n2 matrix
pairwise<-function(y1,y2)
{
  if (y1[1]==y2[1])
  {
    n1<-y1[2]
    n2<-y2[2]
    mean1<-apply(y1,1,mean)
    mean2<-apply(y2,1,mean)
    var1<-c()
    var2<-c()
    for (i in 1:p)
    {
      var1[i]<-sum((y1[i,]-mean1[i])^2)/(n1-1)
      var2[i]<-sum((y2[i,]-mean2[i])^2)/(n2-1)
    }
    candidate<-c()
    for (i in 1:p)
    {
      candidate[i]<-abs(mean1[i]-mean2[i])/sqrt(var1[i]/n1+var2[i]/n2)
    }
    test<-max(candidate)
    
    #################
    #### calculate critical value
    
    n1<-y1[2]
    n2<-y2[2]
    y<-cbind(y1,y2)
    testvalue<-c()
    
    for (t in 1:1000)
    {
    sam1<-sample(c(1:(n1+n2)),n1,replace=FALSE)
    sam2<-diff(1:(n1+n2),sam1)
    newy1<-y[,sam1]
    newy2<-y[,-sam1]
    mean1<-apply(newy1,1,mean)
    mean2<-apply(newy2,1,mean)
    var1<-c()
    var2<-c()
    for (i in 1:p)
    {
      var1[i]<-sum((y1[i,]-mean1[i])^2)/(n1-1)
      var2[i]<-sum((y2[i,]-mean2[i])^2)/(n2-1)
    }
    candidate<-c()
    for (i in 1:p)
    {
      candidate[i]<-abs(mean1[i]-mean2[i])/sqrt(var1[i]/n1+var2[i]/n2)
    }
    test<-max(candidate)
    testvalue[t]<-test
  }
  
  prop<-length(which(test>=testvalue))/1000
  if (prop>=0.95)
  {
    return (paste ("huge difference between",sep="",y1,sep="","and",sep="",y2))
  }
  else
  {
   return (paste ("difference between",sep="",y1,sep="","and",sep="",y2,sep="","is not significant"))
  }
    #################
  }
  else 
    return (paste (y1,sep="","and ",y2,sep="","should have the same rows"))
}
