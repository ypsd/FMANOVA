DirOut=function(data,DirOutmatrix=FALSE,h=0.55,method="Mah")
{
  temp=dim(data)
  #####################
  #Univariate cases   #
  #####################
  
  if (length(temp)==2)
  {
    
    data=t(data)
    p=dim(data)[1]
    n=dim(data)[2]
    Dirout=matrix(0,n,p)
    dmat=matrix(0,p,n)
    medvec=apply(data,1,median)
    madvec=apply(data,1,mad)
    outmat=abs((data-medvec)/(madvec))
    signmat=sign((data-medvec))
    Dirout=t(outmat*signmat)
    out_avr=apply(Dirout,1,mean)
    out_var=apply(Dirout,1,var)
    M=cbind(out_avr,out_var)
    ans=cov.rob(M,method="mcd",nsamp="best",quantile.used=floor(n*h))
    cov=ans$cov
    me=ans$center
    D=mahalanobis(M,me,cov)
  }
  
  #####################
  #Multivariate cases #
  #####################
  
  
  if (length(temp)==3)
  {
    n=temp[1]
    p=temp[2]
    d=temp[3]
    Dirout=array(0,dim=c(n,p,d))
    for (j in 1:p)
    {
      temp=covMcd(data[,j,], alpha = 0.51,control = rrcov.control(alpha=0.95))
      me=temp$center
      if (method=="Mah")
      {
        out=mahalanobis(data[,j,],temp$center,temp$cov)
      }
      
      if (method=="SDO")
      {
        out=adjOutlyingness(data[,j,],clower=0,cupper=0)$adjout
      }
      for (i in 1:n)
      {
        if ((sum((data[i,j,]-me)^2))^(1/2)==0)
        {
          Dirout[i,j,]=rep(0,d)
        }
        else{
          dir=(data[i,j,]-me)/(sum((data[i,j,]-me)^2))^(1/2)
          Dirout[i,j,]=dir*out[i]
        }
      }
    }
    out_avr=apply(Dirout,c(1,3),mean)
    out_var=apply(Dirout^2,1,mean)*d-apply(out_avr^2,1,sum)
    M=cbind(out_avr,out_var)
    ans=cov.rob(M,method="mcd",nsamp="best",quantile.used=floor(n*h))
    cov=ans$cov
    me=ans$center
    D=mahalanobis(M,me,cov)
  }
  
  if (DirOutmatrix)
    list(D=D,Dirout=Dirout,out_avr=out_avr,out_var=out_var)
  else 
    list(D=D,out_avr=out_avr,out_var=out_var)
}
library(fda)
data("CanadianWeather")
CanadianTempPrecip.fd <- with(CanadianWeather, smooth.basis(day.5,dailyAv[,,-2], daybasis65)$fd )
str(CanadianTempPrecip.fd)
rgns <- 1:4
names(rgns) <- c('Arctic', 'Atlantic', 'Continental', 'Pacific')
Rgns <- rgns[CanadianWeather$region]
with(CanadianWeather, points(-coordinates[, 2], coordinates[, 1],
                             col=Rgns, pch=Rgns) )
legend('topright', legend=names(rgns), col=rgns, pch=rgns)
Arctic_temp=matrix()
  #array(0,c(365,3,3))
Atlantic_temp=matrix()
  #array(0,c(365,15,3))
Continental_temp=matrix()
  #array(0,c(365,12,3))
Pacific_temp=matrix()
  #array(0,c(365,5,3))
for ( i in 1:35)
{
  if (CanadianWeather$region[i]=="Arctic")
    Arctic_temp=cbind(Arctic_temp,CanadianWeather$dailyAv[,i,1])
  if (CanadianWeather$region[i]=="Atlantic")
    Atlantic_temp=cbind(Atlantic_temp,CanadianWeather$dailyAv[,i,1])
  if (CanadianWeather$region[i]=="Continental")
    Continental_temp=cbind(Continental_temp,CanadianWeather$dailyAv[,i,1])
  if(CanadianWeather$region[i]=="Pacific")
    Pacific_temp=cbind(Pacific_temp,CanadianWeather$dailyAv[,i,1])
}
A=CanadianWeather$dailyAv[,CanadianWeather$region=="Arctic",]
B=CanadianWeather$dailyAv[,CanadianWeather$region=="Atlantic",]
C=CanadianWeather$dailyAv[,CanadianWeather$region=="Continental",]
D=CanadianWeather$dailyAv[,CanadianWeather$region=="Pacific",]
a=matrix(0,365*3,3)
b=matrix(0,365*3,15)
c=matrix(0,365*3,12)
d=matrix(0,365*3,5)
a=rbind(A[,,1],A[,,2],A[,,3])
b=rbind(B[,,1],B[,,2],B[,,3])
c=rbind(C[,,1],C[,,2],C[,,3])
d=rbind(D[,,1],D[,,2],D[,,3])
source("bivmbd.R")
depth1=bivmbd(a)
depth2=bivmbd(b)
depth3=bivmbd(c)
depth4=bivmbd(d)
median_a=a[,order(depth1,decreasing=T)[1]]
median_b=b[,order(depth2,decreasing=T)[1]]
median_c=c[,order(depth3,decreasing=T)[1]]
median_d=d[,order(depth4,decreasing=T)[1]]
for(i in 1:ncol(a))
{
  a[,i]=a[,i]-median_a
}
for(i in 1:ncol(b))
{
  b[,i]=b[,i]-median_b
}
for(i in 1:ncol(c))
{
  c[,i]=c[,i]-median_c
}
for(i in 1:ncol(d))
{
  d[,i]=d[,i]-median_d
}
merge=cbind(median_a,median_b,median_c,median_d)
medium=merge[,order(bivmbd(merge),decreasing=T)[1]]
reffect1=median_a-medium
reffect2=median_b-medium
reffect3=median_c-medium
reffect4=median_d-medium
geffect=medium

mean_geffect=apply(cbind(a,b,c,d),1,mean)
mean_r1=apply(a,1,mean)-mean_geffect
mean_r2=apply(b,1,mean)-mean_geffect
mean_r3=apply(c,1,mean)-mean_geffect
mean_r4=apply(d,1,mean)-mean_geffect
#a=array(0,c(3,365,3))
#b=array(0,c(15,365,3))
#c=array(0,c(12,365,3))
#d=array(0,c(5,365,3))
#for (i in 1:3)
#{
 # a[,,i]=t(A[,,i])
  #b[,,i]=t(B[,,i])
  #c[,,i]=t(C[,,i])
  #d[,,i]=t(D[,,i])
#}
#/#distance_a=DirOut(a,DirOutmatrix=FALSE,h=0.55,method="Mah")$D
#/#distance_b=DirOut(b,DirOutmatrix=FALSE,h=0.55,method="Mah")$D
#/#distance_c=DirOut(c,DirOutmatrix=FALSE,h=0.55,method="Mah")$D
#/#distance_d=DirOut(d,DirOutmatrix=FALSE,h=0.55,method="Mah")$D
#/#