library(fda)
library(fields)
library(depth)

n=1000
p=50
R=150
alpha=function(i,nb,t){
  ((-1)^(i))*(4)*(t-0.6/nb)^2
}
beta=function(j,nb,t){
  if (nb==1)
  {
    3*(j-2)*cos(2*pi*t)
    }
  else{
    3*(j-2)*sin(2*pi*t)
  }
}

mu=function(t,nb)
{(3+nb)*t}
t=seq(0,1,len=p)

var=c(0.5,0.5)

rho=0.5

nu=matrix(c(0.5,(2+0.5)/2,(2+0.5)/2,2),nrow=2,ncol=2,byrow=TRUE)

a=matrix(c(2.5,2.5,2.5,2.5),nrow=2,ncol=2,byrow=TRUE)  ##scale
H<-matrix(0,nrow=p,ncol=p)
for (i in 1:p)
{
  for (j in 1:p)
  {
    H[i,j]<-abs(t[i]-t[j]) 
  }
}


matern_1<-matrix(0,nrow=p,ncol=p)
matern_12<-matrix(0,nrow=p,ncol=p)
matern_2<-matrix(0,nrow=p,ncol=p)

for (i in 1:p) { 
  for (j in 1:p){
matern_1[i,j]<-ifelse(H[i,j] != 0, var[1]*(a[1,1]%*%H[i,j])^nu[1,1]*besselK(a[1,1]%*%H[i,j], nu[1,1])/(2^(nu[1,1]-1)*gamma(nu[1,1])), var[1])
matern_12[i,j]<-ifelse(H[i,j] != 0, rho*sqrt(var[1]*var[2])*(a[1,2]%*%H[i,j])^nu[1,2]*besselK(a[1,2]%*%H[i,j], nu[1,2])/(2^(nu[1,2]-1)*gamma(nu[1,2])), rho*sqrt(var[1]*var[2]))
matern_2[i,j]<- ifelse(H[i,j] != 0, var[2]*(a[2,2]%*%H[i,j])^nu[2,2]*besselK(a[2,2]%*%H[i,j], nu[2,2])/(2^(nu[2,2]-1)*gamma(nu[2,2])), var[2])
  }
}
#covariance function in time
cov1=cbind(matern_1,matern_12)
cov2=cbind(matern_12,matern_2)
cov=rbind(cov1,cov2)
# Cholesky Decomposition
L=chol(cov)

Grand=matrix(NA,nrow=2*p,ncol=R)
Row1=matrix(NA,nrow=2*p,ncol=R)
Row2=matrix(NA,nrow=2*p,ncol=R)
Col1=matrix(NA,nrow=2*p,ncol=R)
Col2=matrix(NA,nrow=2*p,ncol=R)
Col3=matrix(NA,nrow=2*p,ncol=R)

mar_Grand=matrix(NA,nrow=2*p,ncol=R)
mar_Row1=matrix(NA,nrow=2*p,ncol=R)
mar_Row2=matrix(NA,nrow=2*p,ncol=R)
mar_Col1=matrix(NA,nrow=2*p,ncol=R)
mar_Col2=matrix(NA,nrow=2*p,ncol=R)
mar_Col3=matrix(NA,nrow=2*p,ncol=R)

mean_Grand=matrix(NA,nrow=2*p,ncol=R)
mean_Row1=matrix(NA,nrow=2*p,ncol=R)
mean_Row2=matrix(NA,nrow=2*p,ncol=R)
mean_Col1=matrix(NA,nrow=2*p,ncol=R)
mean_Col2=matrix(NA,nrow=2*p,ncol=R)
mean_Col3=matrix(NA,nrow=2*p,ncol=R)

A11=matrix(NA,nrow=2*p,ncol=n)
A12=matrix(NA,nrow=2*p,ncol=n)
A13=matrix(NA,nrow=2*p,ncol=n)
A21=matrix(NA,nrow=2*p,ncol=n)
A22=matrix(NA,nrow=2*p,ncol=n)
A23=matrix(NA,nrow=2*p,ncol=n)

q=matrix(c(0.07,0.03,0.05,0.05,0.07,0.03),nrow=2,ncol=3,byrow=T)

l=0.1
K=15
############################################
for (w in 1:R)
{
  e1<-t(mvrnorm(n = 1000, rep(0,100), cov))
  e2<-t(mvrnorm(n = 1000, rep(0,100), cov))
  e3<-t(mvrnorm(n = 1000, rep(0,100), cov))
  e4<-t(mvrnorm(n = 1000, rep(0,100), cov))
  e5<-t(mvrnorm(n = 1000, rep(0,100), cov))
  e6<-t(mvrnorm(n = 1000, rep(0,100), cov)) 
  
  for (i in 1:n)
  {
    A11[,i]=c(rep(rbinom(1,1,q[1,1]),2*p))
    A12[,i] =c(rep(rbinom(1,1,q[1,2]),2*p))
    A13[,i] =c(rep(rbinom(1,1,q[1,3]),2*p))
    A21[,i]=c(rep(rbinom(1,1,q[2,1]),2*p))
    A22[,i] =c(rep(rbinom(1,1,q[2,2]),2*p))
    A23[,i] =c(rep(rbinom(1,1,q[2,3]),2*p))
  }
  
  Mu=matrix(NA,nrow=2*p,ncol=n)
  Alpha1=matrix(NA,nrow=2*p,ncol=n)
  Alpha2=matrix(NA,nrow=2*p,ncol=n)
  Beta1=matrix(NA,nrow=2*p,ncol=n)
  Beta2=matrix(NA,nrow=2*p,ncol=n)
  Beta3=matrix(NA,nrow=2*p,ncol=n)
  Y11=matrix(NA,nrow=2*p,ncol=n)
  Y12=matrix(NA,nrow=2*p,ncol=n)
  Y13=matrix(NA,nrow=2*p,ncol=n)
  Y21=matrix(NA,nrow=2*p,ncol=n)
  Y22=matrix(NA,nrow=2*p,ncol=n)
  Y23=matrix(NA,nrow=2*p,ncol=n)
  
  for (i in 1:n)
  {
    Mu[,i]=c(mu(t,1),mu(t,2))
    Alpha1[,i]=c(alpha(1,1,t),alpha(1,2,t))
    Alpha2[,i]=c(alpha(2,1,t),alpha(2,2,t))
    Beta1[,i]=c(beta(1,1,t),beta(1,2,t))
    Beta2[,i]=c(beta(2,1,t),beta(2,2,t))
    Beta3[,i]=c(beta(3,1,t),beta(3,2,t))
  }
  
  #################################### Model 2 Setting
  for (i in 1:n)
  {
    
    for (k in 1:(p))
    {
      Y11[k,i]=Mu[k,i]+Alpha1[k,i]+Beta1[k,i]+e1[k,i]+K*A11[k,i]
      Y12[k,i]=Mu[k,i]+ Alpha1[k,i]+Beta2[k,i]+e2[k,i]+K*A12[k,i]
      Y13[k,i]=Mu[k,i]+ Alpha1[k,i]+Beta3[k,i]+e3[k,i]+K*A13[k,i]
      Y21[k,i]=Mu[k,i]+Alpha2[k,i]+Beta1[k,i]+e4[k,i]+K*A21[k,i]
      Y22[k,i]=Mu[k,i]+ Alpha2[k,i]+Beta2[k,i]+e5[k,i]+K*A22[k,i]
      Y23[k,i]=Mu[k,i]+ Alpha2[k,i]+Beta3[k,i]+e6[k,i]+K*A23[k,i]
    }  
    for (k in (p+1):(2*p))
    {
      Y11[k,i]=Mu[k,i]+Alpha1[k,i]+Beta1[k,i]+e1[k,i]-K*A11[k,i]
      Y12[k,i]=Mu[k,i]+ Alpha1[k,i]+Beta2[k,i]+e2[k,i]-K*A12[k,i]
      Y13[k,i]=Mu[k,i]+ Alpha1[k,i]+Beta3[k,i]+e3[k,i]-K*A13[k,i]
      Y21[k,i]=Mu[k,i]+Alpha2[k,i]+Beta1[k,i]+e4[k,i]-K*A21[k,i]
      Y22[k,i]=Mu[k,i]+ Alpha2[k,i]+Beta2[k,i]+e5[k,i]-K*A22[k,i]
      Y23[k,i]=Mu[k,i]+ Alpha2[k,i]+Beta3[k,i]+e6[k,i]-K*A23[k,i]
    }  
  }
  
  
  ###################################################################
 ###mean_based method
   mean_g=apply(cbind(Y11,Y12,Y13,Y21,Y22,Y23),1,mean)
  mean_r1=apply(cbind(Y11,Y12,Y13),1,mean)-mean_g
  mean_r2=apply(cbind(Y21,Y22,Y23),1,mean)-mean_g
  mean_c1=apply(cbind(Y11,Y21),1,mean)-mean_g
  mean_c2=apply(cbind(Y12,Y22),1,mean)-mean_g
  mean_c3=apply(cbind(Y13,Y23),1,mean)-mean_g
  
  mean_Grand[,w]=mean_g
  mean_Row1[,w]=mean_r1
  mean_Row2[,w]=mean_r2
  mean_Col1[,w]=mean_c1
  mean_Col2[,w]=mean_c2
  mean_Col3[,w]=mean_c3
 #################################################
  ###joint median based methods
  mr1=cbind(Y11,Y12,Y13)
  mr2=cbind(Y21,Y22,Y23)
  grand=0
  row1effect=0
  row2effect=0
  col1effect=0
  col2effect=0
  col3effect=0
  
  for (k in 1:4)
  {
    depth_row1=sbd(t(mr1))
    depth_row2=sbd(t(mr2))
    
    index1=order(depth_row1,decreasing=T)
    index2=order(depth_row2,decreasing=T)
    
    median_1=mr1[,index1==1]
    median_2=mr2[,index2==1]
    
    
    for (i in 1:(3*n))
    {
      mr1[,i]=mr1[,i]-median_1
      mr2[,i]=mr2[,i]-median_2
    }
    
    med_row=(median_1+median_2)/2
    row1effect=row1effect+median_1-med_row
    row2effect=row2effect+median_2-med_row
    mc1=cbind(mr1[,1:n],mr2[,1:n])
    mc2=cbind(mr1[,(1+n):(2*n)],mr2[,(1+n):(2*n)])
    mc3=cbind(mr1[,(1+2*n):(3*n)],mr2[,(1+2*n):(3*n)])
    
    depth_col1=sbd(t(mc1))
    depth_col2=sbd(t(mc2))
    depth_col3=sbd(t(mc3))
    
    Index1=order(sbd(t(mc1)),decreasing=T)
    Index2=order(sbd(t(mc2)),decreasing=T)
    Index3=order(sbd(t(mc3)),decreasing=T)
    
    
    med_col1=mc1[,Index1==1]
    med_col2=mc2[,Index2==1]
    med_col3=mc3[,Index3==1]
    
   
    for (i in 1:(2*n))
    {
      mc1[,i]=mc1[,i]-med_col1
      mc2[,i]=mc2[,i]-med_col2
      mc3[,i]=mc3[,i]-med_col3
    }
    
    mr1=cbind(mc1[,1:n],mc2[,1:n],mc3[,1:n])
    mr2=cbind(mc1[,(n+1):(2*n)],mc2[,(n+1):(2*n)],mc3[,(n+1):(2*n)])
    
    med_col=med_col2
    
    grand=grand+med_row+med_col
    col1effect=col1effect+med_col1-med_col
    col2effect=col2effect+med_col2-med_col
    col3effect=col3effect+med_col3-med_col
  }
  
  Grand[,w]=grand
  Row1[,w]=row1effect
  Row2[,w]=row2effect
  Col1[,w]=col1effect
  Col2[,w]=col2effect
  Col3[,w]=col3effect
  ######################################
  
  ###marginal median methods
  mar_mr1=cbind(Y11[1:p,],Y12[1:p,],Y13[1:p,])
  mar_mr2=cbind(Y21[1:p,],Y22[1:p,],Y23[1:p,])
  
  mar_mr3=cbind(Y11[(p+1):(2*p),],Y12[(p+1):(2*p),],Y13[(p+1):(2*p),])
  mar_mr4=cbind(Y21[(p+1):(2*p),],Y22[(p+1):(2*p),],Y23[(p+1):(2*p),])
  
  
  grand_v1=0
  row1effect_v1=0
  row2effect_v1=0
  col1effect_v1=0
  col2effect_v1=0
  col3effect_v1=0
  
  grand_v2=0
  row1effect_v2=0
  row2effect_v2=0
  col1effect_v2=0
  col2effect_v2=0
  col3effect_v2=0
  ############# same procedure for variable 1 and variable 2
  ############for variable 1
  for (k in 1:4)
  {
    mar_depth_row1=fbplot(mar_mr1,method='MBD',plot=FALSE)$depth
    mar_depth_row2=fbplot(mar_mr2,method='MBD',plot=FALSE)$depth
    
    mar_index1=order(mar_depth_row1,decreasing=T)
    mar_index2=order(mar_depth_row2,decreasing=T)
    
    mar_median_1=mar_mr1[,mar_index1==1]
    mar_median_2=mar_mr2[,mar_index2==1]
    

    for (i in 1:(3*n))
    {
      mar_mr1[,i]=mar_mr1[,i]-mar_median_1
      mar_mr2[,i]=mar_mr2[,i]-mar_median_2
    }
    
    mar_med_row=(mar_median_1+mar_median_2)/2
    row1effect_v1=row1effect_v1+mar_median_1-mar_med_row
    row2effect_v1=row2effect_v1+mar_median_2-mar_med_row
    mar_mc1=cbind(mar_mr1[,1:n],mar_mr2[,1:n])
    mar_mc2=cbind(mar_mr1[,(1+n):(2*n)],mar_mr2[,(1+n):(2*n)])
    mar_mc3=cbind(mar_mr1[,(1+2*n):(3*n)],mar_mr2[,(1+2*n):(3*n)])
    
    mar_depth_col1=fbplot(mar_mc1,method='MBD',plot=FALSE)$depth
    mar_depth_col2=fbplot(mar_mc2,method='MBD',plot=FALSE)$depth
    mar_depth_col3=fbplot(mar_mc3,method='MBD',plot=FALSE)$depth
    
    mar_Index1=order(mar_depth_col1,decreasing=T)
    mar_Index2=order(mar_depth_col2,decreasing=T)
    mar_Index3=order(mar_depth_col3,decreasing=T)
    
    
    mar_med_col1=mar_mc1[,mar_Index1==1]
    mar_med_col2=mar_mc2[,mar_Index2==1]
    mar_med_col3=mar_mc3[,mar_Index3==1]
    
    
    for (i in 1:(2*n))
    {
      mar_mc1[,i]=mar_mc1[,i]-mar_med_col1
      mar_mc2[,i]=mar_mc2[,i]-mar_med_col2
      mar_mc3[,i]=mar_mc3[,i]-mar_med_col3
    }
    mar_mr1=cbind(mar_mc1[,1:n],mar_mc2[,1:n],mar_mc3[,1:n])
    mar_mr2=cbind(mar_mc1[,(n+1):(2*n)],mar_mc2[,(n+1):(2*n)],mar_mc3[,(n+1):(2*n)])
    
    mar_med_col=mar_med_col2
     
    grand_v1=grand_v1+mar_med_row+mar_med_col
    col1effect_v1=col1effect_v1+mar_med_col1-mar_med_col
    col2effect_v1=col2effect_v1+mar_med_col2-mar_med_col
    col3effect_v1=col3effect_v1+mar_med_col3-mar_med_col
  }
  
  mar_Grand[1:p,w]=grand_v1
  mar_Row1[1:p,w]=row1effect_v1
  mar_Row2[1:p,w]=row2effect_v1
  mar_Col1[1:p,w]=col1effect_v1
  mar_Col2[1:p,w]=col2effect_v1
  mar_Col3[1:p,w]=col3effect_v1
  
  #########################
  ##### same procedure for variable 2
  for (k in 1:4)
  {
    mar_depth_row3=fbplot(mar_mr3,method='MBD',plot=FALSE)$depth
    mar_depth_row4=fbplot(mar_mr4,method='MBD',plot=FALSE)$depth
    
    mar_index3=order(mar_depth_row3,decreasing=T)
    mar_index4=order(mar_depth_row4,decreasing=T)
    
    mar_median_3=mar_mr3[,mar_index3==1]
    mar_median_4=mar_mr4[,mar_index4==1]
    
    
    for (i in 1:(3*n))
    {
      mar_mr3[,i]=mar_mr3[,i]-mar_median_3
      mar_mr4[,i]=mar_mr4[,i]-mar_median_4
    }
    
    mar_med_row_v2=(mar_median_3+mar_median_4)/2
    row1effect_v2=row1effect_v2+mar_median_3-mar_med_row_v2
    row2effect_v2=row2effect_v2+mar_median_4-mar_med_row_v2
    mar_mc4=cbind(mar_mr3[,1:n],mar_mr4[,1:n])
    mar_mc5=cbind(mar_mr3[,(1+n):(2*n)],mar_mr4[,(1+n):(2*n)])
    mar_mc6=cbind(mar_mr3[,(1+2*n):(3*n)],mar_mr4[,(1+2*n):(3*n)])
    
    mar_depth_col4=fbplot(mar_mc4,method='MBD',plot=FALSE)$depth
    mar_depth_col5=fbplot(mar_mc5,method='MBD',plot=FALSE)$depth
    mar_depth_col6=fbplot(mar_mc6,method='MBD',plot=FALSE)$depth
    
    mar_Index4=order(mar_depth_col4,decreasing=T)
    mar_Index5=order(mar_depth_col5,decreasing=T)
    mar_Index6=order(mar_depth_col6,decreasing=T)
    
    
    mar_med_col4=mar_mc4[,mar_Index4==1]
    mar_med_col5=mar_mc5[,mar_Index5==1]
    mar_med_col6=mar_mc6[,mar_Index6==1]
   
    
    for (i in 1:(2*n))
    {
      mar_mc4[,i]=mar_mc4[,i]-mar_med_col4
      mar_mc5[,i]=mar_mc5[,i]-mar_med_col5
      mar_mc6[,i]=mar_mc6[,i]-mar_med_col6
    }
    mar_mr3=cbind(mar_mc4[,1:n],mar_mc5[,1:n],mar_mc6[,1:n])
    mar_mr4=cbind(mar_mc4[,(n+1):(2*n)],mar_mc5[,(n+1):(2*n)],mar_mc6[,(n+1):(2*n)])
    
    mar_med_col_v2=mar_med_col5
     
    grand_v2=grand_v2+mar_med_row_v2+mar_med_col_v2
    col1effect_v2=col1effect_v2+mar_med_col4-mar_med_col_v2
    col2effect_v2=col2effect_v2+mar_med_col5-mar_med_col_v2
    col3effect_v2=col3effect_v2+mar_med_col6-mar_med_col_v2
  }
  
  mar_Grand[(p+1):(2*p),w]=grand_v2
  mar_Row1[(p+1):(2*p),w]=row1effect_v2
  mar_Row2[(p+1):(2*p),w]=row2effect_v2
  mar_Col1[(p+1):(2*p),w]=col1effect_v2
  mar_Col2[(p+1):(2*p),w]=col2effect_v2
  mar_Col3[(p+1):(2*p),w]=col3effect_v2

}
par(cex.axis=1.3,cex.lab=1.3,cex.main=1.3,pin=c(3.3,2.7))
par(mfrow=c(1,3),mgp=c(3,1,0),bty="o",mar=c(4.5,4.5,3.2,3))
################
## plot of MSBD estimation for variable 1
fbplot(Grand[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-3,5.8),main="Model 2 (Median) variable 1",ylab="Grand Effect",xlab="t")
lines(t,4*t,lty=6,col = "green",lwd=1.8)
fbplot(Row1[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-4,3.5),main="Model 2 (Median) variable 1 ",ylab="Row Effect 1",xlab="t")
lines(t,-4*(t-0.6)^2,lty=6,col = "green",lwd=1.8)
fbplot(Col1[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-6.7,6.5),main="Model 2 (Median) variable 1",ylab="Column Effect 1",xlab="t")
lines(t,-3*cos(2*pi*t),lty=6,col = "green",lwd=1.8)
##################
## plot of MBD estimation for variable 1
fbplot(mar_Grand[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-3,6),main="Model 2 (Median) variable 1",ylab="Grand Effect",xlab="t")
lines(t,4*t,lty=6,col = "green",lwd=1.8)
fbplot(mar_Row1[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-4,3.5),main="Model 2 (Median) variable 1 ",ylab="Row Effect 1",xlab="t")
lines(t,-4*(t-0.6)^2,lty=6,col = "green",lwd=1.8)
fbplot(mar_Col1[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-6.7,6.5),main="Model 2 (Median) variable 1",ylab="Column Effect 1",xlab="t")
lines(t,-3*cos(2*pi*t),lty=6,col = "green",lwd=1.8)
#######################
######### plot of mean estimation for variable 1

fbplot(mean_Grand[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-3,6),main="Model 2 (Mean) variable 1",ylab="Grand Effect",xlab="t")
lines(t,4*t,lty=6,col = "green",lwd=1.8)
fbplot(mean_Row1[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-4,3.5),main="Model 2 (Mean) variable 1 ",ylab="Row Effect 1",xlab="t")
lines(t,-4*(t-0.6)^2,lty=6,col = "green",lwd=1.8)
fbplot(mean_Col1[1:p,],method='MBD',x=t,xlim=c(0,1),ylim=c(-6.7,6.5),main="Model 2 (Mean) variable 1",ylab="Column Effect 1",xlab="t")
lines(t,-3*cos(2*pi*t),lty=6,col = "green",lwd=1.8)

########################
############ plot of MSBD estimation for variable 2
fbplot(Grand[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-3,8),main="Model 2 (Median) variable 2 ",ylab="Grand Effect",xlab="t")
lines(t,5*t,lty=6,col = "green",lwd=1.8)
fbplot(Row1[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-3.5,3),main="Model 2 (Median) variable 2 ",ylab="Row Effect 1",xlab="t")
lines(t,-4*(t-0.3)^2,lty=6,col = "green",lwd=1.8)
fbplot(Col1[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-8,9),main="Model 2 (Median) variable 2 ",ylab="Column Effect 1",xlab="t")
lines(t,-3*sin(2*pi*t),lty=6,col = "green",lwd=1.8)
#########################
######### plot of MBD estimation for variable 2
fbplot(mar_Grand[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-3,8),main="Model 2 (Median) variable 2 ",ylab="Grand Effect",xlab="t")
lines(t,5*t,lty=6,col = "green",lwd=1.8)
fbplot(mar_Row1[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-3.5,3),main="Model 2 (Median) variable 2 ",ylab="Row Effect 1",xlab="t")
lines(t,-4*(t-0.3)^2,lty=6,col = "green",lwd=1.8)
fbplot(mar_Col1[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-8,9),main="Model 2 (Median) variable 2 ",ylab="Column Effect 1",xlab="t")
lines(t,-3*sin(2*pi*t),lty=6,col = "green",lwd=1.8)
#############################
########### plot of mean estimation for variable 2
fbplot(mean_Grand[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-3,8),main="Model 2 (Mean) variable 2 ",ylab="Grand Effect",xlab="t")
lines(t,5*t,lty=6,col = "green",lwd=1.8)
fbplot(mean_Row1[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-3.5,3),main="Model 2 (Mean) variable 2 ",ylab="Row Effect 1",xlab="t")
lines(t,-4*(t-0.3)^2,lty=6,col = "green",lwd=1.8)
fbplot(mean_Col1[(p+1):(2*p),],method='MBD',x=t,xlim=c(0,1),ylim=c(-8,9),main="Model 2 (Mean) variable 2 ",ylab="Column Effect 1",xlab="t")
lines(t,-3*sin(2*pi*t),lty=6,col = "green",lwd=1.8)
#################################



