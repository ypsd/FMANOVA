library(fda)
library(fields)
library(depth)
library(dplyr)
library(MASS)

n=1000
p=50
R=150

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


mar_Grand=matrix(NA,nrow=2*p,ncol=R)


mean_Grand=matrix(NA,nrow=2*p,ncol=R)


l=0.1
K=15
############################################

for (r in 1:R)
{
  random1<-mvrnorm(n = 1000, rep(0,100), cov)
  for (i in 200:270)
  {
    for (j in 1:50)
    {
      random1[i,j]<-random1[i,j]-runif(0.8,1)*4*(t[j]-0.6)^2
    }
    
    for (j in 51:100)
    {
      random1[i,j]<-random1[i,j]
    }
  }
  m<-t(random1)
  ############# mean_based method
  avg<-apply(m,1,mean)
  mean_Grand[,r]<-avg
  m1<-t(random1[,1:50])
  m2<-t(random1[,51:100])
  ##################
  ########### MSBD median method
  dth=sbd(random1)
  Index=order(dth,decreasing=T)
  med=m[,which(Index==1)]
  
  Grand[,r]=med
  
  #################
  ######### MBD median method
  dth1=fbplot(m1,method='MBD')$depth
  Index1=order(dth1,decreasing=T)
  med_1=m1[,which(Index1==1)]
  
  dth2=fbplot(m2,method='MBD')$depth
  Index2=order(dth2,decreasing=T)
  med_2=m2[,which(Index2==1)]
  
  mar_Grand[1:p,r]=med_1
  mar_Grand[51:100,r]=med_2
}


par(cex.axis=1.3,cex.lab=1.3,cex.main=1.3,pin=c(3.3,2.7))
par(mfrow=c(1,3),mgp=c(3,1,0),bty="o",mar=c(4.5,4.5,3.2,3))

################
###########MSBD median based empirical
emp <- cor(t(Grand))
theo <- cov2cor(cov)

tlocs <- base::t(matrix(rep(1:50,50), ncol=50, byrow=F))
hlag<-c()
loc <- 1
hlag <- tlocs[loc,] - loc
emp_vals <- cbind(emp[1:50,1:50][,loc], emp[51:100,51:100][,loc], emp[1:50,51:100][,loc])

for(loc in 2:50){
  hlag <- c(hlag, abs(tlocs[loc,] - loc))
  emp_vals <- rbind(emp_vals, cbind(emp[1:50,1:50][,loc], emp[51:100,51:100][,loc], emp[1:50,51:100][,loc]))
}

empirical <- data.frame(hlag, emp_vals)
colnames(empirical) <- c('hlag', 'var1_cor', 'var2_cor', 'cross_cor')


binned.1 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(var1_cor))
binned.2 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(var2_cor))
binned.3 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(cross_cor))

binned_orig <- cbind(binned.1$hlag, binned.1$avg1, binned.2$avg1, binned.3$avg1)

plot(binned_orig[,4], main = paste(expression(CCF),"of Estimation from MSBD"), ylim = c(-0.07, 0.52),ylab='Cross-Correlation',xlab="Lag",type="l")
lines(theo[1, 51:100], col=2,  ylim = c(-0.07, 0.52),lty=2)
legend("topright", legend = c("Empirical ccf", "Theoretical ccf "),
       text.width = strwidth("1,000,000"),
       col=1:2,
       lty = 1:2, xjust = 1, yjust = 1,cex=0.7)
#########################################
############mean_based empirical
emp <- cor(t(mean_Grand))
theo <- cov2cor(cov)

tlocs <- base::t(matrix(rep(1:50,50), ncol=50, byrow=F))
hlag<-c()
loc <- 1
hlag <- tlocs[loc,] - loc
emp_vals <- cbind(emp[1:50,1:50][,loc], emp[51:100,51:100][,loc], emp[1:50,51:100][,loc])

for(loc in 2:50){
  hlag <- c(hlag, abs(tlocs[loc,] - loc))
  emp_vals <- rbind(emp_vals, cbind(emp[1:50,1:50][,loc], emp[51:100,51:100][,loc], emp[1:50,51:100][,loc]))
}

empirical <- data.frame(hlag, emp_vals)
colnames(empirical) <- c('hlag', 'var1_cor', 'var2_cor', 'cross_cor')


binned.1 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(var1_cor))
binned.2 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(var2_cor))
binned.3 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(cross_cor))

binned_orig <- cbind(binned.1$hlag, binned.1$avg1, binned.2$avg1, binned.3$avg1)

plot(binned_orig[,4], main = paste(expression(CCF),"of Estimation from Mean"), ylim = c(-0.07, 0.52),ylab='Cross-Correlation',xlab="Lag",type="l")
lines(theo[1, 51:100], col=2,  ylim = c(-0.07, 0.52),lty=2)
legend("topright", legend = c("Empirical ccf", "Theoretical ccf "),
       text.width = strwidth("1,000,000"),
       col=1:2,
       lty = 1:2, xjust = 1, yjust = 1,cex=0.7)
###############################


################## MBD median based
emp <- cor(t(mar_Grand))
theo <- cov2cor(cov)

tlocs <- base::t(matrix(rep(1:50,50), ncol=50, byrow=F))
hlag<-c()
loc <- 1
hlag <- tlocs[loc,] - loc
emp_vals <- cbind(emp[1:50,1:50][,loc], emp[51:100,51:100][,loc], emp[1:50,51:100][,loc])

for(loc in 2:50){
  hlag <- c(hlag, abs(tlocs[loc,] - loc))
  emp_vals <- rbind(emp_vals, cbind(emp[1:50,1:50][,loc], emp[51:100,51:100][,loc], emp[1:50,51:100][,loc]))
}

empirical <- data.frame(hlag, emp_vals)
colnames(empirical) <- c('hlag', 'var1_cor', 'var2_cor', 'cross_cor')

binned.1 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(var1_cor))
binned.2 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(var2_cor))
binned.3 <- empirical %>% group_by(hlag) %>% summarize(avg1=mean(cross_cor))

binned_orig <- cbind(binned.1$hlag, binned.1$avg1, binned.2$avg1, binned.3$avg1)

plot(binned_orig[,4], main = paste(expression(CCF),"of Estimation from MBD"), ylim = c(-0.15, 0.52),ylab='Cross-Correlation',xlab="Lag",type="l")
lines(theo[1, 51:100], col=2,  ylim = c(-0.15, 0.52),lty=2)
legend("topright", legend = c("Empirical ccf", "Theoretical ccf "),
       text.width = strwidth("1,000,000"),
       col=1:2,
       lty = 1:2, xjust = 1, yjust = 1,cex=0.7)