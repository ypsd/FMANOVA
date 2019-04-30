
library(fda.usc)
data("aemet")
m=median(aemet$df$longitude)
n=median(aemet$df$latitude)
k=400
x1=c()
x2=c()
x3=c()
x4=c()
x5=c()
x6=c()
x7=c()
x8=c()
for (i in 1: length(aemet$df$longitude))
{
  if (aemet$df$longitude[i] <m & aemet$df$latitude[i]<n & aemet$df$altitude[i]<k)
    x1=cbind(x1,i)  ## southwest low
  if (aemet$df$longitude[i] >=m & aemet$df$latitude[i]<n& aemet$df$altitude[i]<k)
    x2=cbind(x2,i)  ##southeast low
  if (aemet$df$longitude[i] <m & aemet$df$latitude[i]>=n& aemet$df$altitude[i]<k)
    x3=cbind(x3,i)  ##northwest low
  if (aemet$df$longitude[i] >=m & aemet$df$latitude[i]>=n& aemet$df$altitude[i]<k)
    x4=cbind(x4,i)  ##northeast low
  if (aemet$df$longitude[i] <m & aemet$df$latitude[i]<n & aemet$df$altitude[i]>=k)
    x5=cbind(x5,i)  ##southwest high
  if (aemet$df$longitude[i] >=m & aemet$df$latitude[i]<n& aemet$df$altitude[i]>=k)
    x6=cbind(x6,i)  ##southeast high
  if (aemet$df$longitude[i] <m & aemet$df$latitude[i]>=n& aemet$df$altitude[i]>=k)
    x7=cbind(x7,i)  ##northwest high
  if (aemet$df$longitude[i] >=m & aemet$df$latitude[i]>=n& aemet$df$altitude[i]>=k)
    x8=cbind(x8,i)  ##northeast high
}
Y=matrix(0,nrow=73, ncol=1095)
for (i in 1:73)
{
  x=c(aemet$temp$data[i,],aemet$logprec$data[i,],aemet$wind.speed$data[i,])
  Y[i,]=as.vector(x)
}

r1=Y[c(x1,x5),]
r2=Y[c(x2,x6),]
r3=Y[c(x3,x7),]
r4=Y[c(x4,x8),]

##row effects x1 x2 x3 x4
##row effetcs x5 x6 x7 x8
##column effects x1 x5
##column effects x2 x6
##column effects x3 x7
##column effects x4 x8
row1=Y[c(x1,x2,x3,x4),]
row2=Y[c(x5,x6,x7,x8),]
geffect=0
reffect1=0
reffect2=0
ceffect1=0
ceffect2=0
ceffect3=0
ceffect4=0
mean_g=apply(Y,2,mean)
mean_r1=apply(row1,2,mean)-mean_g
mean_r2=apply(row2,2,mean)-mean_g
mean_c1=apply(Y[c(x1,x5),],2,mean)-mean_g
mean_c2=apply(Y[c(x2,x6),],2,mean)-mean_g
mean_c3=apply(Y[c(x3,x7),],2,mean)-mean_g
mean_c4=apply(Y[c(x4,x8),],2,mean)-mean_g

for (t in 1:3)
{
  depth_row1=trivmbd(row1)
  depth_row2=trivmbd(row2)
  
  
  medium_row1=row1[order(depth_row1,decreasing=T)[1],]
  medium_row2=row2[order(depth_row2,decreasing=T)[1],]
  
  med_row=(medium_row1+medium_row2)/2
  reffect1=reffect1+medium_row1-med_row
  reffect2=reffect2+medium_row2-med_row
  geffect=geffect+med_row
  for (i in 1: nrow(row1))
  {
    row1[i,]=row1[i,]-medium_row1
  }
  for (i in 1: nrow(row2))
  {
    row2[i,]=row2[i,]-medium_row2
  }
  col1=rbind(row1[1:length(x1),],row2[1:length(x5),])
  col2=rbind(row1[(length(x1)+1):(length(c(x1,x2))),],row2[(length(x5)+1):(length(c(x5,x6))),])
  col3=rbind(row1[(length(c(x1,x2))+1):(length(c(x1,x2,x3))),],row2[(length(c(x5,x6))+1):length(c(x5,x6,x7)),])
  col4=rbind(row1[(length(c(x1,x2,x3))+1):length(c(x1,x2,x3,x4)),],row2[(length(c(x5,x6,x7))+1):length(c(x5,x6,x7,x8)),]) 
  
  depth_col1=trivmbd(col1)
  depth_col2=trivmbd(col2)
  depth_col3=trivmbd(col3)
  depth_col4=trivmbd(col4)
  
  med_col1=col1[order(depth_col1,decreasing=T)[1],]
  med_col2=col2[order(depth_col2,decreasing=T)[1],]
  med_col3=col3[order(depth_col3,decreasing=T)[1],]
  med_col4=col4[order(depth_col4,decreasing=T)[1],]
  
  med_col=rbind(med_col1,med_col2,med_col3,med_col4)[order(trivmbd(rbind(med_col1,med_col2,med_col3,med_col4)),decreasing=T)[1],]
  
  geffect=geffect+med_col
  ceffect1=ceffect1+med_col1-med_col
  ceffect2=ceffect2+med_col2-med_col
  ceffect3=ceffect3+med_col3-med_col
  ceffect4=ceffect4+med_col4-med_col
  
  for (i in 1:nrow(col1))
  {
    col1[i,]=col1[i,]-med_col1
  }
  
  for (i in 1:nrow(col2))
  {
    col2[i,]=col2[i,]-med_col2
  }
  
  for (i in 1:nrow(col3))
  {
    col3[i,]=col3[i,]-med_col3
  }
  
  for (i in 1:nrow(col4))
  {
    col4[i,]=col4[i,]-med_col4
  }
  
  row1=rbind(col1[1:length(x1),],col2[1:length(x2),],col3[1:length(x3),],col4[1:length(x4),])
  row2=rbind(col1[(length(x1)+1):nrow(col1),],col2[(length(x2)+1):nrow(col2),],col3[(length(x3)+1):nrow(col3),],col4[(length(x4)+1):nrow(col4),])
}

par(cex.axis=1.3,cex.lab=1.3,cex.main=1.3,pin=c(3.3,2.7))
par(mfrow=c(1,3),mgp=c(3,1,0),bty="o",mar=c(4.5,4.5,3.2,3),cex.axis=1,cex.lab=1,cex.main=1)
plot(c(1:365),geffect[1:365],ylab="Grand Effect",xlab="Time (Days)",main="Variable: Temperature",type="l",col="black",lty=1,ylim=c(8,26))
lines(c(1:365),mean_g[1:365],lty=2,col=6)
legend("topright", legend = c("Median  ", "Mean  "),
       lty = 1:2,col=c("black",6),xjust = 1, yjust = 1,cex=0.7)
plot(c(1:365),smooth(geffect[366:730]),ylab="Grand Effect",xlab="Time (Days)",main="Variable: log10 Precipitation",type="l",col="black",lty=1,ylim=c(-6.5,2))
lines(c(1:365),smooth(mean_g[366:730]),lty=2,col=6)
legend("top", legend = c("Median  ", "Mean  "),
       lty = 1:2,col=c("black",6),xjust = 1, yjust = 1,cex=0.7)
plot(c(1:365),smooth(geffect[731:1095]),ylab="Grand Effect",xlab="Time (Days)",main="Variable: Wind Speed",type="l",col="black",lty=1,ylim = c(2.2,4.3))
lines(c(1:365),(mean_g[731:1095]),lty=2,col=6)
legend("top", legend = c("Median  ", "Mean  "),
       lty = 1:2,col=c("black",6),xjust = 1, yjust = 1,cex=0.7)



#par(cex.axis=1.3,cex.lab=1.3,cex.main=1.3,pin=c(3.3,2.7))
par(mfrow=c(3,2),mgp=c(3,1,0),bty="o",mar=c(4.5,4.5,3.2,3),cex.axis=1,cex.lab=1,cex.main=1.1)
plot(c(1:365),ceffect1[1:365],ylab="Region Effect", main="Temperature (Median)",col=2,xlab="Time (Days)",ylim=c(-8.5,4.5),type='l')
lines(c(1:365),ceffect2[1:365],col=3,lty=2)
lines(c(1:365),ceffect3[1:365],col=4,lty=3)
lines(c(1:365),ceffect4[1:365],col=5,lty=4)
legend("topleft", legend = c("Southwest", "Southeast","Northwest","Northeast"),
       lty = c(1,2,3,4),col=c(2,3,4,5),xjust = 1, yjust = 1,cex=0.6)

plot(c(1:365),mean_c1[1:365],ylab="Region Effect", main="Temperature (Mean)",col=2,xlab="Time (Days)",ylim=c(-8.5,4.5),type='l')
lines(c(1:365),mean_c2[1:365],col=3,lty=2)
lines(c(1:365),mean_c3[1:365],col=4,lty=3)
lines(c(1:365),mean_c4[1:365],col=5,lty=4)
legend("bottom", legend = c("Southwest", "Southeast","Northwest","Northeast"),
       lty = c(1,2,3,4),col=c(2,3,4,5),xjust = 1, yjust = 1,cex=0.6)

plot(c(1:365), smooth(ceffect1[366:730]),ylab="Region Effect", main="log10 Precipitation (Median)",col=2,xlab="Time (Days)",ylim=c(-3,6.5),type='l')
lines(c(1:365), smooth(ceffect2[366:730]),col=3,lty=2)
lines(c(1:365), smooth(ceffect3[366:730]),col=4,lty=3)
lines(c(1:365), smooth(ceffect4[366:730]),col=5,lty=4)
legend("topleft", legend = c("Southwest", "Southeast","Northwest","Northeast"),
       lty = c(1,2,3,4),col=c(2,3,4,5),xjust = 1, yjust = 1,cex=0.6)

plot(c(1:365), smooth(mean_c1[366:730]),ylab="Region Effect", main="log10 Precipitation (Mean)",col=2,xlab="Time (Days)",ylim=c(-3,6.5),type='l')
lines(c(1:365), smooth(mean_c2[366:730]),col=3,lty=2)
lines(c(1:365),smooth(mean_c3[366:730]),col=4,lty=3)
lines(c(1:365),smooth(mean_c4[366:730]),col=5,lty=4)
legend("topleft", legend = c("Southwest", "Southeast","Northwest","Northeast"),
       lty = c(1,2,3,4),col=c(2,3,4,5),xjust = 1, yjust = 1,cex=0.6)

plot(c(1:365),smooth(ceffect1[731:1095]),ylab="Region Effect", main="Wind Speed (Median)",col=2,xlab="Time (Days)",ylim=c(-1.9,1.6),type='l')
lines(c(1:365),smooth(ceffect2[731:1095]),col=3,lty=2)
lines(c(1:365),smooth(ceffect3[731:1095]),col=4,lty=3)
lines(c(1:365),smooth(ceffect4[731:1095]),col=5,lty=4)
legend("bottom", legend = c("Southwest", "Southeast","Northwest","Northeast"),
       lty = c(1,2,3,4),col=c(2,3,4,5),xjust = 1, yjust = 1,cex=0.6)

plot(c(1:365),smooth(mean_c1[731:1095]),ylab="Region Effect", main="Wind Speed (Mean)",col=2,xlab="Time (Days)",ylim=c(-1.9,1.6),type='l')
lines(c(1:365),smooth(mean_c2[731:1095]),col=3,lty=2)
lines(c(1:365),smooth(mean_c3[731:1095]),col=4,lty=3)
lines(c(1:365),smooth(mean_c4[731:1095]),col=5,lty=4)
legend("bottom", legend = c("Southwest", "Southeast","Northwest","Northeast"),
       lty = c(1,2,3,4),col=c(2,3,4,5),xjust = 1, yjust = 1,cex=0.6)

plot(c(1:365),reffect1[1:365],ylab="Altitude Effect", main="Temperature (Median)",lty=1,col=2,xlab="Time (Days)",ylim=c(-4,4),type='l')
lines(c(1:365),reffect2[1:365],col=3,lty=2)
legend("top", legend = c("Low Altitude ", "High Altitude "),
      lty = 1:2,col=2:3,xjust = 1, yjust = 1,cex=0.65)

plot(c(1:365),mean_r1[1:365],ylab="Altitude Effect", main="Temperature (Mean)",lty=1,col=2,xlab="Time (Days)",ylim=c(-4,4),type='l')
lines(c(1:365),mean_r2[1:365],col=3,lty=2)
legend("top", legend = c("Low Altitude ", "High Altitude "),
       lty = 1:2,col=2:3,xjust = 1, yjust = 1,cex=0.65)


plot(c(1:365),smooth(reffect1[366:730]),ylab="Altitude Effect", main="log10 Precipitation (Median)",lty=1,col=2,xlab="Time (Days)",ylim=c(-1.6,1.5),type='l')
lines(c(1:365),smooth(reffect2[366:730]),col=3,lty=2)
legend("bottomleft", legend = c("Low Altitude ", "High Altitude "),
       lty = 1:2,col=2:3,xjust = 1, yjust = 1,cex=0.65)


plot(c(1:365),smooth(mean_r1[366:730]),ylab="Altitude Effect", main="log10 Precipitation (Mean)",lty=1,col=2,xlab="Time (Days)",ylim=c(-1.6,1.5),type='l')
lines(c(1:365),smooth(mean_r2[366:730]),col=3,lty=2)
legend("bottomleft", legend = c("Low Altitude ", "High Altitude "),
       lty = 1:2,col=2:3,xjust = 1, yjust = 1,cex=0.65)


plot(c(1:365),reffect1[731:1095],ylab="Altitude Effect", main="Wind Speed (Median)",lty=1,col=2,xlab="Time (Days)",ylim=c(-1.3,1.3),type='l')
lines(c(1:365),reffect2[731:1095],col=3,lty=2)
legend("topright", legend = c("Low Altitude ", "High Altitude "),
       lty = 1:2,col=2:3,xjust = 1, yjust = 1,cex=0.65)

plot(c(1:365),mean_r1[731:1095],ylab="Altitude Effect", main="Wind Speed (Mean)",lty=1,col=2,xlab="Time (Days)",ylim=c(-1.3,1.3),type='l')
lines(c(1:365),mean_r2[731:1095],col=3,lty=2)
legend("topright", legend = c("Low Altitude ", "High Altitude "),
       lty = 1:2,col=2:3,xjust = 1, yjust = 1,cex=0.65)

## Region effect
r1=Y[c(x1,x5),]
r2=Y[c(x2,x6),]
r3=Y[c(x3,x7),]
r4=Y[c(x4,x8),]

par(cex.axis=1,cex.lab=1,cex.main=1,pin=c(3.3,2.7))
par(mfrow=c(2,2),mgp=c(3,1,0),bty="o",mar=c(4.5,4.5,3.2,3),cex.axis=1,cex.lab=1,cex.main=1)

plot(c(1:365),r1[1,1:365],ylab="Temperature",main="Southwest Region",col=1,lty=1,ylim=c(-1,28.5),xlab="Time (Days)")
for (i in 2:nrow(r1))
 { 
  lines(c(1:365),r1[i,1:365],col=i)
}

plot(c(1:365),r2[1,1:365],ylab="Temperature",main="Southeast Region",col=1,lty=1,ylim=c(-1,28.5),xlab="Time (Days)")
for (i in 2:nrow(r2))
 {
  lines(c(1:365),r2[i,1:365],col=i)
}

plot(c(1:365),r3[1,1:365],ylab="Temperature",main="Northwest Region",col=1,lty=1,ylim=c(-1,28.5),xlab="Time (Days)")
for (i in 2:nrow(r3))
 {
  lines(c(1:365),r3[i,1:365],col=i)
}
plot(c(1:365),r4[1,1:365],ylab="Temperature",main="Northeast Region",col=1,lty=1,ylim=c(-1,28.5),xlab="Time (Days)")
for (i in 2:nrow(r4))
  {
  lines(c(1:365),r4[i,1:365],col=i)
  }

par(cex.axis=1,cex.lab=1,cex.main=1,pin=c(3.3,2.7))
par(mfrow=c(2,2),mgp=c(3,1,0),bty="o",mar=c(4.5,4.5,3.2,3),cex.axis=1,cex.lab=1,cex.main=1)
plot(c(1:365),r1[1,731:1095],ylab="Wind Speed",main="Southwest Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:nrow(r1))
  lines(c(1:365),r1[i,731:1095],col=i)

plot(c(1:365),r2[1,731:1095],ylab="Wind Speed",main="Southeast Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:nrow(r2))
  lines(c(1:365),r2[i,731:1095],col=i)

plot(c(1:365),r3[1,731:1095],ylab="Wind Speed",main="Northwest Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:nrow(r3))
  lines(c(1:365),r3[i,731:1095],col=i)

plot(c(1:365),r4[1,731:1095],ylab="Wind Speed",main="Northeast Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:nrow(r4))
  lines(c(1:365),r4[i,731:1095],col=i)

plot(c(1:365),smooth(r1[366:730,1]),ylab="log Precipitation",main="Southwest Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:ncol(r1))
  lines(c(1:365),smooth(r1[366:730,i]),col=i)

plot(c(1:365),smooth(r2[366:730,1]),ylab="log Precipitation",main="Southeast Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:ncol(r2))
  lines(c(1:365),smooth(r2[366:730,i]),col=i)

plot(c(1:365),smooth(r3[366:730,1]),ylab="log Precipitation",main="Northwest Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:ncol(r3))
  lines(c(1:365),smooth(r3[366:730,i]),col=i)

plot(c(1:365),smooth(r4[366:730,1]),ylab="log Precipitation",main="Northeast Region",col=1,lty=1,ylim=c(0.5,12),xlab="Time (Days)",type='l')
for (i in 2:ncol(r4))
  lines(c(1:365),smooth(r4[366:730,i]),col=i)


## Altitude effect
low=Y[c(x1,x2,x3,x4),]
high=Y[c(x5,x6,x7,x8),]
plot(c(1:365),low[1,1:365],ylab="Temperature",main="Low Altitude Region",col=2,lty=4,ylim=c(-1,30),xlab="Time (Days)")
for (i in 2:nrow(low))
lines(c(1:365),low[i,1:365],col=i+1,lty=i+1)

plot(c(1:365),high[1,1:365],ylab="Temperature",main="High Altitude Region",col=2,ylim=c(-1,30),xlab="Time (Days)")
for (i in 2:nrow(high))
  lines(c(1:365),high[i,1:365],col=i+1,lty=i+1)

plot(c(1:365),smooth(low[1,731:1095]),ylab="Wind Speed",main="Low Altitude Region",col=2,ylim=c(0,11.5),xlab="Time (Days)")
for (i in 2:nrow(low))
  lines(smooth(low[i,731:1095]),col=i+1,lty=i+1)

plot(c(1:365),smooth(high[1,731:1095]),ylab="Wind Speed",main="High Altitude Region",col=1,lty=1,ylim=c(0,11.5),xlab="Time (Days)")
for (i in 2:nrow(high))
  lines(c(1:365),smooth(high[i,731:1095]),col=i,lty=i)
par(mfrow=c(1,2))
par(cex.axis=1.3,cex.lab=1.3,cex.main=1,pin=c(3.3,2.7))
par(mfrow=c(1,2),mgp=c(3,1,0),bty="o",mar=c(4.5,4.5,3.2,3),cex.axis=0.8,cex.lab=0.8,cex.main=0.87)
plot(c(1:365),smooth(low[1,366:730]),ylab="log10 Precipitation",main="Low Altitude Region",col=1,lty=1,ylim=c(-7,4),xlab="Time (Days)",type='l')
for (i in 2:nrow(low))
  lines(c(1:365),smooth(low[i,366:730]),col=i)

plot(c(1:365),smooth(high[1,366:730]),ylab="log10 Precipitation",main="High Altitude Region",col=1,lty=1,ylim=c(-7,4),xlab="Time (Days)",type='l')
for (i in 2:nrow(high))
  lines(c(1:365),smooth(high[i,366:730]),col=i)

