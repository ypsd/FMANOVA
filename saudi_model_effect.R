uas.array76_average<-array(NA,dim=c(464,236,365))
uas.array81_average<-array(NA,dim=c(464,236,365))
uas.array86_average<-array(NA,dim=c(464,236,365))
uas.array91_average<-array(NA,dim=c(464,236,365))
uas.array96_average<-array(NA,dim=c(464,236,365))
uas.array01_average<-array(NA,dim=c(464,236,365))
vas.array76_average<-array(NA,dim=c(464,236,365))
vas.array81_average<-array(NA,dim=c(464,236,365))
vas.array86_average<-array(NA,dim=c(464,236,365))
vas.array91_average<-array(NA,dim=c(464,236,365))
vas.array96_average<-array(NA,dim=c(464,236,365))
vas.array01_average<-array(NA,dim=c(464,236,365))
uas<-array(NA,dim=c(464,236,365))
vas<-array(NA,dim=c(464,236,365))
for (i in 1:365)
{
  uas.array76_average[,,i]<-(uas.array76[,,i]+uas.array76[,,(i+365)]+uas.array76[,,(i+730)]+uas.array76[,,(i+1095)]+uas.array76[,,(i+1460)])
  uas.array81_average[,,i]<-(uas.array81[,,i]+uas.array81[,,(i+365)]+uas.array81[,,(i+730)]+uas.array81[,,(i+1095)]+uas.array81[,,(i+1460)])
  uas.array86_average[,,i]<-(uas.array86[,,i]+uas.array86[,,(i+365)]+uas.array86[,,(i+730)]+uas.array86[,,(i+1095)]+uas.array86[,,(i+1460)])
  uas.array91_average[,,i]<-(uas.array91[,,i]+uas.array91[,,(i+365)]+uas.array91[,,(i+730)]+uas.array91[,,(i+1095)]+uas.array91[,,(i+1460)])
  uas.array96_average[,,i]<-(uas.array96[,,i]+uas.array96[,,(i+365)]+uas.array96[,,(i+730)]+uas.array96[,,(i+1095)]+uas.array96[,,(i+1460)])
  uas.array01_average[,,i]<-(uas.array01[,,i]+uas.array01[,,(i+365)]+uas.array01[,,(i+730)]+uas.array01[,,(i+1095)]+uas.array01[,,(i+1460)])
  vas.array76_average[,,i]<-(vas.array76[,,i]+vas.array76[,,(i+365)]+vas.array76[,,(i+730)]+vas.array76[,,(i+1095)]+vas.array76[,,(i+1460)])
  vas.array81_average[,,i]<-(vas.array81[,,i]+vas.array81[,,(i+365)]+vas.array81[,,(i+730)]+vas.array81[,,(i+1095)]+vas.array81[,,(i+1460)])
  vas.array86_average[,,i]<-(vas.array86[,,i]+vas.array86[,,(i+365)]+vas.array86[,,(i+730)]+vas.array86[,,(i+1095)]+vas.array86[,,(i+1460)])
  vas.array91_average[,,i]<-(vas.array91[,,i]+vas.array91[,,(i+365)]+vas.array91[,,(i+730)]+vas.array91[,,(i+1095)]+vas.array91[,,(i+1460)])
  vas.array96_average[,,i]<-(vas.array96[,,i]+vas.array96[,,(i+365)]+vas.array96[,,(i+730)]+vas.array96[,,(i+1095)]+vas.array96[,,(i+1460)])
  vas.array01_average[,,i]<-(vas.array01[,,i]+vas.array01[,,(i+365)]+vas.array01[,,(i+730)]+vas.array01[,,(i+1095)]+vas.array01[,,(i+1460)])
}
for (i in 1:365)
{
  uas[,,i]<-(uas.array76_average[,,i]+uas.array81_average[,,i]+uas.array86_average[,,i]+uas.array91_average[,,i]+uas.array96_average[,,i]+uas.array01_average[,,i])/365
  vas[,,i]<-(vas.array76_average[,,i]+vas.array81_average[,,i]+vas.array86_average[,,i]+vas.array91_average[,,i]+vas.array96_average[,,i]+vas.array01_average[,,i])/365
}


saudi_uas<-matrix(NA,ncol=length(saudi_index),nrow=365)
saudi_vas<-matrix(NA,ncol=length(saudi_index),nrow=365)
for (i in 1:length(saudi_index))
{
  saudi_uas[,i]<-uas[saudi_position[i,1],saudi_position[i,2],]
  saudi_vas[,i]<-vas[saudi_position[i,1],saudi_position[i,2],]
}
group1<-rbind(t(saudi_uas[(92:302),]),t(saudi_vas[(92:302),]))
group2<-rbind(t(rbind(saudi_uas[(1:91),],saudi_uas[(303:365),])),t(rbind(saudi_vas[(1:91),],saudi_vas[(303:365),])))
image_geffect=0
image_group1=0
image_group2=0
image_mean_g=apply(cbind(group1,group2),1,mean)
image_mean_r1=apply(group1,1,mean)-image_mean_g
image_mean_r2=apply(group2,1,mean)-image_mean_g

depth_group1=sbd(group1)
depth_group2=sbd(group2)
med_group1=group1[,order(depth_group1,decreasing=T)[1]]
med_group2=group2[,order(depth_group2,decreasing=T)[1]]
image_median=(med_group1+med_group2)/2
image_group1=image_group1+med_group1-image_median
image_group2=image_group2+med_group2-image_median
image_geffect=image_geffect+image_median


