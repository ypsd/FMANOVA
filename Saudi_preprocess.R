library(GENEAread)
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(ggmap)
library(prevR)
library(maps)
getwd()


ncname<-"uas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19760101-19801231"
ncfname<-paste(ncname,".nc",sep="")
dname<-"uas"
ncin<-nc_open(ncfname)
print(ncin)
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)
head(lat)
print(c(nlon, nlat))
t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
t  # list the values
nt <- dim(t)  # get the number of values and list them
nt
tunits
uas.array76 <- ncvar_get(ncin, dname)
ncname2<-"uas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19810101-19851231"
ncfname2<-paste(ncname2,".nc",sep="")
dname<-"uas"
ncin2<-nc_open(ncfname2)
print(ncin2)
t2 <- ncvar_get(ncin2, "time")
tunits2 <- ncatt_get(ncin2, "time", "units")
t2  # list the values
nt2 <- dim(t2)  # get the number of values and list them
nt2
tunits2
uas.array81 <- ncvar_get(ncin2, dname)

ncname3<-"uas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19860101-19901231"
ncfname3<-paste(ncname3,".nc",sep="")
dname<-"uas"
ncin3<-nc_open(ncfname3)
print(ncin3)
t3 <- ncvar_get(ncin3, "time")
tunits3 <- ncatt_get(ncin3, "time", "units")
t3  # list the values
nt3 <- dim(t3)  # get the number of values and list them
nt3
tunits3
uas.array86 <- ncvar_get(ncin3, dname)


ncname4<-"uas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19910101-19951231"
ncfname4<-paste(ncname4,".nc",sep="")
dname<-"uas"
ncin4<-nc_open(ncfname4)
print(ncin4)
t4 <- ncvar_get(ncin4, "time")
tunits4 <- ncatt_get(ncin4, "time", "units")
t4  # list the values
nt4 <- dim(t4)  # get the number of values and list them
nt4
tunits4
uas.array91 <- ncvar_get(ncin4, dname)
ncname5<-"uas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19960101-20001231"
ncfname5<-paste(ncname5,".nc",sep="")
dname<-"uas"
ncin5<-nc_open(ncfname5)
print(ncin5)
t5 <- ncvar_get(ncin5, "time")
tunits5 <- ncatt_get(ncin5, "time", "units")
t5  # list the values
nt5 <- dim(t5)  # get the number of values and list them
nt5
tunits5
uas.array96 <- ncvar_get(ncin5, dname)
ncname6<-"uas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_20010101-20051231"
ncfname6<-paste(ncname6,".nc",sep="")
dname<-"uas"
ncin6<-nc_open(ncfname6)
print(ncin6)
t6 <- ncvar_get(ncin6, "time")
tunits6 <- ncatt_get(ncin6, "time", "units")
t6  # list the values
nt6 <- dim(t6)  # get the number of values and list them
nt6
tunits6
uas.array01 <- ncvar_get(ncin6, dname)

Ncname<-"vas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19760101-19801231"
Ncfname<-paste(Ncname,".nc",sep="")
Dname<-"vas"
Ncin<-nc_open(Ncfname)
print(Ncin)
T <- ncvar_get(Ncin, "time")
Tunits <- ncatt_get(Ncin, "time", "units")
T  # list the values
Nt <- dim(T)  # get the number of values and list them
Nt
Tunits
vas.array76 <- ncvar_get(Ncin, Dname)

Ncname2<-"vas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19810101-19851231"
Ncfname2<-paste(Ncname2,".nc",sep="")
Dname<-"vas"
Ncin2<-nc_open(Ncfname2)
print(Ncin2)
T2 <- ncvar_get(Ncin2, "time")
Tunits2 <- ncatt_get(Ncin2, "time", "units")
T  # list the values
Nt2 <- dim(T2)  # get the number of values and list them
Nt2
Tunits2
vas.array81 <- ncvar_get(Ncin2, Dname)

Ncname3<-"vas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19860101-19901231"
Ncfname3<-paste(Ncname3,".nc",sep="")
Dname<-"vas"
Ncin3<-nc_open(Ncfname3)
print(Ncin3)
T3 <- ncvar_get(Ncin3, "time")
Tunits3 <- ncatt_get(Ncin3, "time", "units")
T  # list the values
Nt3 <- dim(T3)  # get the number of values and list them
Nt3
Tunits3
vas.array86 <- ncvar_get(Ncin3, Dname)

Ncname4<-"vas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19910101-19951231"
Ncfname4<-paste(Ncname4,".nc",sep="")
Dname<-"vas"
Ncin4<-nc_open(Ncfname4)
print(Ncin4)
T4 <- ncvar_get(Ncin4, "time")
Tunits4 <- ncatt_get(Ncin4, "time", "units")
T  # list the values
Nt4 <- dim(T4)  # get the number of values and list them
Nt4
Tunits4
vas.array91 <- ncvar_get(Ncin4, Dname)

Ncname5<-"vas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19960101-20001231"
Ncfname5<-paste(Ncname5,".nc",sep="")
Dname<-"vas"
Ncin5<-nc_open(Ncfname5)
print(Ncin5)
T5 <- ncvar_get(Ncin5, "time")
Tunits5 <- ncatt_get(Ncin5, "time", "units")
T  # list the values
Nt5 <- dim(T5)  # get the number of values and list them
Nt5
Tunits5
vas.array96 <- ncvar_get(Ncin5, Dname)

Ncname6<-"vas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_20010101-20051231"
Ncfname6<-paste(Ncname6,".nc",sep="")
Dname<-"vas"
Ncin6<-nc_open(Ncfname6)
print(Ncin6)
T6 <- ncvar_get(Ncin6, "time")
Tunits6 <- ncatt_get(Ncin6, "time", "units")
T  # list the values
Nt6 <- dim(T6)  # get the number of values and list them
Nt6
Tunits6
vas.array01 <- ncvar_get(Ncin6, Dname)
normal_coordinate<-read.csv('normal_coordinate.csv', header = FALSE)
normal_lon<-normal_coordinate$V1
normal_lat<-normal_coordinate$V2

register_google(key = "AIzaSyDggcDQ4LIF4IJsHUpD621vaHY97Uy9FcE")
myLocation =  c(33, 18, 59, 30)
myMap <- get_map(location=myLocation, 
                 source="google", maptype="satellite", crop=FALSE)#, color="bw")
plot(myMap)
# #############################################################################################################
coo_all = cbind(expand.grid(normal_lon, normal_lat)$Var1, expand.grid(normal_lon, normal_lat)$Var2)
where <- map.where(database="world", coo_all[,1], coo_all[,2])
saudi_all = which(where == "Saudi Arabia")
lonlat_saudi = coo_all[saudi_all, ]

p11 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=data_t1$u10_saudi), size=0.01) +
  #ggtitle("Observed") +
  scale_colour_gradientn(colours = tim.colors(64), limit=zlm_u10)







