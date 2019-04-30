
library(ggmap)
library(prevR)
library(maps)
library(fields)

register_google(key = "AIzaSyDggcDQ4LIF4IJsHUpD621vaHY97Uy9FcE")
myLocation =  c(33, 18, 59, 32)
Location =  c(33.5, 16, 56, 30)
myMap <- get_map(location=saudi_all, 
                 source="google", maptype = "toner-background", crop=FALSE)#, color="bw")
                                             
plot(myMap)
# #############################################################################################################
##coo_all = cbind(expand.grid(normal_lon, normal_lat)$Var1, expand.grid(normal_lon, normal_lat)$Var2)
coo_all=matrix(NA,nrow=length(normal_lat),ncol=2)
coo_all[,1]=normal_lon
coo_all[,2]=normal_lat
where <- map.where(database="world", coo_all[,1], coo_all[,2])
saudi_all = which(grepl("Saudi Arabia",where))
lonlat_saudi = coo_all[saudi_all, ]

par(mfrow=c(1,1))

p11 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_geffect[1:3534]), size=0.01) +
  ggtitle("U component: Grand Effect (Median)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(p11)
pme1 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_mean_g[1:3534]), size=0.01) +
  ggtitle("U component: Grand Effect (Mean)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(pme1)


p21 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_group1[1:3534]), size=0.01) +
  ggtitle("U component: Rainy Season Effect (Median)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(p21)

pme21 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_mean_r1[1:3534]), size=0.01) +
  ggtitle("U component: Rainy Season Effect (Mean)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(pme21)


p31 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_group2[1:3534]), size=0.01) +
  ggtitle("U component: Dry Season Effect (Median)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(p31)

pme31 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_mean_r2[1:3534]), size=0.01) +
  ggtitle("U component: Dry Season Effect (Mean)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(pme31)



p12 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_geffect[3535:7068]), size=0.01) +
  ggtitle("V component: Grand Effect (Median)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(p12)

pme12 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_group1[3535:7068]), size=0.01) +
  ggtitle("V component: Grand Effect (Mean)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(pme12)



p22 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_group1[3535:7068]), size=0.01) +
  ggtitle("V Component: Rainy Season Effect (Median)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(p22)

pme22 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=
                                 image_mean_r1[3535:7068]), size=0.01) +
  ggtitle("V Component: Rainy Season Effect (Mean)") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(pme22)

p32 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_group2[3535:7068]), size=0.01) +
  ggtitle("V Component: Dry Season Effect (Median) ") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(p32)

pme32 = ggmap(myMap) + 
  geom_point(aes(x = Longitude, y = Latitude, color=U10), 
             data = data.frame(Longitude = lonlat_saudi[,1], Latitude = lonlat_saudi[,2], U10=image_mean_r2[3535:7068]), size=0.01) +
  ggtitle("V Component: Dry Season Effect (Mean) ") +
  scale_colour_gradientn(colours = tim.colors(64))
plot(pme32)