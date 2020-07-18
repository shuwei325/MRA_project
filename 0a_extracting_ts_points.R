getwd()
setwd("../Emuladores/datos")
getwd()

rm(list=ls())
library(fields)
library(spaprtat)
library(maps)
library(mapdata)

load("resolucion/domainfinal.Rdata")
load("resolucion/coordinates.Rdata")
load("resolucion/altitude.Rdata")

load("pr_RegionalDiarioMonson.Rdata")


locations <- unique(cbind(pr_Regional$lat,pr_Regional$lon))

plot(locations[,2]-360,locations[,1],
     ylim=c(23,43),xlim=c(-123,-92),lwd=0.1,
     xlab="Easting", ylab="Northing",
     main="")
map("worldHires",xlim=c(-130,-85), ylim=c(20,50), 
    col=1, add=TRUE)
rect(-120,25,-95,40,border="red",lwd=3.5)

## puntos

nlon <- length(unique(pr_Regional$lon))
nlat <- length(unique(pr_Regional$lat))
p50_varlon <- sort(unique(pr_Regional$lon))[(nlon+1)/2]
p50_varlat <- sort(unique(pr_Regional$lat))[(nlat+1)/2]
p25_varlon <- sort(unique(pr_Regional$lon))[(nlon+3)/4]
p25_varlat <- sort(unique(pr_Regional$lat))[(nlat+1)/4]
p75_varlon <- sort(unique(pr_Regional$lon))[(nlon+3)/4*3]
p75_varlat <- sort(unique(pr_Regional$lat))[(nlat+1)/4*3]

points(p50_varlon-360, p50_varlat, col="red")
points(p25_varlon-360, p25_varlat, col="red")
points(p75_varlon-360, p75_varlat, col="red")
points(p25_varlon-360, p75_varlat, col="red")
points(p75_varlon-360, p25_varlat, col="red")

points(p50_varlon-360, p50_varlat, col="red",pch="1")
points(p25_varlon-360, p25_varlat, col="red",pch="2")
points(p75_varlon-360, p75_varlat, col="red",pch="3")
points(p25_varlon-360, p75_varlat, col="red",pch="4")
points(p75_varlon-360, p25_varlat, col="red",pch="5")




## para extraerlos:

pr_Regional %>% 
  filter(lon==mean_varlon)



## para extraerlos:
library(dplyr)
p1<-pr_Regional %>% 
    filter(lon==p50_varlon) %>% mutate(id_location=1)  
  p50_varlon;p50_varlat
  table(p1$lon,p1$lat)
p2<-pr_Regional %>% 
    filter(lon==p25_varlon)  %>% mutate(id_location=2)    
  p25_varlon;p25_varlat
  table(p2$lon,p2$lat)
p3<-pr_Regional %>% 
    filter(lon==p75_varlon)  %>% mutate(id_location=3)
  p75_varlon;p75_varlat
  table(p3$lon,p3$lat)
p4<-pr_Regional %>% 
    filter(lat==p75_varlat) %>% mutate(id_location=4)
  p25_varlon;p75_varlat
  table(p4$lon,p4$lat)
p5<-pr_Regional %>% 
    filter(lat==p25_varlat) %>% mutate(id_location=5)
  p75_varlon;p25_varlat
  table(p5$lon,p5$lat)

pr_points_ts<-bind_rows(p1,p2,p3,p4,p5)
dim(pr_points_ts)

getwd()
setwd("../../schou")
save(pr_points_ts, file="pr_points_ts.Rdata")
