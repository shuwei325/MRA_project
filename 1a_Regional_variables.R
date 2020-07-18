library(tidyverse)
library(fracdiff)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(RColorBrewer)
library(forecast)
library(fpp2)
library(ggfortify)
library(dplR)
library(wavethresh)
library(WaveletComp)
library(astsa)

load(file ="ts_points_Regional.Rdata")


data<-left_join(hus_data,pr_data,by=c("Year","Month","Day")) %>%
  left_join(.,ts_data,by=c("Year","Month","Day")) %>%
  left_join(.,ua_data,by=c("Year","Month","Day")) %>%
  left_join(.,va_data,by=c("Year","Month","Day"))

dim(data)
sum(is.na(data))
names(data)

datos.d<-data[-c(1:3)]
names(datos.d)

dim(datos.d)
attach(datos.d)

datos.ts<-ts(datos.d)

i<-0
autoplot(datos.ts[,(5*i+1):(5*i+5)])
autoplot(datos.ts[,(5*i+1):(5*i+5)], facets = TRUE)


(corr<-round(cor(datos.ts),4))
#ggcorrplot(corr)
corrplot(corr, method="circle")

autoplot(datos.ts, facets = TRUE)
