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

str(ts_data)

datos.d<-ts_data[4:8]

names(datos.d)
dim(datos.d)

attach(datos.d)

datos.ts<-ts(datos.d)

autoplot(datos.ts)
autoplot(datos.ts, facets = TRUE)


round(corr<-cor(datos.ts),2)
corrplot(corr, method="circle")


y1<-datos.ts[,1]
y2<-datos.ts[,2]
y3<-datos.ts[,3]
y4<-datos.ts[,4]
y5<-datos.ts[,5]


par(mfrow=c(2,3))
acf(y1,lag.max=400,ci=0.95)
acf(y2,lag.max=400,ci=0.95)
acf(y3,lag.max=400,ci=0.95)
acf(y4,lag.max=400,ci=0.95)
acf(y5,lag.max=400,ci=0.95)

par(mfrow=c(1,3))
ts.plot(y1)
acf(y1,lag.max=500,ci=0.95)
pacf(y1,lag.max=500,ci=0.95)

#diff 1
dy1<-diff(y1)
ts.plot(dy1)
acf(dy1,lag.max=500,ci=0.95)
pacf(dy1,lag.max=500,ci=0.95)


# GPH estimate with big bandwidth or else estimate sucks
fdGPH(y1, bandw=.9)   # m = n^bandw

  
  
# Spectral Analysis
par(mfrow=c(1,2))
raw.spec1 <- spec.pgram(y1, taper = 0)
plot(raw.spec1, log = "no")

max(raw.spec1$spec)
(freq<-raw.spec1$freq[(raw.spec1$spec==max(raw.spec1$spec))])
(periodo<-1/freq)


ts.plot(datos.ts, col=1:9)
spec = mvspec(datos.ts, spans=c(3,3), taper=.1)


colnames(datos.d)<-c("y1","y2","y3","y4","y5")

my.w1 <- analyze.wavelet(datos.d, "y1",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 1500,make.pval = TRUE, n.sim = 10)
my.w2 <- analyze.wavelet(datos.d, "y2",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)
my.w3 <- analyze.wavelet(datos.d, "y3",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)
my.w4 <- analyze.wavelet(datos.d, "y4",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)
my.w5 <- analyze.wavelet(datos.d, "y5",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)


wt.image(my.w1, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w2, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w3, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w4, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w5, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))


