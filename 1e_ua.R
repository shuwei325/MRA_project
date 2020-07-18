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

str(ua_data)

datos.d<-ua_data[4:8]

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

par(mfrow=c(2,3))
ts.plot(y4)
acf(y4,lag.max=500,ci=0.95)
pacf(y4,lag.max=500,ci=0.95)

ts.plot(y5)
acf(y5,lag.max=500,ci=0.95)
pacf(y5,lag.max=500,ci=0.95)

#diff 1
dy4<-diff(y4)
dy5<-diff(y5)

par(mfrow=c(2,3))
ts.plot(dy4)
acf(dy4,lag.max=500,ci=0.95)
pacf(dy4,lag.max=500,ci=0.95)

ts.plot(dy5)
acf(dy5,lag.max=500,ci=0.95)
pacf(dy5,lag.max=500,ci=0.95)


adj_y4 = y4 - mean(y4)
adj_y4.fd = fracdiff(adj_y4, nar=0, nma=0, M=30)
adj_y4.fd$d  
adj_y4.fd$stderror.dpq  

adj_y5 = y5 - mean(y5)
adj_y5.fd = fracdiff(adj_y5, nar=0, nma=0, M=30)
adj_y5.fd$d  
adj_y5.fd$stderror.dpq  


res.fd = diffseries(y4, adj_y4.fd$d)       # frac diff resids            
res.arima = resid(arima(y4, order=c(1,1,1))) # arima resids

par(mfrow=c(2,2))
ts.plot(res.fd)
acf(res.fd,lag.max=500,ci=0.95)
ts.plot(dy4)
acf(dy4,lag.max=500,ci=0.95)

dev.new()
par(mfrow=c(2,1))  
acf(res.arima, 100, xlim=c(1,97), ylim=c(-.2,.2), main="arima resids")
acf(res.fd, 100, xlim=c(1,97), ylim=c(-.5,.5), main="frac diff resids")

##
# GPH estimate with big bandwidth or else estimate sucks
str(fdGPH(y4, bandw=.9))   # m = n^bandw
str(fdGPH(y5, bandw=.9))

str(fdGPH(y4, bandw=.8))   # m = n^bandw
str(fdGPH(y5, bandw=.8))

  
  
# Spectral Analysis
par(mfrow=c(1,2))
raw.spec4 <- spec.pgram(y4, taper = 0)
plot(raw.spec4, log = "no")

par(mfrow=c(1,2))
raw.spec5 <- spec.pgram(y5, taper = 0)
plot(raw.spec5, log = "no")

max(raw.spec5$spec)
(freq<-raw.spec5$freq[(raw.spec5$spec==max(raw.spec5$spec))])
(periodo<-1/freq)

ts.plot(datos.ts, col=1:9)
spec = mvspec(datos.ts, spans=c(3,3), taper=.1)


colnames(datos.d)<-c("y1","y2","y3","y4","y5")

my.w1 <- analyze.wavelet(datos.d, "y1",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)
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


