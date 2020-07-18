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

str(hus_data)

datos.d<-hus_data[4:8]

names(datos.d)
dim(datos.d)
dim(datos)
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
ts.plot(y3)
acf(y3,lag.max=500,ci=0.95)
pacf(y3,lag.max=500,ci=0.95)

ts.plot(y5)
acf(y5,lag.max=500,ci=0.95)
pacf(y5,lag.max=500,ci=0.95)

#diff 1
dy3<-diff(y3)
dy5<-diff(y5)

ts.plot(dy3)
acf(dy3,lag.max=500,ci=0.95)
pacf(dy3,lag.max=500,ci=0.95)

ts.plot(dy5)
acf(dy5,lag.max=500,ci=0.95)
pacf(dy5,lag.max=500,ci=0.95)

library(fracdiff)

adj_y3 = y3 - mean(y3)
adj_y3.fd = fracdiff(adj_y3, nar=0, nma=0, M=30)
#adj_y1.fd = fracdiff(adj_y1, nar=0, nma=0, M=100)
adj_y3.fd$d  # = 0.3841688
adj_y3.fd$stderror.dpq  

adj_y5 = y5 - mean(y5)
adj_y5.fd = fracdiff(adj_y5, nar=0, nma=0, M=30)
#adj_y1.fd = fracdiff(adj_y1, nar=0, nma=0, M=100)
adj_y5.fd$d  # = 0.3841688
adj_y5.fd$stderror.dpq  

res.fd = diffseries(y3, adj_y3.fd$d)       # frac diff resids            
res.arima = resid(arima(y5, order=c(1,1,1))) # arima resids

par(mfrow=c(2,2))
ts.plot(res.fd)
acf(res.fd,lag.max=500,ci=0.95)
ts.plot(dy3)
acf(dy3,lag.max=500,ci=0.95)

dev.new()
par(mfrow=c(2,1))  
acf(res.arima, 100, xlim=c(1,97), ylim=c(-.2,.2), main="arima resids")
acf(res.fd, 100, xlim=c(1,97), ylim=c(-.2,.2), main="frac diff resids")

##
library(arfima)
summary(y3.fd <- arfima(y3))  
summary(y3.fd)$coef

# residual stuff
innov = resid(y3.fd)  
plot.ts(innov[[1]])  
acf(innov[[1]])  

# GPH estimate with big bandwidth or else estimate sucks
fdGPH(y3, bandw=.9)   # m = n^bandw
fdGPH(y5, bandw=.9)   # m = n^bandw


# Spectral Analysis
par(mfrow=c(1,2))
raw.spec3 <- spec.pgram(y3, taper = 0)
plot(raw.spec3, log = "no")

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
