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

str(va_data)

datos.d<-va_data[4:8]

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
ts.plot(y1)
acf(y1,lag.max=500,ci=0.95)
pacf(y1,lag.max=500,ci=0.95)

ts.plot(y4)
acf(y4,lag.max=500,ci=0.95)
pacf(y4,lag.max=500,ci=0.95)


#diff 1
dy1<-diff(y1)
dy4<-diff(y4)

par(mfrow=c(2,3))
ts.plot(dy1)
acf(dy1,lag.max=500,ci=0.95)
pacf(dy1,lag.max=500,ci=0.95)

ts.plot(dy4)
acf(dy4,lag.max=500,ci=0.95)
pacf(dy4,lag.max=500,ci=0.95)

adj_y1 = y1 - mean(y1)
adj_y1.fd = fracdiff(adj_y1, nar=0, nma=0, M=30)
adj_y1.fd$d  
adj_y1.fd$stderror.dpq  


adj_y4 = y4 - mean(y4)
adj_y4.fd = fracdiff(adj_y4, nar=0, nma=0, M=30)
adj_y4.fd$d  
adj_y4.fd$stderror.dpq  



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
str(fdGPH(y1, bandw=.8))   # m = n^bandw
str(fdGPH(y1, bandw=.8))

str(fdGPH(y4, bandw=.8))   # m = n^bandw
str(fdGPH(y4, bandw=.9))


  
  
# Spectral Analysis
par(mfrow=c(1,2))
raw.spec1 <- spec.pgram(y1, taper = 0)
plot(raw.spec1, log = "no")

max(raw.spec1$spec)
(freq<-raw.spec1$freq[(raw.spec1$spec==max(raw.spec1$spec))])
(periodo<-1/freq)


par(mfrow=c(1,2))
raw.spec4 <- spec.pgram(y4, taper = 0)
plot(raw.spec4, log = "no")

max(raw.spec4$spec)
(freq<-raw.spec4$freq[(raw.spec4$spec==max(raw.spec4$spec))])
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


