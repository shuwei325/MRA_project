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

load(file ="tas_hour.Rdata")
str(datos)
time<-as.numeric(row.names(datos))

datos.d <- datos %>% mutate(time.d=floor(time)) %>%
  group_by(time.d) %>% summarise_each( funs(mean))

names(datos.d)
dim(datos.d)
dim(datos)
attach(datos.d)

datos.ts<-ts(datos.d[,-1])

autoplot(datos.ts)
autoplot(datos.ts, facets = TRUE)

(corr<-cor(datos.ts))
#ggcorrplot(corr)
corrplot(corr, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

datos.ts<-ts(datos)
y1<-datos.ts[,"y1"]
y2<-datos.ts[,"y2"]
y3<-datos.ts[,"y3"]
y4<-datos.ts[,"y4"]
y5<-datos.ts[,"y5"]
y6<-datos.ts[,"y6"]
y7<-datos.ts[,"y7"]
y8<-datos.ts[,"y8"]
y9<-datos.ts[,"y9"]

par(mfrow=c(3,3))
acf(y1,lag.max=100,ci=0.95)
acf(y2,lag.max=100,ci=0.95)
acf(y3,lag.max=100,ci=0.95)
acf(y4,lag.max=100,ci=0.95)
acf(y5,lag.max=100,ci=0.95)
acf(y6,lag.max=100,ci=0.95)
acf(y7,lag.max=100,ci=0.95)
acf(y8,lag.max=100,ci=0.95)
acf(y9,lag.max=100,ci=0.95)

par(mfrow=c(2,3))
ts.plot(y1)
acf(y1,lag.max=500,ci=0.95)
pacf(y1,lag.max=500,ci=0.95)
ts.plot(y4)
acf(y4,lag.max=500,ci=0.95)
pacf(y4,lag.max=500,ci=0.95)

# Spectral Analysis
par(mfrow=c(1,2))
raw.spec1 <- spec.pgram(y1, taper = 0)
plot(raw.spec1, log = "no")
raw.spec4 <- spec.pgram(y4, taper = 0)
plot(raw.spec4, log = "no")

ts.plot(datos.ts, col=1:9)
spec = mvspec(datos.ts, spans=c(3,3), taper=.1)

my.w1 <- analyze.wavelet(datos.d, "y1",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)
my.w2 <- analyze.wavelet(datos.d, "y2",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)
my.w4 <- analyze.wavelet(datos.d, "y4",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)


wt.image(my.w1, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w2, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w4, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))


