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

load("D:/UCR/CIMPA/Gauss server/Emuladores/datos/PSL_Global.Rdata")
y1 <- PSL_Global %>% filter(lat>=30 & lat<=31 & lon>=240 & lon <=241)
y2 <- PSL_Global %>% filter(lat>=30 & lat<=31 & lon>=260 & lon <=261)
y3 <- PSL_Global %>% filter(lat>=30 & lat<=31 & lon>=291 & lon <=292)
y4 <- PSL_Global %>% filter(lat>=41 & lat<=42 & lon>=240 & lon <=241)
y5 <- PSL_Global %>% filter(lat>=41 & lat<=42 & lon>=260 & lon <=261)
y6 <- PSL_Global %>% filter(lat>=41 & lat<=42 & lon>=291 & lon <=292)
y7 <- PSL_Global %>% filter(lat>=51 & lat<=52 & lon>=240 & lon <=241)
y8 <- PSL_Global %>% filter(lat>=51 & lat<=52 & lon>=260 & lon <=261)
y9 <- PSL_Global %>% filter(lat>=51 & lat<=52 & lon>=291 & lon <=292)

datos <- data.frame(y1[,1:2],y1$PSL,y2$PSL,y3$PSL,y4$PSL,y5$PSL,y6$PSL,y7$PSL,y8$PSL,y9$PSL)
colnames(datos)[3:11]<-c("y1","y2","y3","y4","y5","y6","y7","y8","y9")
names(datos)

datos.ts<-ts(datos[,-c(1,2)])
autoplot(datos.ts)
autoplot(datos.ts, facets = TRUE)

(corr<-cor(datos.ts))
#ggcorrplot(corr)
corrplot(corr, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

y1<-datos.ts[,"y1"]
y2<-datos.ts[,"y2"]
y3<-datos.ts[,"y3"]
y4<-datos.ts[,"y4"]
y5<-datos.ts[,"y5"]
y6<-datos.ts[,"y6"]
y7<-datos.ts[,"y7"]
y8<-datos.ts[,"y8"]
y9<-datos.ts[,"y9"]


gglagplot(y1)
gglagplot(y2)
gglagplot(y3)
gglagplot(y4)
gglagplot(y5)
gglagplot(y6)
gglagplot(y7)
gglagplot(y8)
gglagplot(y9)

# Autocorrelation:

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


autoplot(y1)
ggAcf(y1,lag.max=100,ci=0.95)
ggPacf(y1,lag.max=100,ci=0.95)

autoplot(y7)
ggAcf(y7,lag.max=100,ci=0.95)
ggPacf(y7,lag.max=100,ci=0.95)

# Spectral Analysis
par(mfrow=c(1,2))
raw.spec1 <- spec.pgram(y1, taper = 0)
plot(raw.spec1, log = "no")
raw.spec7 <- spec.pgram(y7, taper = 0)
plot(raw.spec7, log = "no")

ts.plot(datos.ts, col=1:9)
spec = mvspec(datos.ts, spans=c(3,3), taper=.1)



# Wavelets analysis

#The WaveletCompu library uses Morlet wavelets.

my.data <- data.frame(datos.ts)

my.w1 <- analyze.wavelet(my.data, "y1",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w2 <- analyze.wavelet(my.data, "y2",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w3 <- analyze.wavelet(my.data, "y3",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w4 <- analyze.wavelet(my.data, "y4",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w5 <- analyze.wavelet(my.data, "y5",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w6 <- analyze.wavelet(my.data, "y6",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w7 <- analyze.wavelet(my.data, "y7",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w8 <- analyze.wavelet(my.data, "y8",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)
my.w9 <- analyze.wavelet(my.data, "y9",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 128,make.pval = TRUE, n.sim = 10)

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
wt.image(my.w6, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w7, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w8, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w9, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))







