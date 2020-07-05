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


autoplot(datos.ts)
autoplot(datos.ts, facets = TRUE)

(corr<-cor(datos.ts))

#ggcorrplot(corr)
corrplot(corr, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

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


# location 1 --------------------------------------------------------------

autoplot(y1)
ggAcf(y1,lag.max=100,ci=0.95)
ggPacf(y1,lag.max=100,ci=0.95)


# location 2 --------------------------------------------------------------

autoplot(y2)
ggAcf(y2,lag.max=100,ci=0.95)
ggPacf(y2,lag.max=100,ci=0.95)

# Wavelet analysis --------------------------------------------------------

length(y1)
8*365*2
8*365*2*2
y1a<-y1[1:5840]
y1b<-y1[5841:11680]
y2a<-y2[1:5840]
y2b<-y2[5841:11680]

my.data<-data.frame(y1a,y1b,y2a,y2b)
my.w1a <- analyze.wavelet(my.data, "y1a",loess.span = 0,
                          dt = 1, dj = 1/250,lowerPeriod = 1,
                          upperPeriod = 4000,make.pval = TRUE, n.sim = 10)
my.w1b <- analyze.wavelet(my.data, "y1b",loess.span = 0,
                          dt = 1, dj = 1/250,lowerPeriod = 1,
                          upperPeriod = 4000,make.pval = TRUE, n.sim = 10)
my.w2a <- analyze.wavelet(my.data, "y2a",loess.span = 0,
                          dt = 1, dj = 1/250,lowerPeriod = 1,
                          upperPeriod = 4000,make.pval = TRUE, n.sim = 10)
my.w2b <- analyze.wavelet(my.data, "y2b",loess.span = 0,
                          dt = 1, dj = 1/250,lowerPeriod = 1,
                          upperPeriod = 4000,make.pval = TRUE, n.sim = 10)
par(mfrow=c(1,1))
wt.image(my.w1a, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w1b, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w2a, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w2b, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))


# By day ------------------------------------------------------------------

library(tidyverse)
names(datos)

datos.d <- datos %>% mutate(time.d=floor(time)) %>%
  group_by(time.d) %>% summarise_each( funs(mean))

names(datos.d)
dim(datos.d)
dim(datos)
attach(datos.d)


my.w1 <- analyze.wavelet(datos.d, "y1",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)
my.w2 <- analyze.wavelet(datos.d, "y2",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 500,make.pval = TRUE, n.sim = 10)

wt.image(my.w1, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w2, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))

