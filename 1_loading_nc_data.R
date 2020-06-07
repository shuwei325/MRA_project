library(tidyverse)

# Obtaining xc and yc information from lat and lon ---------------------

load(file ="latlondata.Rdata")
xcyc1 <- latlondata %>% filter(lat>=29.97 & lat<=30 & lon>=240 & lon <=241)
xcyc2 <- latlondata %>% filter(lat>=29.87 & lat<=30 & lon>=260.7 & lon <=261)
xcyc3 <- latlondata %>% filter(lat>=29.87 & lat<=30 & lon>=293)

xcyc4 <- latlondata %>% filter(lat>=40 & lat<=41 & lon>=240.9 & lon <=241)
xcyc5 <- latlondata %>% filter(lat>=40 & lat<=41 & lon>=260.65 & lon <=261)
xcyc6 <- latlondata %>% filter(lat>=40 & lat<=41 & lon>=293 & lon <=293.2)

xcyc7 <- latlondata %>% filter(lat>=50 & lat<=51 & lon>=240.7 & lon <=241)
xcyc8 <- latlondata %>% filter(lat>=50 & lat<=51 & lon>=260.65 & lon <=261)
xcyc9 <- latlondata %>% filter(lat>=50 & lat<=51 & lon>=300.8 & lon <=301)


  PATH <- "D:/UCR/CIMPA/codes/data/tas/"
  PATH <- "D:/UCR/CIMPA/codes/data/pr/"
  dirbaseregional <- PATH
  listfilesregional <- list.files(path = dirbaseregional)[str_detect(list.files(path = dirbaseregional),'nc$')]
  
  c1 <- separate(tibble(listfilesregional),1, sep="_", as.character(c(1:4))) %>% select("1")
  c1 <- paste(c1[1,1])
  datos <- NULL
  
  for(i in 1:length(listfilesregional)){
    show(paste0('Construccion datos por archivo ',i))
    regional <- ncdf4::nc_open(paste0(dirbaseregional,listfilesregional[i]))  ##Leemos el archivo .nc utilizando la lista previamente creada
    varreg_pre <- ncdf4::ncvar_get(regional,c1)
    xcvar <- ncdf4::ncvar_get(regional,'xc')
    ycvar <- ncdf4::ncvar_get(regional,'yc')
    timevar <- ncdf4::ncvar_get(regional,'time')  ##Hasta acá lo que se hace es obtener las variables espaciales, temporales y la de interés del archivo de datos
    
    dimnames(varreg_pre)[[1]] <- xcvar
    dimnames(varreg_pre)[[2]] <- ycvar
    dimnames(varreg_pre)[[3]] <- as.character(timevar) ##Define nombres a los ejes del conjunto de los datos
    
    y1<-varreg_pre[(xcvar==as.numeric(xcyc1[1])),(ycvar==as.numeric(xcyc1[2])),]
    y2<-varreg_pre[(xcvar==as.numeric(xcyc2[1])),(ycvar==as.numeric(xcyc2[2])),]
    y3<-varreg_pre[(xcvar==as.numeric(xcyc3[1])),(ycvar==as.numeric(xcyc3[2])),]
    y4<-varreg_pre[(xcvar==as.numeric(xcyc4[1])),(ycvar==as.numeric(xcyc4[2])),]
    y5<-varreg_pre[(xcvar==as.numeric(xcyc5[1])),(ycvar==as.numeric(xcyc5[2])),]
    y6<-varreg_pre[(xcvar==as.numeric(xcyc6[1])),(ycvar==as.numeric(xcyc6[2])),]
    y7<-varreg_pre[(xcvar==as.numeric(xcyc7[1])),(ycvar==as.numeric(xcyc7[2])),]
    y8<-varreg_pre[(xcvar==as.numeric(xcyc8[1])),(ycvar==as.numeric(xcyc8[2])),]
    y9<-varreg_pre[(xcvar==as.numeric(xcyc9[1])),(ycvar==as.numeric(xcyc9[2])),]
  
    datos_pre<-data.frame(y1,y2,y3,y4,y5,y6,y7,y8,y9)
    datos<- rbind(datos,datos_pre)
  }
  
  save(datos,file =paste0(c1,"_hour.Rdata"))







