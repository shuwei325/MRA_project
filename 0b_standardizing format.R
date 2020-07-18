library(tidyverse)
load(file ="hus_points_ts.Rdata")
str(hus_points_ts)

hus1 <- hus_points_ts %>% filter(id_location==1) %>% 
  dplyr::select(c("Year", "Month", "Day", "hus")) %>% arrange(Year,Month,Day) 
hus2 <- hus_points_ts %>% filter(id_location==2) %>% 
  dplyr::select(c("Year", "Month", "Day", "hus")) %>% arrange(Year,Month,Day) 
hus3 <- hus_points_ts %>% filter(id_location==3) %>% 
  dplyr::select(c("Year", "Month", "Day", "hus")) %>% arrange(Year,Month,Day) 
hus4 <- hus_points_ts %>% filter(id_location==4) %>% 
  dplyr::select(c("Year", "Month", "Day", "hus")) %>% arrange(Year,Month,Day) 
hus5 <- hus_points_ts %>% filter(id_location==5) %>% 
  dplyr::select(c("Year", "Month", "Day", "hus")) %>% arrange(Year,Month,Day) 

hus_data<-left_join(hus1,hus2,by=c("Year","Month","Day")) %>%
  left_join(.,hus3,by=c("Year","Month","Day")) %>%
  left_join(.,hus4,by=c("Year","Month","Day")) %>%
  left_join(.,hus5,by=c("Year","Month","Day"))

colnames(hus_data)[4:8]<-c("hus.1","hus.2","hus.3","hus.4","hus.5")



# pr ----------------------------------------------------------------------

load(file ="pr_points_ts.Rdata")
str(pr_points_ts)

pr1 <- pr_points_ts %>% filter(id_location==1) %>% 
  dplyr::select(c("Year", "Month", "Day", "pr")) %>% arrange(Year,Month,Day) 
pr2 <- pr_points_ts %>% filter(id_location==2) %>% 
  dplyr::select(c("Year", "Month", "Day", "pr")) %>% arrange(Year,Month,Day) 
pr3 <- pr_points_ts %>% filter(id_location==3) %>% 
  dplyr::select(c("Year", "Month", "Day", "pr")) %>% arrange(Year,Month,Day) 
pr4 <- pr_points_ts %>% filter(id_location==4) %>% 
  dplyr::select(c("Year", "Month", "Day", "pr")) %>% arrange(Year,Month,Day) 
pr5 <- pr_points_ts %>% filter(id_location==5) %>% 
  dplyr::select(c("Year", "Month", "Day", "pr")) %>% arrange(Year,Month,Day) 

pr_data<-left_join(pr1,pr2,by=c("Year","Month","Day")) %>%
  left_join(.,pr3,by=c("Year","Month","Day")) %>%
  left_join(.,pr4,by=c("Year","Month","Day")) %>%
  left_join(.,pr5,by=c("Year","Month","Day"))

colnames(pr_data)[4:8]<-c("pr.1","pr.2","pr.3","pr.4","pr.5")

pr_data


# va ----------------------------------------------------------------------

load(file ="va_points_ts.Rdata")
str(va_points_ts)

va1 <- va_points_ts %>% filter(id_location==1) %>% 
  dplyr::select(c("Year", "Month", "Day", "va")) %>% arrange(Year,Month,Day) 
va2 <- va_points_ts %>% filter(id_location==2) %>% 
  dplyr::select(c("Year", "Month", "Day", "va")) %>% arrange(Year,Month,Day) 
va3 <- va_points_ts %>% filter(id_location==3) %>% 
  dplyr::select(c("Year", "Month", "Day", "va")) %>% arrange(Year,Month,Day) 
va4 <- va_points_ts %>% filter(id_location==4) %>% 
  dplyr::select(c("Year", "Month", "Day", "va")) %>% arrange(Year,Month,Day) 
va5 <- va_points_ts %>% filter(id_location==5) %>% 
  dplyr::select(c("Year", "Month", "Day", "va")) %>% arrange(Year,Month,Day) 

va_data<-left_join(va1,va2,by=c("Year","Month","Day")) %>%
  left_join(.,va3,by=c("Year","Month","Day")) %>%
  left_join(.,va4,by=c("Year","Month","Day")) %>%
  left_join(.,va5,by=c("Year","Month","Day"))

colnames(va_data)[4:8]<-c("va.1","va.2","va.3","va.4","va.5")

va_data



# ua ----------------------------------------------------------------------

load(file ="ua_points_ts.Rdata")
str(ua_points_ts)

ua1 <- ua_points_ts %>% filter(id_location==1) %>% 
  dplyr::select(c("Year", "Month", "Day", "ua")) %>% arrange(Year,Month,Day) 
ua2 <- ua_points_ts %>% filter(id_location==2) %>% 
  dplyr::select(c("Year", "Month", "Day", "ua")) %>% arrange(Year,Month,Day) 
ua3 <- ua_points_ts %>% filter(id_location==3) %>% 
  dplyr::select(c("Year", "Month", "Day", "ua")) %>% arrange(Year,Month,Day) 
ua4 <- ua_points_ts %>% filter(id_location==4) %>% 
  dplyr::select(c("Year", "Month", "Day", "ua")) %>% arrange(Year,Month,Day) 
ua5 <- ua_points_ts %>% filter(id_location==5) %>% 
  dplyr::select(c("Year", "Month", "Day", "ua")) %>% arrange(Year,Month,Day) 

ua_data<-left_join(ua1,ua2,by=c("Year","Month","Day")) %>%
  left_join(.,ua3,by=c("Year","Month","Day")) %>%
  left_join(.,ua4,by=c("Year","Month","Day")) %>%
  left_join(.,ua5,by=c("Year","Month","Day"))

colnames(ua_data)[4:8]<-c("ua.1","ua.2","ua.3","ua.4","ua.5")



# ts ----------------------------------------------------------------------

load(file ="ts_points_ts.Rdata")
str(ts_points_ts)

ts1 <- ts_points_ts %>% filter(id_location==1) %>% 
  dplyr::select(c("Year", "Month", "Day", "ts")) %>% arrange(Year,Month,Day) 
ts2 <- ts_points_ts %>% filter(id_location==2) %>% 
  dplyr::select(c("Year", "Month", "Day", "ts")) %>% arrange(Year,Month,Day) 
ts3 <- ts_points_ts %>% filter(id_location==3) %>% 
  dplyr::select(c("Year", "Month", "Day", "ts")) %>% arrange(Year,Month,Day) 
ts4 <- ts_points_ts %>% filter(id_location==4) %>% 
  dplyr::select(c("Year", "Month", "Day", "ts")) %>% arrange(Year,Month,Day) 
ts5 <- ts_points_ts %>% filter(id_location==5) %>% 
  dplyr::select(c("Year", "Month", "Day", "ts")) %>% arrange(Year,Month,Day) 

ts_data<-left_join(ts1,ts2,by=c("Year","Month","Day")) %>%
  left_join(.,ts3,by=c("Year","Month","Day")) %>%
  left_join(.,ts4,by=c("Year","Month","Day")) %>%
  left_join(.,ts5,by=c("Year","Month","Day"))

colnames(ts_data)[4:8]<-c("ts.1","ts.2","ts.3","ts.4","ts.5")

ts_data<-ts_data[1:1430,]

save(hus_data,va_data,ua_data,pr_data,ts_data, file="ts_points_Regional.Rdata")

load("ts_points_Regional.Rdata")
