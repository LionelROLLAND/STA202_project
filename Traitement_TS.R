rm(list=ls())
library(zoo)
library(xts)
library(lubridate)
setwd("~/.STA202_BDD/STA202_project/BDD/")

##Chargement des données
acc_grav_pday=readRDS(file="acc_grav_pday.rds")
acc_lege_pday=readRDS(file="acc_lege_pday.rds")
days=readRDS(file="days.rds")

acc_grav_pday2=readRDS(file="acc_grav_pday2.rds")
acc_lege_pday2=readRDS(file="acc_lege_pday2.rds")
days2=readRDS(file="days2.rds")
##.

##dates
date1=strptime("01/01/2005", "%d/%m/%Y")
date2=strptime("31/12/2017", "%d/%m/%Y")
date3=strptime("31/12/2020", "%d/%m/%Y")
Date=seq.POSIXt(date1,date3, by = "day")
Date0517=seq.POSIXt(date1,date2, by = "day")
Date1820=seq.POSIXt(date2,date3, by = "day")
##.

##TS
acc_grav_ts=xts(acc_grav_pday,order.by=Date)
acc_grav_ts_0517=acc_grav_ts[Date0517,]
acc_grav_ts_1820=acc_grav_ts[Date1820,]
##.



##acc_grav

##Tendance

##Moyenne mobile
MA_grav_0517=filter(acc_grav_ts_0517,filter=array(1/10,dim=10),
                    method = c("convolution"),
                    sides = 2, circular = FALSE)
MA_grav_0517=xts(MA_grav_0517,order.by=)

plot(acc_grav_ts_0517,type='l',col='black')
lines(MA_grav_0517,type='l',col='blue')

##Moyenne mobile.
##Tendance.
##acc_grav.


