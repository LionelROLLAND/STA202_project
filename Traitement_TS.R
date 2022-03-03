rm(list=ls())
library(zoo)
library(xts)
library(lubridate)
library(mgcv)
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
Date_0517=seq.POSIXt(date1,date2, by = "day")
Date_1820=seq.POSIXt(date2,date3, by = "day")
##.

##TS
acc_grav_ts=xts(acc_grav_pday,order.by=Date)
acc_grav_ts_0517=acc_grav_ts[Date_0517,]
acc_grav_ts_1820=acc_grav_ts[Date_1820,]
##.



##acc_grav

##0517
##Tendance

##Moyenne mobile
window_len=30
MA_grav_0517=filter(acc_grav_ts_0517,filter=array(1/window_len,dim=window_len),
                    method = c("convolution"),
                    sides = 2, circular = FALSE)
MA_grav_0517=xts(MA_grav_0517,order.by=Date_0517)

plot(acc_grav_ts_0517,type='l',col='black')
lines(MA_grav_0517,type='l',col='blue')

##Moyenne mobile.

##REG
t_0517=1:(length((Date_0517)))
reg_lin_0517=lm(acc_grav_ts_0517~t_0517)
reg_quad_0517=lm(acc_grav_ts_0517~t_0517 + I(t_0517^2))

plot(acc_grav_ts_0517,type='l',col='black')
lines(reg_lin_0517$fitted.values,type='l',col='blue')
lines(reg_quad_0517$fitted.values,type='l',col='purple')
##REG.
##Base de splines
spl_0517=gam(acc_grav_ts_0517~s(t_0517,k=2))
tend_spl_0517=xts(spl_0517$fitted,order.by=Date_0517)
plot(acc_grav_ts_0517,type='l',col='black')
lines(tend_spl_0517,type='l',col='blue')
##Base de splines.
##Tendance.
##0517.
##acc_grav.


