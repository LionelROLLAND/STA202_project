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
acc_grav_ts_0517=xts(acc_grav_ts[Date_0517,],order.by=Date_0517)
acc_grav_ts_1820=xts(acc_grav_ts[Date_1820,],order.by=Date_1820)

acc_lege_ts=xts(acc_lege_pday,order.by=Date)
acc_lege_ts_0517=xts(acc_lege_ts[Date_0517,],order.by=Date_0517)
acc_lege_ts_1820=xts(acc_lege_ts[Date_1820,],order.by=Date_1820)

acc_tot_ts_1820 = acc_grav_ts_1820 + acc_lege_ts_1820
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
grav_reg_lin_0517=lm(acc_grav_ts_0517~t_0517)
grav_reg_quad_0517=lm(acc_grav_ts_0517~t_0517 + I(t_0517^2))

plot(acc_grav_ts_0517,type='l',col='black')
lines(grav_reg_lin_0517$fitted.values,type='l',col='blue')
lines(grav_reg_quad_0517$fitted.values,type='l',col='purple')

print("Moyennes grav sans tendances (regressions lin et quad)")
mean(acc_grav_ts_0517-grav_reg_lin_0517$fitted.values)
mean(acc_grav_ts_0517-grav_reg_quad_0517$fitted.values)
##REG.
##Base de splines
spl_0517=gam(acc_grav_ts_0517~s(t_0517,k=2))
tend_spl_0517=xts(spl_0517$fitted,order.by=Date_0517)
plot(acc_grav_ts_0517,type='l',col='black')
lines(tend_spl_0517,type='l',col='blue')
##Base de splines.
##Tendance.

##Saisonnalité
#untend_ts_0517=acc_grav_ts-grav_reg_lin_0517$fitted.values
untend_ts_0517=acc_grav_ts-grav_reg_quad_0517$fitted.values ##pacf meilleur à lafin

##Moyenne mobile
sais_week_0517=filter(untend_ts_0517,filter=array(1/7,dim=7),
                     method = c("convolution"),
                     sides = 2, circular = TRUE)
sais_week_0517=xts(sais_week_0517,order.by=Date_0517)


sais_mon_0517=filter(sais_week_0517,filter=array(1/30,dim=30),
                    method = c("convolution"),
                    sides = 2, circular = TRUE)
sais_mon_0517=xts(sais_mon_0517,order.by=Date_0517)

#sais_ann_0517=filter(sais_mon_0517,filter=array(1/365,dim=365), #à décommenter pour enlever les mois !
sais_ann_0517=filter(sais_week_0517,filter=array(1/365,dim=365),
                     method= c("convolution"),
                     sides =2, circular=TRUE)

sais_ann_0517=xts(sais_ann_0517,order.by=Date_0517)


plot(untend_ts_0517,type="l",col="black")
lines(sais_week_0517,type="l",col="purple")
lines(sais_mon_0517,type="l",col="blue")
lines(sais_ann_0517,type="l",col="red")


pacf(sais_ann_0517)
##Moyenne mobile.

##Saisonnalité.

##acc_grav.


##acc_lege

##Tendance

##REG
t_0517=1:(length((Date_0517)))
lege_reg_lin_0517=lm(acc_lege_ts_0517~t_0517)
lege_reg_quad_0517=lm(acc_lege_ts_0517~t_0517 + I(t_0517^2))

plot(acc_lege_ts_0517,type='l',col='black')
lines(lege_reg_lin_0517$fitted.values,type='l',col='blue')
lines(lege_reg_quad_0517$fitted.values,type='l',col='purple')

print("Moyennes lege sans tendances (regressions lin et quad)")
mean(acc_lege_ts_0517-lege_reg_lin_0517$fitted.values)
mean(acc_lege_ts_0517-lege_reg_quad_0517$fitted.values)
##REG.

##Tendance.


##Saisonnalité
#untend_ts_0517=acc_lege_ts-lege_reg_lin_0517$fitted.values
untend_ts_0517=acc_lege_ts-lege_reg_quad_0517$fitted.values 

##Moyenne mobile
sais_week_0517=filter(untend_ts_0517,filter=array(1/7,dim=7),
                      method = c("convolution"),
                      sides = 2, circular = TRUE)
sais_week_0517=xts(sais_week_0517,order.by=Date_0517)


sais_mon_0517=filter(sais_week_0517,filter=array(1/30,dim=30),
                     method = c("convolution"),
                     sides = 2, circular = TRUE)
sais_mon_0517=xts(sais_mon_0517,order.by=Date_0517)

sais_ann_0517=filter(sais_mon_0517,filter=array(1/365,dim=365),
                     method= c("convolution"),
                     sides =2, circular=TRUE)

sais_ann_0517=xts(sais_ann_0517,order.by=Date_0517)


plot(untend_ts_0517,type="l",col="black")
lines(sais_week_0517,type="l",col="purple")
lines(sais_mon_0517,type="l",col="blue")
lines(sais_ann_0517,type="l",col="red")


pacf(sais_ann_0517)
##Moyenne mobile.

##Saisonnalité.

##acc_lege.

##0517.

##acc_grav

##1820
##Tendance

##Moyenne mobile
window_len=30
MA_grav_1820=filter(acc_grav_ts_1820,filter=array(1/window_len,dim=window_len),
                    method = c("convolution"),
                    sides = 2, circular = FALSE)
MA_grav_1820=xts(MA_grav_1820,order.by=Date_1820)

plot(acc_grav_ts_1820,type='l',col='black')
lines(MA_grav_1820,type='l',col='blue')

##Moyenne mobile.

##REG
t_1820=1:(length((Date_1820)))
grav_reg_lin_1820=lm(acc_grav_ts_1820~t_1820)
grav_reg_quad_1820=lm(acc_grav_ts_1820~t_1820 + I(t_1820^2))

plot(acc_grav_ts_1820,type='l',col='black')
lines(grav_reg_lin_1820$fitted.values,type='l',col='blue')
lines(grav_reg_quad_1820$fitted.values,type='l',col='purple')

print("Moyennes grav sans tendances (regressions lin et quad)")
mean(acc_grav_ts_1820-grav_reg_lin_1820$fitted.values)
mean(acc_grav_ts_1820-grav_reg_quad_1820$fitted.values)
##REG.
##Base de splines
spl_1820=gam(acc_grav_ts_1820~s(t_1820,k=2))
tend_spl_1820=xts(spl_1820$fitted,order.by=Date_1820)
plot(acc_grav_ts_1820,type='l',col='black')
lines(tend_spl_1820,type='l',col='blue')
##Base de splines.
##Tendance.

##Saisonnalité
#untend_ts_1820=acc_grav_ts-grav_reg_lin_1820$fitted.values
untend_ts_1820=acc_grav_ts-grav_reg_quad_1820$fitted.values ##pacf meilleur à lafin

##Moyenne mobile
sais_week_1820=filter(untend_ts_1820,filter=array(1/7,dim=7),
                      method = c("convolution"),
                      sides = 2, circular = TRUE)
sais_week_1820=xts(sais_week_1820,order.by=Date_1820)


sais_mon_1820=filter(sais_week_1820,filter=array(1/30,dim=30),
                     method = c("convolution"),
                     sides = 2, circular = TRUE)
sais_mon_1820=xts(sais_mon_1820,order.by=Date_1820)

#sais_ann_1820=filter(sais_mon_1820,filter=array(1/365,dim=365), #à décommenter pour enlever les mois !
sais_ann_1820=filter(sais_week_1820,filter=array(1/365,dim=365),
                     method= c("convolution"),
                     sides =2, circular=TRUE)

sais_ann_1820=xts(sais_ann_1820,order.by=Date_1820)


plot(untend_ts_1820,type="l",col="black")
lines(sais_week_1820,type="l",col="purple")
lines(sais_mon_1820,type="l",col="blue")
lines(sais_ann_1820,type="l",col="red")


pacf(sais_ann_1820)
##Moyenne mobile.

##Saisonnalité.

##acc_grav.


##acc_lege

##Tendance

##REG
t_1820=1:(length((Date_1820)))
lege_reg_lin_1820=lm(acc_lege_ts_1820~t_1820)
lege_reg_quad_1820=lm(acc_lege_ts_1820~t_1820 + I(t_1820^2))

plot(acc_lege_ts_1820,type='l',col='black')
lines(lege_reg_lin_1820$fitted.values,type='l',col='blue')
lines(lege_reg_quad_1820$fitted.values,type='l',col='purple')

print("Moyennes lege sans tendances (regressions lin et quad)")
mean(acc_lege_ts_1820-lege_reg_lin_1820$fitted.values)
mean(acc_lege_ts_1820-lege_reg_quad_1820$fitted.values)
##REG.

##Tendance.


##Saisonnalité
#untend_ts_1820=acc_lege_ts-lege_reg_lin_1820$fitted.values
untend_ts_1820=acc_lege_ts-lege_reg_quad_1820$fitted.values 

##Moyenne mobile
sais_week_1820=filter(untend_ts_1820,filter=array(1/7,dim=7),
                      method = c("convolution"),
                      sides = 2, circular = TRUE)
sais_week_1820=xts(sais_week_1820,order.by=Date_1820)


sais_mon_1820=filter(sais_week_1820,filter=array(1/30,dim=30),
                     method = c("convolution"),
                     sides = 2, circular = TRUE)
sais_mon_1820=xts(sais_mon_1820,order.by=Date_1820)

sais_ann_1820=filter(sais_mon_1820,filter=array(1/365,dim=365),
                     method= c("convolution"),
                     sides =2, circular=TRUE)

sais_ann_1820=xts(sais_ann_1820,order.by=Date_1820)


plot(untend_ts_1820,type="l",col="black")
lines(sais_week_1820,type="l",col="purple")
lines(sais_mon_1820,type="l",col="blue")
lines(sais_ann_1820,type="l",col="red")


pacf(sais_ann_1820)
##Moyenne mobile.

##Saisonnalité.

##acc_lege.

##1820 : graves + legers


plot(Date_1820, acc_tot_ts_1820, type="l", cex.axis=0.5,col="darkgreen")

##Desaisonnalisation par differenciation :


acc_1820_des = diff(acc_tot_ts_1820, lag = 1, differences = 1)
plot(Date_1820, acc_1820_des, type="l", cex.axis=0.5,col="blue")

pacf(as.vector(acc_1820_des), na.action=na.pass)
acc_1820_des = diff(acc_1820_des, lag = 6, differences = 1)
plot(Date_1820, acc_1820_des, type="l", cex.axis=0.5,col="blue")

pacf(as.vector(acc_1820_des), na.action=na.pass)
acc_1820_des = diff(acc_1820_des, lag = 7, differences = 1)
plot(Date_1820, acc_1820_des, type="l", cex.axis=0.5,col="blue")

pacf(as.vector(acc_1820_des), na.action=na.pass)
acc_1820_des = diff(acc_1820_des, lag = 5, differences = 1)
plot(Date_1820, acc_1820_des, type="l", cex.axis=0.5,col="blue")

pacf(as.vector(acc_1820_des), na.action=na.pass)
