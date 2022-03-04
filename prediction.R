#prediction lissage expo




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
##.

##Dates
date1=strptime("01/01/2005", "%d/%m/%Y")
date2=strptime("31/12/2016", "%d/%m/%Y")
date3=strptime("31/12/2017", "%d/%m/%Y")
date4=strptime("31/10/2020", "%d/%m/%Y")
date5=strptime("31/12/2020", "%d/%m/%Y")

Date=seq.POSIXt(date1,date5, by = "day")

Date_0516=seq.POSIXt(date1,date2, by = "day")
Date_0517=seq.POSIXt(date1,date3, by = "day")
Date_17=seq.POSIXt(date2+1,date3, by = "day")

Date_1820_=seq.POSIXt(date3,date4, by = "day")
Date_1820=seq.POSIXt(date3+1,date5, by = "day")
Date_20=seq.POSIXt(date4+1,date5, by = "day")

##.

#0517



##TS
acc_grav_ts=xts(acc_grav_pday,order.by=Date)
acc_grav_ts_0516=xts(acc_grav_ts[Date_0516,],order.by=Date_0516)
acc_grav_ts_0517=xts(acc_grav_ts[Date_0517,],order.by=Date_0517)

acc_grav_ts_1820_=xts(acc_grav_ts[Date_1820_,],order.by=Date_1820_)
acc_grav_ts_1820=xts(acc_grav_ts[Date_1820,],order.by=Date_1820)

acc_lege_ts=xts(acc_lege_pday,order.by=Date)
acc_lege_ts_0516=xts(acc_lege_ts[Date_0516,],order.by=Date_0516)
acc_lege_ts_0517=xts(acc_lege_ts[Date_0517,],order.by=Date_0517)

acc_lege_ts_1820_=xts(acc_lege_ts[Date_1820_,],order.by=Date_1820_)
acc_lege_ts_1820=xts(acc_lege_ts[Date_1820,],order.by=Date_1820)
##.

##Lissage 

##grav
holt_acc_grav_0516=HoltWinters(ts(acc_grav_ts_0516,frequency = 7), alpha = NULL, beta = NULL, gamma = NULL,
            seasonal = c("additive"),
            start.periods = 4, l.start = NULL, b.start = NULL,
            s.start = NULL,
            optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
            optim.control = list())

print(c(holt_acc_grav_0516$alpha,holt_acc_grav_0516$beta,holt_acc_grav_0516$gamma))
liss_acc_grav_0516=xts(c(integer(7),holt_acc_grav_0516$fitted[,1]),order.by=Date_0516)

plot(acc_grav_ts_0516,type="l",col="black")
lines(liss_acc_grav_0516,type="l",col="blue")



date6=strptime("01/01/2017", "%d/%m/%Y")
date7=strptime("31/01/2017", "%d/%m/%Y")
date_predic=seq.POSIXt(date6,date7, by = "day")

predic_acc_grav_17=predict(holt_acc_grav_0516,n.ahead=31)
predic_acc_grav_17=xts(predic_acc_grav_17,order.by=date_predic)


plot(acc_grav_ts_0517[date_predic,],type="l",col="black",ylim=c(50,85))
lines(predic_acc_grav_17,type="l",col="darkgreen")

##grav.

##lege
holt_acc_lege_0516=HoltWinters(ts(acc_lege_ts_0516,frequency = 7), alpha = NULL, beta = NULL, gamma = NULL,
                               seasonal = c("additive"),
                               start.periods = 4, l.start = NULL, b.start = NULL,
                               s.start = NULL,
                               optim.start = c(alpha = 0.1, beta = 0.1, gamma = 0.1),
                               optim.control = list())

print(c(holt_acc_lege_0516$alpha,holt_acc_lege_0516$beta,holt_acc_lege_0516$gamma))
liss_acc_lege_0516=xts(c(integer(7),holt_acc_lege_0516$fitted[,1]),order.by=Date_0516)

plot(acc_lege_ts_0516,type="l",col="black")
lines(liss_acc_lege_0516,type="l",col="blue")



date6=strptime("01/01/2017", "%d/%m/%Y")
date7=strptime("31/01/2017", "%d/%m/%Y")
date_predic=seq.POSIXt(date6,date7, by = "day")

predic_acc_lege_17=predict(holt_acc_lege_0516,n.ahead=31)
predic_acc_lege_17=xts(predic_acc_lege_17,order.by=date_predic)


plot(acc_lege_ts_0517[date_predic,],type="l",col="black",ylim=c(100,350))
lines(predic_acc_lege_17,type="l",col="darkgreen")
##lege.

##LIssage.


##par décomposition

##.
