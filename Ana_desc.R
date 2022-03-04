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

##Analyse descriptive

###Accidentes legers 2005-2017

plot(acc_lege_ts_0517)

###Histogramme + loi

hist(acc_lege_ts_0517, breaks = 20, freq=FALSE)
mean_lege_0517 = mean(acc_lege_ts_0517)
var_lege_0517 = var(acc_lege_ts_0517)
prob_chap = 1 - var_lege_0517/mean_lege_0517
size_chap = as.integer(mean_lege_0517/prob_chap)
p_chap = mean_lege_0517/var_lege_0517
n_chap = mean_lege_0517*p_chap/(1-p_chap)
theta_chap = var_lege_0517/mean_lege_0517
k_chap = mean_lege_0517/theta_chap
test = c(0:1000)
#lines(dbinom(test, size = size_chap, prob = prob_chap))
#lines(dnorm(test, mean = mean_lege_0517, sd = sd_lege_0517))
lines(dgamma(test, shape = k_chap, scale = theta_chap))
#lines(dnbinom(test, size = n_chap, prob = p_chap))

###pacf, acf

cond = (days >= date1 & days <= date2)
acc_lege_0517 = subset(acc_lege_pday, cond)
acf(acc_lege_0517, 40)
pacf(acc_lege_0517, 40)

###Dangerosite par jour de la semaine

wd = weekdays(Date_0517)
lege_week_0517 = tapply(acc_lege_0517, INDEX=wd, FUN=sum)
ord = c(7,4,1,2,3,6,5)
lege_week_0517 = lege_week_0517[order(ord)]
wd_x= unlist(dimnames(lege_week_0517), recursive=TRUE)
plot(lege_week_0517, xaxt="n", ylim=c(0,300000))
axis(side=1, labels=wd_x, at=c(1:7), cex.axis=0.8)

###Dangerosite par mois

mo = format(Date_0517, format="%B")
lege_mo_0517 = tapply(acc_lege_0517, INDEX=mo, FUN=sum)
ord_mo = c(8,4,12,2,1,7,6,5,3,11,10,9)
lege_mo_0517 = lege_mo_0517[order(ord_mo)]
mo_x = unlist(dimnames(lege_mo_0517), recursive=TRUE)
plot(lege_mo_0517, xaxt="n", ylim=c(0,150000))
axis(side=1, labels=mo_x, at=c(1:12), cex.axis=0.8)

####accidentes graves 2005 - 2017

plot(acc_grav_ts_0517)

###Histogramme + loi

hist(acc_grav_ts_0517, breaks = 30, freq=FALSE)
mean_grav_0517 = mean(acc_grav_ts_0517)
var_grav_0517 = var(acc_grav_ts_0517)
theta_chap = var_grav_0517/mean_grav_0517
k_chap = mean_grav_0517/theta_chap
test = c(0:1000)
lines(dgamma(test, shape = k_chap, scale = theta_chap))

###pacf, acf

acc_grav_0517 = subset(acc_grav_pday, cond)
pacf(acc_grav_0517, 40)
pacf(acc_grav_0517, 400)
acf(acc_grav_0517, 40)

###Dangerosite par jour de la semaine

grav_week_0517 = tapply(acc_grav_0517, INDEX=wd, FUN=sum)
grav_week_0517 = grav_week_0517[order(ord)]
plot(grav_week_0517, xaxt="n", ylim=c(0,100000))
axis(side=1, labels=wd_x, at=c(1:7), cex.axis=0.8)

###Dangerosite par mois

grav_mo_0517 = tapply(acc_grav_0517, INDEX=mo, FUN=sum)
grav_mo_0517 = grav_mo_0517[order(ord_mo)]
plot(grav_mo_0517, xaxt="n", ylim=c(0,50000))
axis(side=1, labels=mo_x, at=c(1:12), cex.axis=0.8)
######

##Accidentes 2018 - 2020

plot(acc_tot_ts_1820)

###Histogramme + loi

hist(acc_tot_ts_1820, breaks = 30, freq = FALSE)
mean_tot_1820 = mean(acc_tot_ts_1820)
var_tot_1820 = var(acc_tot_ts_1820)
theta_chap = var_tot_1820/mean_tot_1820
k_chap = mean_tot_1820/theta_chap
test = c(0:1000)
lines(dgamma(test, shape = k_chap, scale = theta_chap))

###pacf, acf

pacf(acc_grav_pday2+acc_lege_pday2, 40)
acf(acc_grav_pday2+acc_lege_pday2, 40)

###Dangerosite par jour de la semaine

wd = weekdays(days2)
week_1820 = tapply(acc_grav_pday2+acc_lege_pday2, INDEX=wd, FUN=sum)
week_1820 = grav_week_0517[order(ord)]
plot(week_1820, xaxt="n", ylim=c(0,70000))
axis(side=1, labels=wd_x, at=c(1:7), cex.axis=0.8)

###Dangerosite par mois

mo = format(days2, format="%B")
mo_1820 = tapply(acc_grav_pday2+acc_lege_pday2, INDEX=mo, FUN=sum)
mo_1820 = mo_1820[order(ord_mo)]
plot(grav_mo_0517, xaxt="n", ylim=c(0,50000))
axis(side=1, labels=mo_x, at=c(1:12), cex.axis=0.8)

