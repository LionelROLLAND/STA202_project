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

cond = (days >= date1 & days <= date2)
acc_lege_0517 = subset(acc_lege_pday, cond)
pacf(acc_lege_0517, 40)
####


hist(acc_grav_ts_0517, breaks = 30, freq=FALSE)
mean_grav_0517 = mean(acc_grav_ts_0517)
var_grav_0517 = var(acc_grav_ts_0517)

theta_chap = var_grav_0517/mean_grav_0517
k_chap = mean_grav_0517/theta_chap

test = c(0:1000)
lines(dgamma(test, shape = k_chap, scale = theta_chap))

acc_grav_0517 = subset(acc_grav_pday, cond)
pacf(acc_grav_0517, 40)
pacf(acc_grav_0517, 366)
acf(acc_grav_0517, 40)
######

hist(acc_tot_ts_1820, breaks = 30, freq = FALSE)

mean_tot_1820 = mean(acc_tot_ts_1820)
var_tot_1820 = var(acc_tot_ts_1820)

theta_chap = var_tot_1820/mean_tot_1820
k_chap = mean_tot_1820/theta_chap

test = c(0:1000)
lines(dgamma(test, shape = k_chap, scale = theta_chap))

pacf(acc_grav_pday2+acc_lege_pday2, 40)