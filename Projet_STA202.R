rm(list=ls())
setwd("~/.STA202_BDD/STA202_project/BDD/")

tFun=function(x) {
  #print(x$an)
  print(x)
  print("\r")
}

unpackk=function(dat) {
  annee = as.character(2000 + dat$an)
  str_date = paste(as.character(dat$jour),as.character(dat$mois),annee,sep="/")
  #date_format = strptime(dat_date, format="%d/%m/%Y")
  str_time = as.character(dat$hrmn)
  str_min = substr(str_time, nchar(str_time)-1, nchar(str_time))
  str_hour = substr(str_time, nchar(str_time)-3, nchar(str_time)-2)
  str_t_format = paste(str_hour, str_min, sep=":")
  str_datetime = paste(str_date, str_t_format, sep=" ")
  data_datetime = strptime(str_datetime, format="%d/%m/%Y %H:%M", tz="CEST")
  dat$date = data_datetime
  return(dat)
}

unpackk2=function(dat) {
  annee = as.character(dat$an)
  time = dat$hrmn
  str_date = paste(dat$jour, dat$mois, annee, sep="/")
  str_datetime = paste(str_date, time, sep=" ")
  data_datetime = strptime(str_datetime, format="%d/%m/%Y %H:%M")
  dat$date = data_datetime
  return(dat)
}

###
cara_2005 = read.csv("caracteristiques_2005.csv")
cara_2006 = read.csv("caracteristiques_2006.csv")
cara_2007 = read.csv("caracteristiques_2007.csv")
cara_2008 = read.csv("caracteristiques_2008.csv")
cara_2009 = read.delim2("caracteristiques_2009.csv")##########"
cara_2010 = read.csv("caracteristiques_2010.csv")
cara_2011 = read.csv("caracteristiques_2011.csv")
cara_2012 = read.csv("caracteristiques_2012.csv")
cara_2013 = read.csv("caracteristiques_2013.csv")
cara_2014 = read.csv("caracteristiques_2014.csv")
cara_2015 = read.csv("caracteristiques_2015.csv")
cara_2016 = read.csv("caracteristiques_2016.csv")
cara_2017 = read.csv("caracteristiques-2017.csv")
cara_2018 = read.csv("caracteristiques-2018.csv")
cara_2019 = read.csv2("caracteristiques-2019.csv")######### pas le même séparateur
cara_2020 = read.csv2("caracteristiques-2020.csv")#########"""
###
usag_2005 = read.csv("usagers_2005.csv")
usag_2006 = read.csv("usagers_2006.csv")
usag_2007 = read.csv("usagers_2007.csv")
usag_2008 = read.csv("usagers_2008.csv")
usag_2009 = read.csv("usagers_2009.csv")####
usag_2010 = read.csv("usagers_2010.csv")
usag_2011 = read.csv("usagers_2011.csv")
usag_2012 = read.csv("usagers_2012.csv")
usag_2013 = read.csv("usagers_2013.csv")
usag_2014 = read.csv("usagers_2014.csv")
usag_2015 = read.csv("usagers_2015.csv")
usag_2016 = read.csv("usagers_2016.csv")
usag_2017 = read.csv("usagers-2017.csv")
usag_2018 = read.csv("usagers-2018.csv")
usag_2019 = read.csv2("usagers-2019.csv")############"
usag_2020 = read.csv2("usagers-2020.csv")#############

####
curr_cara = rbind(cara_2005, cara_2006, cara_2007, cara_2008, cara_2009)

curr_cara= rbind(curr_cara, cara_2010, cara_2011, cara_2012, cara_2013)

curr_cara= rbind(curr_cara, cara_2014, cara_2015, cara_2016, cara_2017)

curr_cara= rbind(curr_cara, cara_2018)

###
curr_usag = rbind(usag_2005, usag_2006, usag_2007, usag_2008, usag_2009)

curr_usag= rbind(curr_usag, usag_2010, usag_2011, usag_2012, usag_2013)

curr_usag= rbind(curr_usag, usag_2014, usag_2015, usag_2016, usag_2017)

curr_usag= rbind(curr_usag, usag_2018)

###

curr_dat= merge(curr_cara,curr_usag,by="Num_Acc")
##curr_dat= curr_cara
###

curr_dat = unpackk(curr_dat)

curr_dat = subset(curr_dat, select = c(Num_Acc,grav,date))#-c(gps, an, mois, jour, hrmn))

###

curr_cara2 = rbind(cara_2019, cara_2020)
curr_usag2= rbind(usag_2019, usag_2020)
curr_dat2= merge(curr_cara2,curr_usag2,by="Num_Acc")

###

curr_dat2 = unpackk2(curr_dat2)

curr_dat2 = subset(curr_dat2, select = c(Num_Acc,grav,date))# -c(an, mois, jour, hrmn))

f_dat = rbind(curr_dat, curr_dat2)

###
f_grav = subset(f_dat,(grav==3))####################################"
f_lege = subset(f_dat,(grav==1|grav==4))
###

str_grav_days = format(f_grav$date, "%d/%m/%Y")
str_lege_days = format(f_lege$date, "%d/%m/%Y")

#days = strptime(str_days, "%d/%m/%Y", tz="CEST")

acc_grav_pday = tapply(f_grav$Num_Acc, as.factor(str_grav_days), length)
acc_lege_pday = tapply(f_lege$Num_Acc, as.factor(str_lege_days), length)

days = names(acc_grav_pday)
days = strptime(days, "%d/%m/%Y")

acc_grav_pday = acc_grav_pday[order(days)]
acc_lege_pday = acc_lege_pday[order(days)]

days = days[order(days)]

plot(days, acc_grav_pday+acc_lege_pday, type="l",col="black")

plot(days, acc_lege_pday, type="l",col="darkgreen", ylab="Nb d'accidenté",xlab="Jours")

lines(days, acc_grav_pday, type="l",col="red")
legend(100,95,legend=c("Legers","Graves"),col=c("green","red"),lty=1:2, cex=0.8)



filter_s = strptime("01/01/2005", "%d/%m/%Y")
filter_e = strptime("31/01/2020", "%d/%m/%Y")
cond = (days >= filter_s & days <= filter_e)
s_days = subset(days, cond)

s_acc_grav = subset(acc_grav_pday, cond)
s_acc_lege = subset(acc_lege_pday, cond)
#plot(s_days, s_acc, type="l", lab=c(5, 5, 7), cex.axis=0.5)

plot(s_days, s_acc_lege, type="l", cex.axis=0.5,col="darkgreen")
lines(s_days, s_acc_grav, type="l", cex.axis=0.5,col="red")
#weekdays(s_days)
#test_day = strptime("28/03/2005", "%d/%m/%Y")
#weekdays(test_day)

###
reg=lm(acc_grav_pday~acc_lege_pday)
summary(reg)
###

saveRDS(acc_grav_pday,file="acc_grav_pday.rds")
saveRDS(acc_lege_pday,file="acc_lege_pday.rds")
saveRDS(days,file="days.rds")