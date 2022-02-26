setwd("~/.STA202_BDD/")

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

data_2005 = read.csv("caracteristiques_2005.csv")
data_2006 = read.csv("caracteristiques_2006.csv")
data_2007 = read.csv("caracteristiques_2007.csv")
data_2008 = read.csv("caracteristiques_2008.csv")
data_2009 = read.delim2("caracteristiques_2009.csv")
data_2010 = read.csv("caracteristiques_2010.csv")
data_2011 = read.csv("caracteristiques_2011.csv")
data_2012 = read.csv("caracteristiques_2012.csv")
data_2013 = read.csv("caracteristiques_2013.csv")
data_2014 = read.csv("caracteristiques_2014.csv")
data_2015 = read.csv("caracteristiques_2015.csv")
data_2016 = read.csv("caracteristiques_2016.csv")
data_2017 = read.csv("caracteristiques-2017.csv")
data_2018 = read.csv("caracteristiques-2018.csv")
data_2019 = read.csv2("caracteristiques-2019.csv")
data_2020 = read.csv2("caracteristiques-2020.csv")

curr_dat = rbind(data_2005, data_2006, data_2007, data_2008, data_2009)

curr_dat = rbind(curr_dat, data_2010, data_2011, data_2012, data_2013)

curr_dat = rbind(curr_dat, data_2014, data_2015, data_2016, data_2017)

curr_dat = rbind(curr_dat, data_2018)

curr_dat = unpackk(curr_dat)

curr_dat = subset(curr_dat, select = -c(gps, an, mois, jour, hrmn))

curr_dat2 = rbind(data_2019, data_2020)
curr_dat2 = unpackk2(curr_dat2)

curr_dat2 = subset(curr_dat2, select = -c(an, mois, jour, hrmn))

f_dat = rbind(curr_dat, curr_dat2)




