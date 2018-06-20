library(dplyr)
library(lubridate)
Sys.setlocale("LC_ALL", "Russian_Russia")

#
#filters
#
filterExp <- function(.data, expVal, prec = .2) {
  filter(.data, between(exp, 
                        round(expVal * (1 - prec), 1), 
                        round(expVal * (1 + prec), 1)))
}

filterLang <- function(.data, langs = c("C++", "C")) {
  filter(.data, lang %in% langs)
}

filterCity <- function(.data, locs = "Киев") {
  filter(.data, loc %in% locs)
}

filterRoleclass <- function(.data, class = "DEV") {
  filter(.data, cls == class)
}


#origin data

mysel <- data.frame(
  value = c(250, 300, 1100, 1700, 2000, 2800, 3200, 3200),
  totexp = c( 0, 0.25,  2.2,  3.0,  4.0,  5.0,  6.0, 7.0),
  date = as.Date(c("2008-09-15", "2008-12-15", "2011-08-15", "2012-04-15",
                   "2013-05-15", "2014-02-15", "2015-09-15", "2016-09-15"))
)

#plot(x = mysel$date, y = mysel$value, type = "l")


#impute additional info
mysel2 <- data.frame(
  totexp = mysel$totexp,
  date = as.Date(sapply(mysel$date, FUN = function(x){as.character(x - 1)})),
  value = c(0, mysel$value[1:(length(mysel$value) - 1)])
)

mysel0 <- merge(mysel, 
               mysel2[2:nrow(mysel2),], 
               by = c("date", "value", "totexp"), 
               sort = TRUE, 
               all = TRUE)

#plot(x = mysel0$date, y = mysel0$value, type = "l")

#calculate experience to date
expToDate <- function(date) {
  theDate <- as.Date(date)
  last_recored <- max(mysel0$date[mysel0$date < theDate])
  mysel0[mysel0$date == last_recored, "totexp"] + as.integer(theDate - last_recored) / 365
}


calculateSal <- function(datafile) {
  #get date from filename
  uspos <- gregexpr("_", datafile)
  
  thisDate <- ymd(paste0(
    as.integer(substr(datafile, 1, 4)),
    substr(datafile, uspos[[1]][1] + 1, uspos[[1]][2] - 1),
    "15"
  ))
  
  #read data from file
  sal <- read.csv(paste0("./data/dou/", datafile), encoding = "UTF-8")
  
  #most common case
  if (thisDate >= as.Date("2011-09-15")) {
    names(sal)[which(names(sal) == "Язык.программирования")] <- "lang"
    sal %>%
      filterLang() %>%
      filterExp(expVal = expToDate(thisDate)) %>%
      filterCity() %>%
      select(salary) -> dat
    qua <- quantile(dat$sal, na.rm = TRUE)
    data.frame(date = thisDate, p25 = qua[2], p50 = qua[3], p75 = qua[4])
  }
}


#files list
fl <- dir("data/dou/")[10+3*(0:11)]

#get results
do.call("rbind", lapply(fl[1:10], FUN = calculateSal))



#salary quantiles
sq <- data.frame()
dates <- c()



#october2010
sal <- read.csv("./data/dou/2010_october_clean.csv", encoding = "UTF-8")
names(sal)[c(1, 2, 4, 5)] <- c("loc", "exp", "lang", "sal")

#extrapolate to early days

#dec2008
sal %>%
  filterExp(expVal = expToDate("2008-12-15"), prec = .99) %>%
  filterCity(locs = "другой") %>%
  select(sal) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2008-12-15")

#may2009
sal %>%
  filterExp(expVal = expToDate("2009-05-15"), prec = .5) %>%
  filterCity(locs = "другой") %>%
  select(sal) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2009-05-15")

#dec2009
sal %>%
  filterExp(expVal = expToDate("2009-12-15")) %>%
  filterCity(locs = "другой") %>%
  select(sal) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2009-12-15")

#may2010
sal %>%
  filterExp(expVal = expToDate("2010-05-15")) %>%
  filterCity(locs = "другой") %>%
  select(sal) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2010-05-15")


#go ahead
sal %>%
  filterExp(expVal = expToDate("2010-10-15")) %>%
  filterCity(locs = "другой") %>%
  select(sal) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2010-10-15")



#may2011
sal <- read.csv("./data/dou/2011_may_final.csv", encoding = "UTF-8")
names(sal)[which(names(sal) == "Язык.программирования")] <- "lang"
sal %>%
  filterRoleclass() %>%
  filterExp(expVal = expToDate("2011-05-15")) %>%
  filterCity(locs = "other") %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2011-05-15")

#dec2011
sal <- read.csv("./data/dou/2011_dec_final.csv", encoding = "UTF-8")
names(sal)[which(names(sal) == "Язык.программирования")] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2011-12-15")) %>%
  filterCity(locs = "Киев") %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2011-12-15")

# ... #

sq["dates"] <- as.Date(dates)
names(sq)[1:5] <- c("min", "p25", "med", "p75", "max")

plot(x = mysel0$date, y = mysel0$value, type = "l", lwd = 2)
lines(x = sq$dates, y = sq$med, lwd = 2, col = "blue", lty = "dotted")
lines(x = sq$dates, y = sq$p25, lwd = 2, col = "red", lty = "dashed")
lines(x = sq$dates, y = sq$p75, lwd = 2, col = "green", lty = "longdash")
