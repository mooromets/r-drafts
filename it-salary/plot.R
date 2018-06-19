library(dplyr)

#origin

mysel <- data.frame(
  value = c(250, 300, 1100, 1700, 2000, 2800, 3200, 3200),
  totexp = c( 0, 0.25,  2.2,  3.0,  4.0,  5.0,  6.0, 7.0),
  date = as.Date(c("2008-09-15", "2008-12-15", "2011-08-15", "2012-04-15",
                   "2013-05-15", "2014-02-15", "2015-09-15", "2016-09-15"))
)

plot(x = mysel$date, y = mysel$value, type = "l")

expToDate <- function(date) {
  theDate <- as.Date(date)
  last_recored <- max(mysel0$date[mysel0$date < theDate])
  mysel0[mysel0$date == last_recored, "totexp"] + as.integer(theDate - last_recored) / 365
}

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

plot(x = mysel0$date, y = mysel0$value, type = "l")

Sys.setlocale("LC_ALL", "Russian_Russia")

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
names(sal)[6] <- "lang"
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
names(sal)[7] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2011-12-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2011-12-15")

#may2012
sal <- read.csv("./data/dou/2012_may_final.csv", encoding = "UTF-8")
names(sal)[7] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2012-05-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2012-05-15")

#dec2012
sal <- read.csv("./data/dou/2012_dec_final.csv", encoding = "UTF-8")
names(sal)[3] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2012-12-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2012-12-15")

#may2013
sal <- read.csv("./data/dou/2013_may_final.csv", encoding = "UTF-8")
names(sal)[3] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2013-05-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2013-05-15")

#dec2013
sal <- read.csv("./data/dou/2013_dec_final.csv", encoding = "UTF-8")
names(sal)[3] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2013-12-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2013-12-15")

#may2014
sal <- read.csv("./data/dou/2014_may_final.csv", encoding = "UTF-8")
names(sal)[2] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2014-05-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2014-05-15")

#dec2014
sal <- read.csv("./data/dou/2014_dec_final.csv", encoding = "UTF-8")
names(sal)[2] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2014-12-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2014-12-15")

#may2015
sal <- read.csv("./data/dou/2015_may_final.csv", encoding = "UTF-8")
names(sal)[2] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2015-05-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2015-05-15")

#dec2015
sal <- read.csv("./data/dou/2015_dec_final.csv", encoding = "UTF-8")
names(sal)[3] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2015-12-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2015-12-15")

#may2016
sal <- read.csv("./data/dou/2016_may_final.csv", encoding = "UTF-8")
names(sal)[3] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2016-05-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2016-05-15")

#dec2016
sal <- read.csv("./data/dou/2016_dec_final.csv", encoding = "UTF-8")
names(sal)[3] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2016-12-15")) %>%
  filterCity() %>%
  select(salary) -> dat
print(dim(dat))
sq <- rbind(sq, print(quantile(dat$sal, na.rm = TRUE)))
dates <- c(dates, "2016-12-15")


sq["dates"] <- as.Date(dates)
names(sq)[1:5] <- c("min", "p25", "med", "p75", "max")

plot(x = mysel0$date, y = mysel0$value, type = "l", lwd = 2)
lines(x = sq$dates, y = sq$med, lwd = 2, col = "blue", lty = "dotted")
lines(x = sq$dates, y = sq$p25, lwd = 2, col = "red", lty = "dashed")
lines(x = sq$dates, y = sq$p75, lwd = 2, col = "green", lty = "longdash")
