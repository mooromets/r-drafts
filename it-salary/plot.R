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


#october2010
sal <- read.csv("./data/dou/2010_october_clean.csv", encoding = "UTF-8")
names(sal)[c(1, 2, 4, 5)] <- c("loc", "exp", "lang", "sal")
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2010-10-15")) %>%
  filterCity(locs = "другой") %>%
  select(sal) -> dat
mean(dat$sal, na.rm = TRUE)
median(dat$sal, na.rm = TRUE)


#may2011
sal <- read.csv("./data/dou/2011_may_final.csv", encoding = "UTF-8")
names(sal)[6] <- "lang"
sal %>%
  filterLang() %>%
  filterExp(expVal = expToDate("2011-05-15")) %>%
  filterCity(locs = "other") %>%
  select(salary) -> dat
mean(dat$sal)
median(dat$sal)
