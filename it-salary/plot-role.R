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


calculateSal <- function(datafile, expFun = expToDate) {
  #get date from filename
  uspos <- gregexpr("_", datafile)
  
  thisDate <- ymd(paste0(
    as.integer(substr(datafile, 1, 4)),
    substr(datafile, uspos[[1]][1] + 1, uspos[[1]][2] - 1),
    "15"
  ))
  
  #read data from file
  sal <- read.csv(paste0("./data/dou/", datafile), encoding = "UTF-8")
  
  #general data format case
  if (thisDate >= as.Date("2011-01-15")) {
    thisLoc = ifelse(thisDate == as.Date("2011-05-15"), "other", "Киев")
    sal %>% 
      filterRoleclass() %>%
      filterExp(expVal = expFun(thisDate)) %>%
      filterCity(locs = thisLoc) %>%
      select(salary) -> dat
  } else {
    #old data format
    names(sal)[c(1, 2, 4, 5)] <- c("loc", "exp", "lang", "sal")
    sal %>%
      filterExp(expVal = expFun(thisDate)) %>%
      filterCity(locs = "другой") %>%
      select(sal) -> dat
  }
  qua <- quantile(dat$sal, na.rm = TRUE)
  data.frame(date = thisDate, p25 = qua[2], p50 = qua[3], p75 = qua[4])
  
}


#files list
fl <- dir("data/dou/")[c(1, 3, 7 + 3 * (0:10))]

#get normal results
salHist <- do.call("rbind", lapply(fl[1:13], FUN = calculateSal))

#get extrapolated early years
ey <- c("2008-12-15", "2009-05-15", "2009-12-15", "2010-05-15")
do.call("rbind", lapply(ey, 
                        FUN = function(x) {
                          calculateSal(fl[1], expFun = function(y) expToDate(x))
                        } ) ) ->
  salExtra
salExtra$date <- as.Date(ey)
#TODO increase experince prec parameter in extrapolation calculations for
#more acurate results

#concat
salFinal <- rbind(salHist, salExtra)

#sort
salFinal <- salFinal[order(salFinal$date), ]

#plot
plot(x = mysel0$date, y = mysel0$value, type = "l", lwd = 2)
lines(x = salFinal$date, y = salFinal$p50, lwd = 2, col = "blue", lty = "dotted")
lines(x = salFinal$date, y = salFinal$p25, lwd = 2, col = "red", lty = "dashed")
lines(x = salFinal$date, y = salFinal$p75, lwd = 2, col = "green", lty = "longdash")
