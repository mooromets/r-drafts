library(lubridate)

dat <- read.csv("./data/data.csv")

#covert to date
dat[, 4] <- dmy(dat[, 4])
dat[, 6] <- dmy(dat[, 6])
dat[, 10] <- dmy(dat[, 10])

#remove recent
dat <-  dat[(dat[, 4] < Sys.Date() - 12 ), ]

apply(dat, 1, FUN = function(x) {
  if ( is.na(x[6]) )
    return (NA)
  else
    return (sum(!weekdays(seq(as.Date(x[4]), as.Date(x[6]), "days")) %in% c("Saturday", "Sunday")))
}) -> all

apply(dat, 1, FUN = function(x) {
  if (is.na(x[10]) && !is.na(x[6]))
    return (sum(!weekdays(seq(as.Date(x[4]), as.Date(x[6]), "days")) %in% c("Saturday", "Sunday")))
  else 
    return (NA)
}) -> noSuc

apply(dat, 1, FUN = function(x) {
  if (!is.na(x[10]) && !is.na(x[6]))
    return (sum(!weekdays(seq(as.Date(x[4]), as.Date(x[6]), "days")) %in% c("Saturday", "Sunday")))
  else 
    return (NA)
}) -> suc


#probs = seq(0.25, .75, 0.25) #probs <- c(0.05, .25, .5, .75, .95) 
probs = seq(0, 1, 0.25)

print("-= ALL =-")
cat("count:", length(all), "\n")
cat("Mean:", round(mean(all, na.rm = TRUE), 1), "\n")
print(quantile(all, na.rm = TRUE, probs = probs))
cat("no answer:", sum(is.na(all)), "=>",  round(sum(is.na(all)) / length(all), 3) * 100, "% ")
cat("\n\n")

print("-= not successfull =-")
cat("count:", sum(!is.na(noSuc)), "\n")
cat("Mean:", round(mean(noSuc, na.rm = TRUE), 1), "\n")
print(quantile(noSuc, na.rm = TRUE, probs = probs))
cat("\n\n")


print("-= successfull =-")
cat("count:", sum(!is.na(suc)), "\n")
cat("Mean:", round(mean(suc, na.rm = TRUE), 1), "\n")
print(quantile(suc, na.rm = TRUE, probs = probs))
cat("\n\n")    