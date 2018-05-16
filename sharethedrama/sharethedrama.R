library(dplyr)
library(readtext)
library(plyr)

dat <- read.csv("names.csv")

# extract first and last names from the name string
t(apply(X = dat,
      MARGIN = 1,
      FUN =  function (x) {
       names <- unlist(strsplit(x["name"], " "))
       if (x["type"] == 1)
          return (c(names[length(names)], names[1]))
       else
         return (c(names[1], names[length(names)]))
      })) -> dat[, c("Firstname", "Lastname")]

# city and codes data
temp <- read.csv("cityncode.csv")
code <- temp[, "code"]
city <- temp[, "city"]

#generate users
num <- 4
ldply  (sample(c("m", "f"), num, replace = TRUE),
        .fun = function(sex){
          fn <- sample(with(dat, Firstname[name1 == sex]), 1)
          ln <- sample(with(dat, Lastname[name2 == sex | name2 == "u" ]), 1)
          email <- paste0(tolower(fn),
                          sample(c("10":"23",
                                   "88":"99", 
                                   rep(paste0(c("", "-"), 
                                              substr(tolower(ln), 1, sample(3:nchar(ln), 1))), 
                                       10)),
                                 1),
                          "@",
                          sample(c("ukr.net", "i.ua", "email.ua", "mail.ua", "yandex.ua"), 1))
          phone <- sprintf("+38(0%d)%03d-%03d-%02d", 
                           sample(code, 1),
                           sample(1:1000, 1),
                           sample(1:100, 1),
                           sample(1:100, 1))
          city <- sample(as.character(city), 1)
          return(c(paste(fn, ln), email, city, phone))
        }) -> users

#get the macros
macros <- scan(file = "sample.iim", what = character(), sep = "\n")

#process all users
apply(users,
      1,
       FUN = function(u){
         sapply(macros,
                USE.NAMES = FALSE,
                FUN = function(s) {
                  s <- gsub("sample@mail.net", u[2], s)
                  s <- gsub("john<SP>doe", gsub(" ", "<SP>", u[1]), s)
                  s <- gsub("kyiv", u[3], s)
                  s <- gsub("38phone", u[4], s)
                })
         }) -> script

write(as.vector(script), "difference.iim") 

#write log
oldUsers <- read.csv("log.txt", header = FALSE)
write.csv(rbind(oldUsers, users), "log.txt", row.names = FALSE)
