library(dplyr)
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
num <- 20
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
          phone <- sample(code, 1) *10000000 + sample(1:10000000, 1)
          city <- sample(as.character(city), 1)
          return(c(paste(fn, ln), email, city, phone))
        })