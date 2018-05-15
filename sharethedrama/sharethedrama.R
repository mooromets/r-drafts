#library(dplyr)
dat <- read.csv("names.csv")

apply(X = dat,
      MARGIN = 1,
      FUN =  function (x) {
       names <- unlist(strsplit(x["name"], " "))
       if (x["type"] == 1)
          return (c(names[length(names)], names[1]))
       else
         return (c(names[1], names[length(names)]))
      }) -> 
  dat[, c("Firstname", "Lastname")]

temp <- read.csv("cityncode.csv")
code <- temp[, "code"]
city <- temp[, "city"]