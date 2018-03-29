
library(dplyr)

setwd("./drive_distance/")

plz <- read.csv("./de_postal_codes.csv")
plzNeureut <- read.csv("./distances76149.csv")

# get lat and long for selected postal codes
df <- inner_join(plz, plzNeureut, by = c("Postal.Code" = "plz"))

# skip unused variables
out <- select(df, Place.Name, Latitude, Longitude, km)

#save every circle to a separate file
lapply(unique(out$km), 
       function (x) {
         tmp <- out[out$km == x, ]
         write.csv(tmp, paste0("points", as.character(x), ".csv"))
       })
