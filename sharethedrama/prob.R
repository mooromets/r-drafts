users <- 511000
points <- 3500

sinp <- points/users
cat("sinlge prob", sinp, "\n")

sump <- sum(sinp^(1:15))
cat("prob from 15 draws", sump, "\n")
