users <- 699000
points <- 8690

sinp <- points/users
cat("sinlge prob", sinp, "\n")

sump <- sum(sinp^(1:15))
cat("prob from 15 draws", sump, "\n")
