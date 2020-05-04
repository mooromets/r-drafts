require("dplyr")

#apply a function on every file that exists in data 
fapply <- function (dir, data, func) {
  files <- list.files(path=dir, pattern="*.*", full.names=TRUE, recursive=FALSE)
  lapply(files, function(x) {
    fn <- basename(x)
    size <- round(file.size(x)/1024/1024, 2)
    if (sum(data$FileName == fn & data$SizeMB == size) > 0) {
      if (func(x))
        c(fn, size)
    } else
      c(fn, 0.0) 
  })  
}

#remove files that exist in data
cleanDir <- function(dir, data) {
  res <- fapply(dir, data, file.remove)
  tmpDF <- as.data.frame(do.call(rbind, res))
  csvName <- paste0(gsub("[:/]+", "-", dir), ".csv")
  write.csv(tmpDF, csvName)
  (tmpDF)
}

#drop rows in data frame where rows contain processed dirs
dropRows <- function(data) {
  skip <- scan("skip.txt", what=character())
  for(x in skip) {
    data <- data[!grepl(x, data$Dir, ignore.case = TRUE),]
  }
  data
}
