source("./globals.R")

require("dplyr")

#TODO obsolete?
#apply a function on every file that exists in data
fapply <- function (dir, data, func) {
  files <- list.files(path=dir, pattern="*.*", full.names=TRUE, recursive=FALSE)
  lapply(files, function(x) {
    fn <- basename(x)
    size <- round(file.size(x)/1024/1024, 2)
    if (sum(data$FileName == fn & abs(data$SizeMB - size) < 0.02) > 0) {
      if (func(x))
        c(fn, size)
    } else
      c(fn, 0.0) 
  })  
}

#TODO obsolete?
#remove files that exist in data
cleanDir <- function(dir, data) {
  res <- fapply(dir, data, file.remove)
  tmpDF <- as.data.frame(do.call(rbind, res))
  csvName <- paste0(gsub("[:/]+", "-", dir), ".csv")
  write.csv(tmpDF, csvName, fileEncoding = ENCODING)
  (tmpDF)
}

#drop rows in data frame where rows contain processed dirs
dropRows <- function(data, skipFile = SKIP_DIR_FILE) {
  skip <- scan(skipFile, what=character())
  for(x in skip) {
    data <- data[!grepl(x, data$Dir, ignore.case = TRUE),]
  }
  data
}

# Apply a function on every duplicate file in the directory exists in data.frame.
# Params:
#   fullPath - directory (-ies)
#   data - data.frame with all files
#   func - function
#   left - if TRUE - left part of join - apply function on files in directory
#           if FALSE - apply on files from other directories
applyOnDuplicates <- function(fullPath, data, func, recursive=TRUE, left=TRUE){
  sapply(
    fullPath,
    function(path){
      if (recursive)
        idxLeft <- grepl(paste0("^", path, ".*"), data$Dir)
      else
        idxLeft <- data$Dir == path
      x <- inner_join(
        data[idxLeft, ],
        data[!idxLeft, ],
        by = c("FileName", "SizeMB")
      )
      if (left)
        files <- x$Name.x
      else
        files <- x$Name.y

      return (sapply(files, func))
    })
}

# helper function for removing a file with logging
rmFile <- function(file) {
  logFile <- paste0(format(Sys.time(), "%m%d_%H%M"), ".log")
  if (file.exists(file)) {
    if (file.remove(file)) {
      write(paste("DEL-OK", file), file=logFile, append=TRUE)
      return(TRUE)
    } else {
      write(paste("CAN'T-DEL", file), file=logFile, append=TRUE)
    }
  } else {
    write(paste("NOT-FOUND", file), file=logFile, append=TRUE)
  }
  return (FALSE)
}
