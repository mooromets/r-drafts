source("./globals.R")

require("dplyr")
require("tools")

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

# list all files and their info in a directory recursively
# path - a directory
# oldFiles - data.frame from last scan (saves time for md5)
getAllFilesInfo <- function(path, oldFiles = NULL){
  Sys.setlocale("LC_CTYPE", SYS_LOCALE_LANG) #needed here ?
  #obtain info
  allFiles <- lapply(
    list.files(path = path, 
               include.dirs = FALSE, 
               recursive = TRUE, 
               full.names = TRUE), 
    FUN = file.info)
  
  tmpDF <- as.data.frame(do.call(rbind, allFiles))
  complIdx <- complete.cases(tmpDF)
  if(!all(complIdx)) {
    tmpDF <- tmpDF[complete.cases(tmpDF), ]
    warning(sprintf("couldn't read file info for %d files", sum(!complIdx)))
  }
  #rownames as s column
  tmpDF <- cbind(path = rownames(tmpDF), tmpDF)
  tmpDF$path <- as.character(tmpDF$path)
  #int row names
  rownames(tmpDF) <- 1:nrow(tmpDF)
  tmpDF$filename <- basename(tmpDF$path)
  #checksum
  if (!is.null(oldFiles)) {
    tmpDF$mtime <- as.character(tmpDF$mtime) # for joining on string time
    df_join <- left_join(tmpDF, oldFiles, by = c("path", "mtime", "size"))
    idxCalcMd5 <- is.na(df_join$md5)
    if (sum(idxCalcMd5) > 0) {
      tmpDF$md5[idxCalcMd5] <- get_md5_with_progress(tmpDF$path[idxCalcMd5])
    }
  } else
    tmpDF$md5 <- get_md5_with_progress(tmpDF$path)
  return (tmpDF)
}

get_md5_with_progress <- function (files){
  filesLists <- split(files, ceiling(seq_along(files)/length(files)*20))
  x <- c()
  for(i in 1:length(filesLists)) {
    x <- c(x, md5sum(filesLists[[i]]))
    if (sum(is.na(x)) > 0) {
      stop(sprintf("MD5 couldn't be calculated for %d files", sum(!is.na(x))))
    }
    print(sprintf("%s : %d of %d files done (%d %%)",
                  format(Sys.time(), "%X"),
                  length(x),
                  length(files),
                  i*5))
  }
  return(x)
}

#
storeLargeDirInfo <- function(path, oldFiles = NULL) {
  for (x in sample(path)) {
    print(x)
    print(Sys.time())
    df <- getAllFilesInfo(path = x, oldFiles)
    write.csv(
      df,
      paste0(basename(x), ".csv"),
      fileEncoding = ENCODING)
    print(Sys.time())
  }
}
