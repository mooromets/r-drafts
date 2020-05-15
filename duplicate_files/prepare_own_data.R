source("./globals.R")
source("./utils.R")

require("lubridate")
require("dplyr")

DEF_PATH = "full-list/"
DEF_DATA = "all-files.csv"
Sys.setlocale("LC_CTYPE", SYS_LOCALE_LANG)

# read raw files
df <- lapply(
  list.files(DEF_PATH, full.names = TRUE), 
  function(x) {
    read.csv(x, encoding = ENCODING)
  }
) %>% bind_rows()

# clean
df$dirname <- dirname(df$path)
df <- dplyr::select(df, -c(atime, isdir, mode, exe, X))

# save clean data
write.csv(df, DEF_DATA, fileEncoding = ENCODING)

