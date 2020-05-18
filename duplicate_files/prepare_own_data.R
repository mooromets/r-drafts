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

# create frequecy table
df_freq <-
  group_by(df, md5, size) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

#amount of space wasted (MB)
(sum(df_freq$size*(df_freq$n-1)))

# update freq column
df <- left_join(df,
                df_freq[,c("md5","n")],
                by = c("md5"))

freq_dirs <- filter(df, n > 1) %>%
  group_by(dirname) %>%
  summarize(
    nFiles = n(),
    sumSize = sum(size)
  ) %>%
  arrange(desc(sumSize))

