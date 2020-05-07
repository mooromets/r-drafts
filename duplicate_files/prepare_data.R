source("./globals.R")
source("./utils.R")

require("lubridate")
require("dplyr")

Sys.setlocale("LC_CTYPE", SYS_LOCALE_LANG)

df <- read.csv(INPUT_DATA_FILE, as.is=TRUE, encoding = ENCODING)

# clean
# remove all small files
df <- df[!grepl("Bytes", df$Size),]
#df <- df[!grepl("KB", df$Size),]

# remove system files
extList <- c("dll$", "exe$", "sys$", "tlb$", "admx$", "etl$", "cpl$", "mui$", 
             "xml$", "sdi$", ".db$", ".info$")
for(ext in extList) {
  df <- df[!grepl(ext, df$Name, ignore.case = TRUE),]  
}

#drop dirs not existing anymore
#df <- dropRows(df)

# add columns
df$FileName <- basename(df$Name)
df$Dir <- dirname(df$Name)
df$Modified <- dmy_hms(df$Date.Modified)
df$Created <- dmy_hms(df$Date.Created)
df$SizeMB[grepl("KB", df$Size)] <- 
  round(
    as.numeric( #a numeric is at the begining of the string
      sapply(
        strsplit( 
          sub(",", #remove comma in a number 
              "", 
              df$Size[grepl("KB", df$Size)]), #data with 'MB' string
          " "), 
        "[[", 
        1)) / 1024,
  3)

df$SizeMB[grepl("MB", df$Size)] <- 
  as.numeric( #a numeric is at the begining of the string
    sapply(
      strsplit( 
        sub(",", #remove comma in a number 
            "", 
            df$Size[grepl("MB", df$Size)]), #data with 'MB' string
        " "), 
      "[[", 
      1))
df$SizeMB[grepl("GB", df$Size)] <- 
  1024 * as.numeric(
    sapply(
      strsplit(
        df$Size[grepl("GB", df$Size)], 
        " "), 
      "[[", 
      1))

# drop some columns
df <- dplyr::select(df, -c(Date.Modified, Date.Created, Size))

# update dir name column
df$dirName <- gsub("^.*/", "", df$Dir)

# create frequecy table
df_freq <-
  group_by(df, FileName, SizeMB) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

#amount of space wasted (MB)
(sum(df_freq$SizeMB*(df_freq$n-1)))

# update freq column
df <- left_join(df,
          df_freq,
          by = c("FileName", "SizeMB"))

freq_dirs <- filter(df, n > 1) %>%
  group_by(Dir) %>%
  summarize(
    nFiles = n(),
    sumSize = sum(SizeMB)
  ) %>%
  arrange(desc(sumSize))

# update dir name column
freq_dirs$dirName <- gsub("^.*/", "", freq_dirs$Dir)
