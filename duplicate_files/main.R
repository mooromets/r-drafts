require("lubridate")
require("dplyr")

df <- read.csv("./files.csv", as.is=TRUE, encoding = "cp1251")

# clean
# remove all small files
df <- df[!grepl("Bytes", df$Size),]
df <- df[!grepl("KB", df$Size),]

# remove system files
extList <- c("dll$", "exe$", "sys$", "tlb$", "admx$", "etl$", "cpl$", "mui$", "xml$", "sdi$")
for(ext in extList) {
  df <- df[!grepl(ext, df$Name, ignore.case = TRUE),]  
}

# !!! DROP already processed  folders
dropList <- c("F:\\\\video\\\\")
for(drop in dropList) {
  df <- df[!grepl(drop, df$Name, ignore.case = TRUE),]  
}

# add columns
df$FileName <- basename(df$Name)
df$Dir <- dirname(df$Name)
df$Modified <- dmy_hms(df$Date.Modified)
df$Created <- dmy_hms(df$Date.Created)
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

# EXPLORE

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

freq_dirs <- filter(df, n > 3) %>%
  group_by(Dir) %>%
  summarize(
    nFiles = n(),
    sumSize = sum(SizeMB)
  ) %>%
  arrange(desc(sumSize))

