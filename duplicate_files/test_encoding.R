
#prepare

path = "E:/BACK_UP/keep_as_is_2020/"

df_read <- function(){
  as.data.frame(
    do.call(
      rbind,
      lapply(
        list.files(path = path, 
                   include.dirs = FALSE, 
                   recursive = TRUE, 
                   full.names = TRUE), 
        FUN = file.info)
    ))  
}

old_loc_LC <- Sys.getlocale("LC_CTYPE")

# start

# here enconding is wrong
df <- df_read()

Sys.setlocale("LC_CTYPE", "russian")

# data frame is OK
df <- df_read()

# store with default encoding
write.csv(df, "enc.csv")

# encoding is OK
df <- read.csv("enc.csv")

# UTF-8
# store with UTF-8
write.csv(df, "enc_err.csv", fileEncoding = "UTF-8")
# read with UTF-8
df <- read.csv("enc_err.csv", encoding = "UTF-8")
