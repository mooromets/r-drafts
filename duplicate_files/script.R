source("./utils.R")
source("./globals.R")

dirNames <- c(
)

applyOnDuplicates(
  dirNames,
  df,
  rmFile
)


### obsolete ?

for (dirName in dirNames)
  cleanDir(dirName, df)

# append dirNames to the skipped-dirs file
# prorgammatically:
write(paste0("\"", dirNames, "$\""), append = TRUE, file = SKIP_DIR_FILE)
#OR manually

df <- dropRows(df)
freq_dirs <- dropRows(freq_dirs)
