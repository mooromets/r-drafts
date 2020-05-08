source("./utils.R")
source("./globals.R")


#delete all duplicates from THESE dirs 
applyOnDuplicates(
  grep("4_no_name_2020", freq_dirs$Dir, value = TRUE),
  df,
  rmFile
)

applyOnDuplicates(
  grep("3_sorted_no_name_2020", freq_dirs$Dir, value = TRUE),
  df,
  rmFile
)

applyOnDuplicates(
  grep("2_unsorted_2020", freq_dirs$Dir, value = TRUE),
  df,
  rmFile
)

#delete all duplicates from OTHER dirs (and keep these)
applyOnDuplicates(
  grep("1_sorted_2020", freq_dirs$Dir, value = TRUE),
  df,
  rmFile,
  left = FALSE
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
