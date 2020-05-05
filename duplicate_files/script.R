require("./utils.R")

dirNames <- c(
  
)

for (dirName in dirNames)
  cleanDir(dirName, df)

# append dirNames to "skip.txt"
# prorgammatically:
write(paste0("\"", dirNames, "$\""), append = TRUE, file = "skip.txt")
#OR manually

df <- dropRows(df)
freq_dirs <- dropRows(freq_dirs)
