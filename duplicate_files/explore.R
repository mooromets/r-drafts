source("./utils.R")
source("./globals.R")

# FIND duplicate folders candidates

# strict mode:
# two folder names match exactly
freq_dirs <- dropRows(freq_dirs)
u_dirs <- unique(freq_dirs$dirName)
x <- lapply(u_dirs,
            function(dir){
              idx <- c(freq_dirs$dirName == dir)
              if (sum(idx) > 1)
                cbind(dirName = dir, freq_dirs[idx, 1:3])
            }) %>% bind_rows()

# moderate mode:
# a folder name is a substring of another folder
freq_dirs <- dropRows(freq_dirs)
u_dirs <- unique(freq_dirs$dirName)
x <- lapply(u_dirs,
            function(dir){
              idx <- grep(dir, freq_dirs$dirName)
              if (length(idx) > 1)
                cbind(dirName = dir, freq_dirs[idx, 1:3])
            }) %>% bind_rows()

# soft mode:
# a folder name exists in the path of another dir
freq_dirs <- dropRows(freq_dirs)
u_dirs <- unique(freq_dirs$dirName)
x <- lapply(u_dirs,
            function(dir){
              idx <- grep(dir, freq_dirs$Dir)
              if (length(idx) > 1)
                cbind(dirName = dir, freq_dirs[idx, 1:3])
            }) %>% bind_rows()
