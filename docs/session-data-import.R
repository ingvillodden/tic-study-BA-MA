
# Making a file combining all the session data from individual files to one file

## Loading needed packages
library(tidyverse)
library(readxl)

# List all files in the data folder
files <- list.files("data/session-data/")

# Make a list to store imports
file.list <- list()

# Loop over the list of files and read individual files
for(i in 1:length(files)) {
  
 file.list[[i]] <- read_excel(paste0("data/session-data/", files[i]), 
                              na = "na")

}

# Bind all files together
dat <- bind_rows(file.list)


## Save imported data 
saveRDS(dat, "data/derivedData/session-data.RDS")


