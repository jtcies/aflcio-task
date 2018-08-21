library(tidyverse)
library(googledrive)
library(here)

# download the data for the task from google drive
# copied to my personal google drive folder

files <- drive_ls("AFL-CIO/Test Materials", pattern = ".txt")

files$name %>% 
  map(
    ~drive_download(
      paste0("AFL-CIO/Test Materials/", .), 
      path = here::here("data/raw", .),
      overwrite = TRUE)
  )
