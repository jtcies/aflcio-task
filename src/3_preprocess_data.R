library(tidyverse)
library(tidycensus)
library(stringr)

source(here::here("src/helper_funs.R"))

# this script:
# 1. incorporates external data sources
# 2. combines and merges the data into a single data set for modeling
# 3. recodes variables to prepare for modeling
# 4. splits data into test and train

# import -----------------------------

files <- list.files(here::here("data/raw"))

files %>% 
  set_names() %>% 
  map(
    ~ read_tsv(
      here::here("data/raw/", .)
    )
  ) %>% 
  fix_env_names() %>% 
  list2env(envir = .GlobalEnv)

# preprocess -------------------------

# add variable for district
resp_03$district <- "03"
resp_11$district <- "11"
vf_03$district <- "03"
vf_11$district <- "11"

# merge files
resp <- bind_rows(resp_03, resp_11)
vf <- bind_rows(vf_03, vf_11)

# add in membership to voterfile
vf <- left_join(vf, members, by = c("state", "test_id"))

vf <- vf %>% 
  mutate(
    afl_mem = if_else(!is.na(member_type_status), 1L, 0L)
  )

# create modeling data  ----------------------

# merge thsoe we have repsones from for modeling
model_dat <- inner_join(resp, vf, by = c("state", "test_id", "district")) 

set.seed(2018)
# 80/20 split
split <- sample(c(0,1), nrow(model_dat), replace = TRUE, prob = c(0.8, 0.2))
train <- model_dat[split == 0, ]
test <- model_dat[split == 1, ]

# write ----------------

write_csv(resp, here::here("data/processed/all_responses.csv"))
write_csv(vf, here::here("data/processed/voterfile_combined.csv"))
write_csv(train, here::here("data/processed/training_data.csv"))
write_csv(test, here::here("data/processed/testing_data.csv"))
