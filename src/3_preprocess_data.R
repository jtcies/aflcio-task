library(tidyverse)
library(tidycensus)
library(stringr)
library(sf)

source(here::here("src/helper_funs.R"))

# this script:
# 1. incorporates external data sources
# 2. combines and merges the data into a single data set for modeling
# 3. recodes variables to prepare for modeling
# 4. splits data into test and train

# functions ------------------------

process_acs <- function(dat) {
  
  dat <- dat %>% 
    st_set_geometry(NULL) %>% 
    as_data_frame() %>% 
    rename(census_fips = GEOID)
  
  if(any(grepl("summary_est", names(dat)))) {
    
    dat %>% 
      mutate(pct = estimate / summary_est) %>% 
      select(census_fips, variable, pct) %>% 
      spread(variable, pct)
    
  } else {
    
    dat %>% 
      select(census_fips, variable, estimate) %>% 
      spread(variable, estimate)
  }
}

# import -----------------------------

files <- list.files(here::here("data/raw"), pattern = "*.txt")

files %>% 
  set_names() %>% 
  map(
    ~ read_tsv(
      here::here("data/raw/", .)
    )
  ) %>% 
  fix_env_names() %>% 
  list2env(envir = .GlobalEnv)

acs_files <- list.files(here::here("data/acs"), pattern = "*tract.rds")

acs_files %>% 
  set_names() %>% 
  map(
    ~ read_rds(
      here::here("data/acs/", .)
    )
  ) %>% 
  fix_env_names() %>% 
  list2env(envir = .GlobalEnv)

pop_density <- "data/external/Population_Density_Census_Tracts.csv" %>% 
  here::here() %>% 
  read_csv() %>% 
  select(
    census_fips = Tract_FIPS,
    pop_sqmi = Population_Density_PerLandSquareMile
  ) %>% 
  mutate(census_fips = as.character(census_fips))

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

# add in acs data
educ <- process_acs(educ_tract)
race <- process_acs(race_tract)
income <- process_acs(income_tract)

vf <- vf %>% 
  left_join(educ, by = "census_fips") %>% 
  left_join(race, by = "census_fips") %>% 
  left_join(income, by = "census_fips") %>% 
  left_join(pop_density, by = "census_fips")

# impute missing income data because of fips
# want to use income change in the final model, will use more recent
# versions of others

acs_income <- vf %>% 
  select(census_fips, ac_medianhhincome, med_hh_inc) %>% 
  filter(!is.na(med_hh_inc)) %>% 
  distinct()

inc_model <- lm(med_hh_inc ~ ac_medianhhincome, data = acs_income)

missing_fips <- is.na(vf$med_hh_inc)

vf[missing_fips, ] <- predict(inc_model, vf[missing_fips, ])

# transfrm some variables

vf <- vf %>% 
  mutate(
    inc_change = med_hh_inc - ac_medianhhincome
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
