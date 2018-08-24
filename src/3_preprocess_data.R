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
    rename(census_fips = GEOID)
  
  if(any(grepl("summary_est", names(dat)))) {
    # for summary statistics
    dat %>% 
      mutate(pct = estimate / summary_est) %>% 
      select(census_fips, variable, pct) %>% 
      spread(variable, pct)
    
  } else {
    # for income
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
hispanic <- process_acs(hispanic_tract)
income <- process_acs(income_tract)
population <- process_acs(pop_tract)



# find a matching fips for those that miss it based on acs data 
# already in the vf
complete_fips <- vf %>% 
  select(census_fips, starts_with("ac")) %>% 
  filter(nchar(census_fips) == 11) %>% 
  distinct()

unknown_fips <- vf %>%   
  filter(nchar(census_fips) < 11) %>% 
  select(starts_with("ac")) %>% 
  distinct()

# there are only two sets of unique ac data for the missing fips
# find the fips that match on all ac vars 
replacement_fips <- complete_fips %>% 
  semi_join(unknown_fips) %>% # using all ac variables
  count(census_fips, sort = TRUE) %>% 
  # those that only matched on set of vars
  filter(n == 1) %>% 
  left_join(complete_fips %>% select(census_fips, ac_pctwhite)) %>% 
  filter(ac_pctwhite %in% unknown_fips$ac_pctwhite) %>% 
  distinct(ac_pctwhite, .keep_all = TRUE) 

# merge in replacements and coalesce
replacement_fips_all_vars <- complete_fips %>% 
  filter(census_fips %in% replacement_fips$census_fips,
         ac_pctwhite %in% replacement_fips$ac_pctwhite) %>% 
  distinct(census_fips, .keep_all = TRUE)

join_vars <- names(replacement_fips_all_vars[-1])

vf <- vf %>% 
  # merge on acs data
  left_join(replacement_fips_all_vars, by = c(join_vars)) %>% 
  mutate(
    census_fips.x = if_else(
      nchar(census_fips.x) < 11, NA_character_, census_fips.x
    ),
    census_fips = coalesce(census_fips.x, census_fips.y)
  ) %>% 
  select(-ends_with('x'), -ends_with(".y"))

# join in cnesus data
vf <- vf %>% 
  left_join(educ, by = "census_fips") %>% 
  left_join(race, by = "census_fips") %>% 
  left_join(income, by = "census_fips") %>% 
  left_join(hispanic, by = "census_fips") %>% 
  left_join(population, by = "census_fips")


# transfrm some variables

vf <- vf %>% 
  mutate(
    inc_change = med_hh_inc - ac_medianhhincome,
    party = case_when(
      ca_partyaffiliation == "DEM" ~ "DEM",
      ca_partyaffiliation == "REP" ~ "REP",
      ca_partyaffiliation == "NPA" ~ "none",
      TRUE ~ "other"
    ),
    dem = if_else(party == "DEM", 1L, 0L),
    tract_hispanic_times_inc = hispanic * med_hh_inc,
    tract_pct_hsg_or_less = less_than_hsg + hsg,
    dist03 = if_else(district == "03", 1L, 0L),
    ca_white = if_else(ca_race == "C", 1L, 0L),
    ca_male = if_else(ca_gender == "M", 1L, 0L)
  ) %>% 
  rename(tract_pct_hispanic = hispanic)

# create modeling data  ----------------------

# merge thsoe we have repsones from for modeling
model_dat <- inner_join(resp, vf, by = c("state", "test_id", "district")) %>% 
  mutate(
    recall_prob = case_when(
      recall_vote == "Yes" ~ 0,
      recall_vote == "No" ~ 1,
      recall_vote == "DK" ~ 0.5
    ),
    recall_binary = case_when(
      recall_vote == "DK" ~ NA_integer_,
      TRUE ~ as.integer(recall_prob)
    )
  )

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
