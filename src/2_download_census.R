library(tidyverse)
library(tidycensus)
library(units)

# download data from the acs 2013 5-year estimates for race, education,
# and income

# functions ----------------
download_acs <- function(vars, summary = NULL, geo, geometry = TRUE) {
  get_acs(
    year = 2013,
    state = "CO", 
    geography = "tract",
    variables = vars,
    summary_var = summary,
    geometry = geometry
  )
}

# variables ------------------

race_vars <- c(
  white = "B02001_002",
  black  = "B02001_003", 
  native_american = "B02001_004",
  asian = "B02001_005",
  pacific_islander = "B02001_006",
  other_race = "B02001_007",
  two_or_more_races = "B02001_008"
)
  
race_summary <- "B02001_001"

hispanic_var <- c(hispanic = "B03001_002")

hispanic_summary <- "B03001_001"

educ_vars <- c(
  less_than_hsg = "B16010_002",
  hsg = "B16010_015",
  some_college = "B16010_028",
  bach_or_higher = "B16010_041"
)

educ_summary <- "B16010_001"

# white educational attainment per request

male_educ_vars <- c(
  less_than_hsg = "C15002A_003",
  hsg_male = "C15002A_004",
  some_college = "C15002A_005",
  bach_or_higher = "C15002A_006"
)

white_male_summary <- "C15002A_002"

female_educ_vars <- c(
  less_than_hsg = "C15002A_008",
  hsg_male = "C15002A_009",
  some_college = "C15002A_010",
  bach_or_higher = "C15002A_011"
) 

white_female_summary <- "C15002A_007"

income_vars <- c(
 med_hh_inc = "B19013_001" 
)

total_pop <- "B00001_001"

# download -------------------------

race_tract <- download_acs(race_vars, race_summary, "tract")
race_county <-  download_acs(race_vars, race_summary, "county")
hispanic_tract <- download_acs(hispanic_var, hispanic_summary, "tract")
hispanic_county <- download_acs(hispanic_var, hispanic_summary, "county")

educ_tract <- download_acs(educ_vars, educ_summary, "tract")
educ_county <- download_acs(educ_vars, educ_summary, "county")

income_tract <- download_acs(income_vars, geo = "tract")
income_county <- download_acs(income_vars, geo = "county")

white_educ_male <- download_acs(
  male_educ_vars, white_male_summary, "county", geometry = FALSE
) %>% 
  mutate(gender = "male")

white_educ_female <- download_acs(
  female_educ_vars, white_female_summary, "county", geometry = FALSE
) %>% 
  mutate(gender = "female")

white_educ_county <- bind_rows(white_educ_male, white_educ_female)

pop_tract <- download_acs(total_pop, geo = "tract", geometry = TRUE)

# determine population density for each census tract

pop_tract$area <- set_units(st_area(pop_tract$geometry), "km^2")

pop_tract$pop_density <- pop_tract$estimate / pop_tract$area

# write ------------------------
# list the data frames and write recursively
dfs <- mget(ls(pattern = "tract|county"))

# write as rds to perserve geometry
dfs %>% 
  iwalk(~write_rds(.x, here::here("data/acs", paste0(.y, ".rds"))))


