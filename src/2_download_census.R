library(tidyverse)
library(tidycensus)

get_acs(
  year = 2013,
  state = "CO", 
  geography = "tract",
  variables = "",
  geometry = TRUE
)