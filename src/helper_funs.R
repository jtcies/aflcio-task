# helper functions used across multiple scripts

# fixes object names to remove file type and path
# make them easier to work with
fix_env_names <- function(x) {
  names(x) <- str_extract(names(x), "[^/]+$")
  names(x) <- str_replace(names(x), "\\.txt", "")
  names(x) <- str_replace(names(x), "\\.rds", "")
  names(x) <- str_replace(names(x), "CO_membership", "members")
  names(x) <- str_replace(names(x), "CO_SD_03_voterfile", "vf_03")
  names(x) <- str_replace(names(x), "CO_SD_11_voterfile", "vf_11")
  names(x) <- str_replace(names(x), "CO03_response_data", "resp_03")
  names(x) <- str_replace(names(x), "CO11_response_data", "resp_11")
  x
}