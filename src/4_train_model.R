library(readr)
library(here)
library(pROC)
library(purrr)
library(dplyr)
library(ggplot2)
library(caret)
library(broom)

source(here::here("src/helper_funs.R"))

train <- "data/processed/training_data.csv" %>% 
  here::here() %>% 
  read_csv() %>% 
  filter(!is.na(recall_binary))

# train model
model_params <- c(
  # outcome
  "recall_binary",
  # personal characteristics
  "ca_age", "sy_ideologymodel", "dem", "afl_mem", "ca_white", 
  "ca_male",
  # home characteristics
  "hh_partisanbehave",
  # neighborhood characteristics
  "tract_pct_hispanic", "tract_pct_hsg_or_less", "dist03",
  "med_hh_inc"
)

set.seed(2018)
model <- glm(
  factor(recall_binary) ~ .,
  data = train[model_params],
  family = "binomial"
)

# key metrics 
summary(model)

# preds <- predict(model, train, type = "response")
# 
# roc_plot(train$recall_binary, preds)
# 
# saveRDS(model, here::here("models/model.rds"))