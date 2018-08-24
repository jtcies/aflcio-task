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
  "ca_age", "sy_ideologymodel", "dem", "rep", "afl_mem",
  "gender_match", "sy_gunscore", "ca_race",
  # home characteristics
  "hh_white", "hh_ageavg", "hh_totalpersons", 
  # neighborhood characteristics
  "tract_pct_hsg_or_less", "dist03", "tract_pct_hispanic"
)

tuning_params <- expand.grid(
  eta = seq(0, 1, by = 0.2),
  nrounds = 20,
  max_depth = c(3, 6),
  gamma = seq(0, 1, by = 0.2),
  min_child_weight = seq(0, 1, by = 0.2),
  subsample = 0.5,
  colsample_bytree = 1
)

fit_control <- trainControl(
  method = "repeatedcv",
  number= 10,
  repeats = 10
)

set.seed(2018)
model <- train(
  factor(recall_binary) ~  .,
  data = train[model_params], 
  method = "xgbTree",
  tuneGrid = tuning_params,
  trControl = fit_control,
)

# key metrics 
model

preds_oppose <- predict(model, train, "prob")[, 2]
roc_plot(train$recall_binary, preds_oppose)

saveRDS(model, here::here("models/model.rds"))

ggplot(varImp(model))
