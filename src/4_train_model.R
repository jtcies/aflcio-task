library(readr)
library(here)
library(pROC)
library(purrr)
library(dplyr)
library(ggplot2)
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
  "ca_age", "sy_ideologymodel", "afl_mem",
  "gender_match", "sy_gunscore", "ca_white", "party",
  # home characteristics
  "hh_white", "hh_ageavg", "hh_totalpersons", 
  # neighborhood characteristics
  "tract_pct_hsg_or_less", "dist03", "tract_pct_hispanic"
)

model <- glm(
  factor(recall_binary) ~ .,
  data = train[model_params],
  family = "binomial"
)

# key metrics 
model
# 
# preds_oppose <- predict(model, train, "prob")[, 2]
preds_oppose <- predict(model, train, "response")
roc_plot(train$recall_binary, preds_oppose)


# naiive model based just on party

naive_model <- glm(
  factor(recall_binary) ~ party, 
  data = train,
  family = "binomial"
)

naive_pred <- predict(naive_model, train, "response")
roc_plot(train$recall_binary, naive_pred)


saveRDS(model, here::here("models/model.rds"))
saveRDS(naive_model, here::here("models/naive_model.rds"))
