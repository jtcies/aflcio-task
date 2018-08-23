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

set.seed(2018)
model <- glm(
  factor(recall_binary) ~ 
    ca_age + 
    sy_ideologymodel +
    party,
  data = train,
  family = "binomial"
)

# key metrics 
summary(model)

preds <- predict(model, train, type = "response")

roc_plot(train$recall_binary, preds)

saveRDS(model, here::here("models/model.rds"))