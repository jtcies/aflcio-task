library(readr)
library(here)
library(pROC)
library(purrr)
library(dplyr)
library(ggplot2)
library(caret)
library(broom)

source(here::here("src/helper_funs.R"))

test <- "data/processed/testing_data.csv" %>% 
  here::here() %>% 
  read_csv() %>% 
  filter(!is.na(recall_binary))

model <- read_rds(here::here("models/model.rds"))

# preds_oppose <- predict(model, test, "prob")[, 2]
preds_oppose <- predict(model, test, "response")
roc_plot(test$recall_binary, preds_oppose)


