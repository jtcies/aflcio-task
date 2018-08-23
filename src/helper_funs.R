library(dplyr)
library(ggplot2)
library(pROC)

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

roc_plot <- function(actuals, predictions) {
  #function to plot roc and calculate auc
  # assign predictions based on changing cutoffs
  assign_pred <- function(actuals, predictions, cutoff) {
    predictions[predictions >= cutoff] <- 1
    predictions[predictions <  cutoff] <- 0
    data.frame(predictions, cutoff, actuals)
  }
  
  diff <- seq(0, 1, by = 0.01) %>%
    map_dfr(~assign_pred(actuals, predictions, .x))
  
  total_pos <- sum(actuals == 1)
  total_neg <- sum(actuals == 0)
  
  summary <- diff %>%
    group_by(cutoff) %>%
    summarise(
      tp = sum(predictions == 1 & actuals == 1),
      fp = sum(predictions == 1 & actuals == 0),
      fn = sum(predictions == 0 & actuals == 1)
    ) %>%
    mutate(
      tpr = tp / total_pos,
      fpr = fp / total_neg,
      prec = tp / (tp + fp),
      recall = tp / (tp + fn),
      f1 = (2 * (prec * recall)) / (prec + recall)
    )
  
  auc_label <- as.character(pROC::auc(actuals, predictions))
  max_cutoff <- summary[which.max(summary$f1), ][["cutoff"]]
  
  roc_plot <- ggplot(summary, aes(x = fpr, y = tpr)) +
    geom_line(size = 1.5) +
    lims(x = c(0, 1), y = c(0, 1)) +
    geom_text(aes(label = auc_label, x = .5, y = .5), inherit.aes = FALSE)
  
  cost_plot <- ggplot(summary, aes(x = cutoff, y = f1)) +
    geom_line(size = 1.5) +
    geom_vline(xintercept = max_cutoff, linetype = 2) +
    labs(caption = paste0("optimal cutoff:", max_cutoff))
  
  list(roc_plot, cost_plot, summary %>% as.data.frame())
}

rmse <- function(actual, pred) {
  
  sqrt(mean((pred - actual) ^ 2))
  
}