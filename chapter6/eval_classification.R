# eval_classification.R
# Occurrence submodel classification performance:
# build_predictions() - pools zero-inflation probs across imputations
# plot_roc() - ROC curve with AUC annotation
# plot_confusion_matrix() - heatmap at Youden-optimal threshold
# render_brier_scores() - Brier score and Brier Skill Score

build_predictions <- function(model = m6_mixed, eng_list = engineered_list) {
  prob_no_rain <- map2(
    model$good,
    eng_list[model$good_idx],
    \(r, data) predict(r$fit, newdata = data, type = "zprob")
  ) %>%
    reduce(`+`) /
    length(model$good)

  tibble(
    prob_no_rain = prob_no_rain,
    actual_class = factor(
      ifelse(eng_list[[1]]$rainfall == 0, "No Rain", "Rain"),
      levels = c("No Rain", "Rain")
    )
  )
}

.get_optimal_threshold <- function(preds) {
  roc_curve(
    preds,
    truth = actual_class,
    prob_no_rain,
    event_level = "first"
  ) %>%
    mutate(j = sensitivity + specificity - 1) %>% # youden's j
    slice_max(j, n = 1) %>%
    pull(.threshold)
}

.with_predicted_class <- function(
  preds,
  threshold = .get_optimal_threshold(preds)
) {
  preds %>%
    mutate(
      predicted_class = factor(
        ifelse(prob_no_rain > threshold, "No Rain", "Rain"),
        levels = c("No Rain", "Rain")
      )
    )
}

plot_roc <- function(preds) {
  auc_val <- roc_auc(
    preds,
    truth = actual_class,
    prob_no_rain,
    event_level = "first"
  )$.estimate

  roc_curve(
    preds,
    truth = actual_class,
    prob_no_rain,
    event_level = "first"
  ) %>%
    autoplot() +
    labs(
      title = "ROC Curve: Predicting Rainfall Occurrence",
      subtitle = sprintf("AUC = %.3f", auc_val)
    )
}

plot_confusion_matrix <- function(preds) {
  threshold <- .get_optimal_threshold(preds)

  .with_predicted_class(preds, threshold) %>%
    conf_mat(truth = actual_class, estimate = predicted_class) %>%
    autoplot(type = "heatmap") +
    scale_fill_viridis() +
    labs(
      title = "Confusion Matrix: Predicting Rainfall Occurrence",
      subtitle = sprintf("Youden-optimal threshold = %.4f", threshold)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 11, face = "bold"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, colour = "grey40"),
      panel.grid = element_blank()
    )
}
render_brier_scores <- function(preds) {
  actual_bin <- as.integer(preds$actual_class == "No Rain")

  brier_score <- mean((preds$prob_no_rain - actual_bin)^2)
  baseline <- mean(actual_bin)
  bss <- 1 - brier_score / mean((baseline - actual_bin)^2)

  cat("Brier Score: ", round(brier_score, 4), "\n")
  cat("Brier Skill Score:", round(bss, 4), "\n")
}
