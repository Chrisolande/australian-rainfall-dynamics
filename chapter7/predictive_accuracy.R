# predictive_accuracy.R
# Predictive accuracy and posterior predictive check:
# render_error_metrics() - MAE/RMSE tables (positive-only and global)
# build_ppc_data() - simulates 50 datasets from fitted model
# plot_ppc() - density overlay of observed vs simulated

render_error_metrics <- function(model = m6_mixed, eng_list = engineered_list) {
  pred_rainfall <- map2(
    model$good,
    eng_list[model$good_idx],
    \(r, data) predict(r$fit, newdata = data, type = "response")
  ) %>%
    reduce(`+`) /
    length(model$good)

  results <- tibble(
    actual = eng_list[[1]]$rainfall,
    predicted = pred_rainfall
  )

  print(
    results %>%
      filter(actual > 0) %>%
      yardstick::metrics(truth = actual, estimate = predicted) %>%
      filter(.metric %in% c("rmse", "mae")) %>%
      mutate(.estimate = round(.estimate, 3)) %>%
      select(.metric, .estimate)
  )

  print(
    results %>%
      yardstick::metrics(truth = actual, estimate = predicted) %>%
      filter(.metric %in% c("rmse", "mae")) %>%
      mutate(.estimate = round(.estimate, 3)) %>%
      select(.metric, .estimate)
  )

  evict_model("m6_mixed")
}

build_ppc_data <- function(
  m_full,
  eng_list = engineered_list,
  nsim = 1000,
  n_show = 50
) {
  set.seed(123)
  sims <- simulate(m_full, nsim = nsim)
  subset_sims <- sims[, sample(ncol(sims), n_show)]

  bind_cols(
    Observed = eng_list[[1]]$rainfall,
    subset_sims
  ) %>%
    tidyr::pivot_longer(
      cols = -Observed,
      names_to = "Simulation",
      values_to = "Simulated_Value"
    )
}

plot_ppc <- function(ppc_dat, eng_list = engineered_list) {
  ggplot() +
    geom_density(
      data = ppc_dat,
      aes(x = Simulated_Value, group = Simulation),
      color = "gray70",
      size = 0.5,
      alpha = 0.5
    ) +
    geom_density(
      data = eng_list[[1]],
      aes(x = rainfall),
      color = "#0072B2",
      size = 1.2
    ) +
    coord_cartesian(xlim = c(-1, 50)) +
    labs(
      title = "Posterior Predictive Check",
      subtitle = "Grey lines: 50 datasets simulated from the fitted model. Blue line: observed data.",
      x = "Rainfall (mm)",
      y = "Density"
    )
}
