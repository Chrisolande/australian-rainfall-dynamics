# eval_autocorrelation.R
# Temporal autocorrelation diagnostics across all locations.
# build_autocorrelation_data() - joins dates back from imp_mids to engineered_list
# run_dw_tests() - parallel Durbin-Watson tests across all locations

build_autocorrelation_data <- function(
  imp = imp_mids,
  eng_list = engineered_list
) {
  dates <- complete(imp, 1) %>%
    select(location, date) %>%
    group_by(location) %>%
    mutate(row_within_loc = row_number())

  eng_list[[1]] %>%
    group_by(location) %>%
    mutate(row_within_loc = row_number()) %>%
    left_join(dates, by = c("location", "row_within_loc"))
}

run_dw_tests <- function(full_data, m_full, res, workers = 4) {
  rows_used <- as.numeric(rownames(m_full$frame))
  model_data <- full_data[rows_used, ]
  loc_indices <- split(seq_len(nrow(model_data)), model_data$location)

  plan(multisession, workers = workers)
  on.exit(plan(sequential))

  future_map(
    names(loc_indices),
    function(loc) {
      idx <- loc_indices[[loc]]
      loc_rows <- model_data[idx, ] %>% arrange(date)

      recalculated <- recalculateResiduals(res, sel = idx)

      dw <- testTemporalAutocorrelation(
        simulationOutput = recalculated,
        time = loc_rows$date,
        plot = FALSE
      )

      tibble(location = loc, dw_stat = dw$statistic, p_value = dw$p.value)
    },
    .options = furrr_options(seed = TRUE)
  ) %>%
    list_rbind() %>%
    arrange(p_value)
}
run_dw_tests <- function(
  full_data,
  m_full,
  res,
  workers = 4,
  save_path = here::here("data", "dw_results.csv")
) {
  rows_used <- as.numeric(rownames(m_full$frame))
  model_data <- full_data[rows_used, ]
  loc_indices <- split(seq_len(nrow(model_data)), model_data$location)

  plan(multisession, workers = workers)
  on.exit(plan(sequential))

  results <- future_map(
    names(loc_indices),
    function(loc) {
      idx <- loc_indices[[loc]]
      loc_rows <- model_data[idx, ] %>% arrange(date)

      recalculated <- recalculateResiduals(res, sel = idx)

      dw <- testTemporalAutocorrelation(
        simulationOutput = recalculated,
        time = loc_rows$date,
        plot = FALSE
      )

      tibble(location = loc, dw_stat = dw$statistic, p_value = dw$p.value)
    },
    .options = furrr_options(seed = TRUE)
  ) %>%
    list_rbind() %>%
    arrange(p_value)

  write_csv(results, save_path)
  results
}
