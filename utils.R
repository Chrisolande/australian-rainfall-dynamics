missing_val <- function(df) {
  missing_tab <- df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "pct_missing"
    ) %>%
    arrange(desc(pct_missing))

  return(
    missing_tab %>% kable(caption = "Percentage of Missing Values by Feature")
  )
}

# multicollinearity via Variance Inflation Factor (VIF)
mc_check <- function(data) {
  vif_check <- lm(rainfall ~ ., data = data)
  test_collinearity <- check_collinearity(vif_check)
  return(test_collinearity)
}

# execute feature selection and dimensionality reduction
select_model_features <- function(data, keep_location = TRUE) {
  # Define list of redundant or highly correlated features to exclude
  cols_to_drop = c(
    "month",
    "day",
    "day_of_year",
    "date",
    "temp9am",
    "temp3pm",
    "min_temp",
    "max_temp",
    "pressure3pm",
    "pressure9am",
    "cloud3pm",
    "cloud9am",
    "dewpoint_3pm",
    "wind_dir3pm",
    "wind_speed3pm",
    "wind_gust_dir",
    "wind_gust_speed",
    "wind_speed9am",
    "wind_dir9am",
    "wind9am_rad",
    "gust_rad",
    "moisture_index",
    "rain_today"
  )

  # Conditionally drop location if analyzing aggregate data
  if (!keep_location) {
    cols_to_drop <- c(cols_to_drop, "location")
  }

  # Remove imputation flags if present
  imp_flags <- names(data)[grepl("_imp_flagged$", names(data))]
  cols_to_drop <- c(cols_to_drop, imp_flags)

  data <- data %>%
    select(-any_of(cols_to_drop)) %>%
    ungroup()

  return(data)
}

# standardize numeric predictors (Z-score scaling)
scale_data <- function(data) {
  data %>%
    mutate(across(
      .cols = where(is.numeric) & !any_of("rainfall"),
      .fns = \(x) as.numeric(scale(x))
    ))
}
