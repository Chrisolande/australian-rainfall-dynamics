librarian::shelf(tidyverse, kableExtra, performance)

missing_val <- function(df) {
  missing_tab <- df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "pct_missing"
    ) %>%
    arrange(desc(pct_missing))

  missing_tab %>%
    kable(
      caption = "Percentage of Missing Values by Feature",
      digits = 2,
      col.names = c("Feature", "Missing (%)"),
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

mc_check <- function(data) {
  vif_fit <- lm(rainfall ~ ., data = data)
  check_collinearity(vif_fit)
}

select_model_features <- function(data, keep_location = TRUE) {
  cols_to_drop <- c(
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

  if (!keep_location) {
    cols_to_drop <- c(cols_to_drop, "location")
  }

  result <- data %>%
    ungroup() %>%
    select(-any_of(cols_to_drop))

  result
}

scale_data <- function(data) {
  data %>%
    mutate(across(
      .cols = where(is.numeric) & !any_of("rainfall"),
      .fns = ~ as.numeric(scale(.))
    ))
}
