source(here::here("config.R"))

# Function to display missing values
missing_val <- function(df) {
  missing_tab <- df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "pct_missing"
    ) %>%
    arrange(desc(pct_missing))

  return(missing_tab %>% kable())
}

# Function to check multicollinearity
mc_check <- function(data) {
  vif_check <- lm(rainfall ~ ., data = data)
  test_collinearity <- check_collinearity(vif_check)
  return(test_collinearity)
}

# Function to select model features
select_model_features <- function(data, keep_location = TRUE) {
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

  if (!keep_location) {
    cols_to_drop <- c(cols_to_drop, "location")
  }

  cols_to_drop <- c(cols_to_drop)

  data <- data %>%
    select(-any_of(cols_to_drop)) %>%
    ungroup()

  return(data)
}

# Function to scale data
scale_data <- function(data) {
  df_scaled <- data %>%
    mutate(across(
      .cols = where(is.numeric) & !c("rainfall"),
      .fns = ~ as.numeric(scale(.x))
    ))

  return(df_scaled)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

stage_datasets <- function(datasets) {
  run_dir <- tempfile(pattern = "mi_pool_")
  dir.create(run_dir, recursive = TRUE)

  paths <- purrr::imap_chr(datasets, \(dat, i) {
    path <- file.path(run_dir, sprintf("imp_dataset_%02d.rds", as.integer(i)))
    saveRDS(dat, path, compress = FALSE)
    path
  })

  attr(paths, "run_dir") <- run_dir
  paths
}
