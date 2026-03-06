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
    "rain_today",
    "sunshine_imp_flagged",
    "evap_imp_flagged",
    "cloud3pm_imp_flagged",
    "cloud9am_imp_flagged"
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

# %%
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
# %%
# retrieve the one full-frame model kept for diagnostics.
# Throw a clear error if unavailable (eg model 1 failed in parallel mode)
# rather than letting DHARMa crash ".
get_diagnostic_model <- function(mixed_result) {
  full_fit <- mixed_result$good[[1]]$fit
  if (is.null(full_fit$frame) || nrow(full_fit$frame) == 0L) {
    stop(
      "No full-frame model available for diagnostics.\n",
      "This happens in parallel mode when the first imputation failed to converge.\n",
      "Re-run with parallel = FALSE, or manually refit one imputation with strip = FALSE.",
      call. = FALSE
    )
  }
  full_fit
}
# %%

# lazy model loading
model_cache <- new.env(parent = emptyenv())

get_model <- function(nm) {
  if (!exists(nm, envir = model_cache, inherits = FALSE)) {
    assign(
      nm,
      readRDS(here::here("models", paste0(nm, ".rds"))),
      envir = model_cache
    )
  }
  get(nm, envir = model_cache, inherits = FALSE)
}

evict_model <- function(...) {
  nms <- c(...)
  for (nm in nms) {
    if (exists(nm, envir = model_cache, inherits = FALSE)) {
      rm(list = nm, envir = model_cache)
    }
  }
  gc()
}
