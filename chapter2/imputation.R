# data_prep_imputation.R
# Full two-stage imputation pipeline.
# Stage 1 - temporal interpolation for smooth met variables.
# Stage 2 - MICE (PMM + Random Forest) for structured missingness.
#
# Public entry point: clean_and_impute_weather(df)
# Returns a mids object; save with saveRDS().

# cleaning

clean_weather <- function(df) {
  df %>%
    clean_names() %>%
    mutate(
      date = as.Date(date),
      month = as.factor(month(date)),
      day = as.factor(wday(date, label = TRUE)),
      day_of_year = yday(date),
      wind_gust_dir = as.factor(wind_gust_dir),
      wind_dir9am = as.factor(wind_dir9am),
      wind_dir3pm = as.factor(wind_dir3pm),
      rain_today = as.factor(rain_today),
      location = as.factor(location)
    ) %>%
    filter(!is.na(rainfall)) %>%
    select(-rain_tomorrow)
}


# temporal interpolation

interpolate_weather <- function(df) {
  interp_vars <- c(
    "min_temp",
    "max_temp",
    "temp9am",
    "temp3pm",
    "pressure9am",
    "pressure3pm",
    "humidity9am",
    "humidity3pm"
  )

  df %>%
    group_by(location) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(across(
      all_of(interp_vars),
      ~ na.approx(., maxgap = MAXGAP, na.rm = FALSE, rule = 1)
    )) %>%
    ungroup()
}


# Ghost sensor flagging

flag_ghost_sensors <- function(df) {
  ghost_prone_vars <- c("sunshine", "evaporation", "cloud3pm", "cloud9am")

  ghost_pairs <- df %>%
    select(location, all_of(ghost_prone_vars)) %>%
    pivot_longer(
      cols = all_of(ghost_prone_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(location, variable) %>%
    summarise(miss_rate = mean(is.na(value)) * 100, .groups = "drop") %>%
    filter(miss_rate > (GHOST_THRESHOLD * 100))

  cat(sprintf("Found %d ghost sensor instances.\n", nrow(ghost_pairs)))

  df %>%
    mutate(
      sunshine_imp_flagged = as.integer(is.na(sunshine)),
      evap_imp_flagged = as.integer(is.na(evaporation)),
      cloud3pm_imp_flagged = as.integer(is.na(cloud3pm)),
      cloud9am_imp_flagged = as.integer(is.na(cloud9am))
    )
}


# MICE configuration

build_mice_config <- function(df) {
  ghost_prone_vars <- c("sunshine", "evaporation", "cloud3pm", "cloud9am")
  wind_vars <- c("wind_gust_dir", "wind_dir9am", "wind_dir3pm")

  init <- mice(df, maxit = 0)
  pred <- init$predictorMatrix
  meth <- init$method

  pred[,] <- 0

  meth[ghost_prone_vars] <- "pmm"
  meth[wind_vars] <- "rf"

  predictor_map <- list(
    sunshine = c(
      "cloud9am",
      "cloud3pm",
      "max_temp",
      "humidity3pm",
      "location",
      "month"
    ),
    evaporation = c(
      "wind_gust_speed",
      "max_temp",
      "humidity3pm",
      "sunshine",
      "location",
      "month"
    ),
    cloud9am = c(
      "humidity9am",
      "humidity3pm",
      "pressure9am",
      "location",
      "month"
    ),
    cloud3pm = c(
      "humidity9am",
      "humidity3pm",
      "pressure9am",
      "location",
      "month"
    ),
    wind_gust_dir = c("wind_gust_speed", "pressure3pm", "location", "month"),
    wind_dir9am = c("wind_speed9am", "pressure9am", "location", "month"),
    wind_dir3pm = c("wind_speed3pm", "pressure3pm", "location", "month")
  )

  for (target in names(predictor_map)) {
    if (target %in% rownames(pred)) {
      predictors <- intersect(colnames(df), predictor_map[[target]])
      pred[target, predictors] <- 1
    }
  }

  ignore_cols <- grep("_imp_flagged$|^date$", colnames(pred), value = TRUE)
  pred[, ignore_cols] <- 0

  list(method = meth, predictorMatrix = pred)
}


# Parallel MICE runner

impute_parallel <- function(df, mice_config, m = M) {
  n_cores <- max(1L, min(as.integer(detectCores()), as.integer(m)))
  m_split <- rep(m %/% n_cores, n_cores)
  remainder <- m %% n_cores
  if (remainder > 0L) {
    m_split[seq_len(remainder)] <- m_split[seq_len(remainder)] + 1L
  }
  seeds <- 123L + seq_len(n_cores) - 1L

  imp_list <- mclapply(
    seq_len(n_cores),
    function(i) {
      mice(
        df,
        method = mice_config$method,
        predictorMatrix = mice_config$predictorMatrix,
        m = m_split[i],
        maxit = 5,
        seed = seeds[i],
        printFlag = FALSE
      )
    },
    mc.cores = n_cores
  )

  Reduce(ibind, imp_list)
}


# entry point

#' Run the full two-stage pipeline.
#'
#' @param df Raw data frame (output of read_csv on weatherAUS.csv).
#' @return A mids object containing m = 10 completed datasets.
clean_and_impute_weather <- function(df) {
  df_flagged <- df %>%
    clean_weather() %>%
    interpolate_weather() %>%
    flag_ghost_sensors()

  mice_config <- build_mice_config(df_flagged)
  impute_parallel(df_flagged, mice_config)
}
