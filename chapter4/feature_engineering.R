# feature_engineering.R
# Full feature engineering pipeline + multicollinearity diagnostics:
# compass_lookup - named vector of compass bearings (16-point rose)
# engineer_features() - applies full pipeline to a single dataset;
#                         for_modeling = TRUE returns select_model_features(keep_location = TRUE)
#                         for_modeling = FALSE returns select_model_features(keep_location = FALSE)
# render_vif_table() - VIF kable; accepts raw or pre-engineered data,
#                         already_engineered = TRUE skips re-processing,
#                         for_model controls whether location is retained

source(here::here("config.R"))
source(here::here("utils.R"))

# %%
df_final <- read_csv(here::here("data", "df_final.csv"))
compass_lookup <- c(
  "N" = 0,
  "NNE" = 22.5,
  "NE" = 45,
  "ENE" = 67.5,
  "E" = 90,
  "ESE" = 112.5,
  "SE" = 135,
  "SSE" = 157.5,
  "S" = 180,
  "SSW" = 202.5,
  "SW" = 225,
  "WSW" = 247.5,
  "W" = 270,
  "WNW" = 292.5,
  "NW" = 315,
  "NNW" = 337.5
)

engineer_features <- function(dat = df_final, for_modeling = FALSE) {
  dat <- dat %>%
    group_by(location) %>%
    arrange(date) %>%
    mutate(
      # Lag the rolling window by one additional day to prevent same-day leakage
      rainfall_ma7 = lag(
        rollmean(rainfall, k = 7, fill = NA, align = "right"),
        n = 1
      ),
      humidity_ma7 = lag(
        rollmean(humidity3pm, k = 7, fill = NA, align = "right"),
        n = 1
      ),

      # Dry spell counter
      rain_event_id = cumsum(lag(rainfall, 1) > 0),
      days_since_rain = row_number() - match(rain_event_id, rain_event_id),

      # First-order Markov state
      rain_yesterday = lag(rain_today, n = 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(rain_yesterday), !is.na(rainfall_ma7)) %>%
    select(-rain_event_id) %>%
    mutate(
      # Cyclical annual encoding
      day_of_year = yday(date),
      day_sin = sin(2 * pi * day_of_year / 365),
      day_cos = cos(2 * pi * day_of_year / 365),

      # Mean-centred interaction term (the "Rain Corner")
      sunshine = as.numeric(scale(sunshine, center = TRUE, scale = FALSE)),
      humidity3pm = as.numeric(scale(
        humidity3pm,
        center = TRUE,
        scale = FALSE
      )),
      sun_humid_interaction = as.numeric(sunshine * humidity3pm),

      # Meteorological derived indices
      pressure_change = pressure3pm - pressure9am,
      dewpoint_9am = temp9am - ((100 - humidity9am) / 5),
      dewpoint_3pm = temp3pm - ((100 - humidity3pm) / 5),
      dewpoint_change = dewpoint_3pm - dewpoint_9am,
      moisture_index = humidity3pm * (1 - sunshine / 15),
      instability_index = (1020 - pressure3pm) * humidity3pm / 100,
      cloud_development = pmax(0, cloud3pm - cloud9am),

      # Circular wind vector decomposition
      gust_rad = compass_lookup[wind_gust_dir] * pi / 180,
      gust_V_NS = wind_gust_speed * cos(gust_rad),
      gust_U_EW = wind_gust_speed * sin(gust_rad),
      wind9am_rad = compass_lookup[wind_dir9am] * pi / 180,
      wind9am_V_NS = wind_speed9am * cos(wind9am_rad),
      wind9am_U_EW = wind_speed9am * sin(wind9am_rad)
    ) %>%
    relocate(rain_today, date, location) %>%
    relocate(rain_yesterday, days_since_rain, .after = location) %>%
    relocate(ends_with("_ma7"), .after = days_since_rain) %>%
    relocate(day_sin, day_cos, .after = date)

  if (for_modeling) {
    return(select_model_features(dat, keep_location = TRUE))
  }
  return(select_model_features(dat, keep_location = FALSE))
}


# %%
render_vif_table <- function(
  dat = df_final,
  already_engineered = FALSE,
  for_model = FALSE
) {
  df_prepped <- if (already_engineered) {
    select_model_features(dat, keep_location = for_model)
  } else {
    engineer_features(dat, for_modeling = for_model)
  }

  vif_results <- mc_check(df_prepped)
  vif_results %>%
    as_tibble() %>%
    arrange(desc(VIF)) %>%
    kable(
      caption = "Variance Inflation Factor (VIF) for Selected Predictors",
      digits = 3,
      booktabs = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      latex_options = c("hold_position")
    )
}

# df_wo_location <- engineer_features(df_final, for_modeling = FALSE)
# # df_wo_location <- select_model_features(df_final, keep_location = TRUE)
# vif_results <- mc_check(df_wo_location)

# render_vif_table(for_model = TRUE)

# %%

# engineered_list <- complete(imp_mids, action = "all") %>%
#   map(\(dat) engineer_features(dat, for_modeling = TRUE))

# %%
# render_vif_table(engineered_list[[1]], already_engineered = TRUE)
