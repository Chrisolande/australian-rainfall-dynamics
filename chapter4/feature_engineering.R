# feature_engineering.R
# Full feature engineering pipeline + multicollinearity diagnostics:
# compass_lookup - named vector of compass bearings (16-point rose)
# engineer_features() - applies full pipeline to a single dataset
# render_vif_table() - VIF kable; accepts raw or pre-engineered data,
#                         already_engineered = TRUE skips re-processing

# %%
# source(here::here("config.R"))
# df_final <- read_csv(here::here("data", "df_final.csv"))
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

# %%
engineer_features <- function(dat) {
  dat %>%
    group_by(location) %>%
    arrange(date) %>%
    mutate(
      rainfall_ma7 = lag(
        rollmean(rainfall, k = 7, fill = NA, align = "right"),
        1
      ),
      humidity_ma7 = lag(
        rollmean(humidity3pm, k = 7, fill = NA, align = "right"),
        1
      ),
      rain_event_id = cumsum(lag(rainfall, 1) > 0),
      days_since_rain = row_number() - match(rain_event_id, rain_event_id),
      rain_yesterday = lag(rain_today, 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(rain_yesterday), !is.na(rainfall_ma7)) %>%
    select(-rain_event_id) %>%
    mutate(
      day_of_year = yday(date),
      sunshine = as.numeric(scale(sunshine, center = TRUE, scale = FALSE)),
      humidity3pm = as.numeric(scale(
        humidity3pm,
        center = TRUE,
        scale = FALSE
      )),
      pressure_change = pressure3pm - pressure9am,
      sun_humid_interaction = as.numeric(sunshine * humidity3pm),
      dewpoint_9am = temp9am - ((100 - humidity9am) / 5),
      dewpoint_3pm = temp3pm - ((100 - humidity3pm) / 5),
      dewpoint_change = dewpoint_3pm - dewpoint_9am,
      moisture_index = humidity3pm * (1 - sunshine / 15),
      instability_index = (1020 - pressure3pm) * humidity3pm / 100,
      cloud_development = pmax(0, cloud3pm - cloud9am),
      day_sin = sin(2 * pi * day_of_year / 365),
      day_cos = cos(2 * pi * day_of_year / 365),
      gust_rad = compass_lookup[wind_gust_dir] * pi / 180,
      gust_V_NS = wind_gust_speed * cos(gust_rad),
      gust_U_EW = wind_gust_speed * sin(gust_rad),
      wind9am_rad = compass_lookup[wind_dir9am] * pi / 180,
      wind9am_V_NS = wind_speed9am * cos(wind9am_rad),
      wind9am_U_EW = wind_speed9am * sin(wind9am_rad)
    ) %>%
    select_model_features(keep_location = TRUE) %>%
    scale_data()
}

# %%
render_vif_table <- function(dat, keep_location = FALSE) {
  df_prepped <- select_model_features(dat, keep_location = keep_location)

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

# # %%
# source(here::here("config.R"))
# engineered_list <- complete(imp_mids, action = "all") %>%
#   map(engineer_features)

# # %%
# render_vif_table(engineered_list[[1]])

# %%
