# distribution_comparison.R
# Distributional family comparison:
# fit_comparison_models() - fits Tweedie, Log-Normal, Gaussian
# plot_distribution_comparison() - density facet plot across four families

fit_comparison_models <- function(eng_list = engineered_list) {
  single_data <- eng_list[[1]]

  base_cond <- rainfall ~ humidity3pm +
    dewpoint_9am +
    day_sin +
    rainfall_ma7 +
    humidity_ma7 +
    ns(dewpoint_change) +
    ns(pressure_change) +
    rain_yesterday +
    sunshine +
    ns(evaporation) +
    ns(instability_index, df = 3) +
    sun_humid_interaction +
    ns(gust_U_EW, df = 2) +
    ns(wind9am_U_EW, df = 2) +
    cloud_development +
    diag(1 + humidity3pm + rain_yesterday + dewpoint_change | location)

  zi_formula <- ~ humidity3pm +
    dewpoint_9am +
    rain_yesterday +
    cloud_development +
    pressure_change +
    sunshine +
    evaporation +
    diag(1 | location) +
    humidity_ma7 +
    day_cos +
    day_sin

  disp_formula <- ~ humidity3pm +
    rainfall_ma7 +
    day_sin +
    day_cos +
    pressure_change +
    dewpoint_change +
    ns(evaporation) +
    ns(gust_U_EW, df = 2)

  ctrl <- glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8),
    parallel = 4
  )

  m_tweedie <- glmmTMB(
    formula = base_cond,
    dispformula = disp_formula,
    data = single_data,
    control = ctrl,
    family = tweedie(link = "log")
  )

  m_lognormal <- glmmTMB(
    formula = base_cond,
    ziformula = zi_formula,
    dispformula = disp_formula,
    data = single_data,
    control = ctrl,
    family = lognormal(link = "log")
  )

  m_linear <- glmmTMB(
    formula = rainfall ~ humidity3pm +
      dewpoint_9am +
      dewpoint_change +
      pressure_change +
      day_cos +
      day_sin +
      rainfall_ma7 +
      days_since_rain +
      humidity_ma7 +
      rain_yesterday +
      sunshine +
      evaporation +
      instability_index +
      sun_humid_interaction +
      cloud_development +
      gust_U_EW +
      gust_V_NS +
      wind9am_V_NS +
      wind9am_U_EW +
      (1 | location),
    data = single_data,
    family = gaussian(link = "identity")
  )

  list(m_tweedie = m_tweedie, m_lognormal = m_lognormal, m_linear = m_linear)
}


plot_distribution_comparison <- function(
  check_data,
  eng_list = engineered_list
) {
  model_palette <- c(
    "Linear (Gaussian)" = "#E76F51",
    "Log-Normal" = "#2A9D8F",
    "Tweedie" = "#E9C46A",
    "ZI-Gamma (Final)" = "#2E86AB"
  )

  check_data %>%
    select(
      rainfall,
      pred_linear,
      pred_lognormal,
      pred_tweedie,
      pred_zig
    ) %>%
    pivot_longer(
      cols = -rainfall,
      names_to = "Model",
      values_to = "Prediction"
    ) %>%
    mutate(
      Model = recode(
        Model,
        pred_linear = "Linear (Gaussian)",
        pred_lognormal = "Log-Normal",
        pred_tweedie = "Tweedie",
        pred_zig = "ZI-Gamma (Final)"
      )
    ) %>%
    ggplot(aes(x = Prediction, fill = Model, colour = Model)) +
    geom_density(alpha = 0.25, linewidth = 0.8) +
    geom_density(
      aes(x = rainfall),
      data = eng_list[[1]],
      inherit.aes = FALSE,
      fill = NA,
      colour = "white",
      linetype = "dashed",
      linewidth = 0.9
    ) +
    facet_wrap(~Model, scales = "free") +
    coord_cartesian(xlim = c(-5, 20)) +
    scale_fill_manual(values = model_palette) +
    scale_colour_manual(values = model_palette) +
    labs(
      title = "Why Distribution Matters",
      subtitle = "White dashed line: observed data. Coloured fill: predicted density per model family.",
      x = "Predicted Rainfall (mm)",
      y = "Density"
    )
}
